# ── Helper: compute per-node % of total error dollars within each tree level ──
#
# For every node in `frame` (rpart$frame), this function:
#   1. Derives each node's total error dollars  = yval * n
#      (yval is the mean response; n is the node count — all cases, no filter)
#   2. Determines each node's depth in the tree
#   3. Within each depth level, divides each node's total error by the sum of
#      total error across all sibling nodes at that depth → proportion 0–1
#
# Returns a numeric vector (length = nrow(frame)) of within-level proportions.
.node_level_pct <- function(frame) {
  n_nodes    <- nrow(frame)
  node_ids   <- as.integer(rownames(frame))   # rpart node IDs (1 = root, 2/3 = children, …)
  node_depth <- floor(log2(node_ids))         # depth: root=0, its children=1, …
  
  node_total_err <- frame$yval * frame$n      # mean × count = total error dollars per node
  
  level_pct <- numeric(n_nodes)
  for (d in unique(node_depth)) {
    idx          <- which(node_depth == d)
    level_sum    <- sum(node_total_err[idx], na.rm = TRUE)
    level_pct[idx] <- if (level_sum == 0) rep(0.5, length(idx))
    else node_total_err[idx] / level_sum
  }
  pmin(pmax(level_pct, 0), 1)
}


# ── Helper: build a color vector from within-level proportions ────────────────
#
# Maps each node's within-level share accurately to the palette.
# The palette runs from green (0%) to red (ceiling_pct), where ceiling_pct
# is the largest share observed at any non-root node. This keeps colors
# proportional and faithful to the actual data without being compressed
# into the green end of a 0-100% scale (since no non-root node ever
# approaches 100%).
#
# Returns a list with:
#   $colors      - character vector of hex colors, one per node
#   $ceiling_pct - the value that maps to red (for use in the legend)
#
# Low share  -> green  (#1a9641)
# Mid share  -> yellow (#ffffbf)
# High share -> red    (#d7191c)
.pct_to_colors <- function(level_pct, node_depth) {
  pal         <- colorRampPalette(c("#1a9641", "#ffffbf", "#d7191c"))(101)
  ceiling_pct <- max(level_pct[node_depth > 0], na.rm = TRUE)
  scaled      <- pmin(level_pct / ceiling_pct, 1)
  list(
    colors      = pal[round(scaled * 100) + 1],
    ceiling_pct = ceiling_pct
  )
}


# ── Helper: draw the color-scale legend ──────────────────────────────────────
.draw_legend <- function(legend_x      = 0.03,
                         legend_y      = 0.975,
                         legend_width  = 0.015,
                         legend_height = 0.20,
                         ceiling_pct   = 1,
                         cex           = 1.1) {
  pal <- colorRampPalette(c("#d7191c", "#ffffbf", "#1a9641"))(100)
  for (i in seq_len(100)) {
    rect(legend_x,
         legend_y - legend_height * (i / 100),
         legend_x + legend_width,
         legend_y - legend_height * ((i - 1) / 100),
         col    = pal[i],
         border = NA)
  }
  rect(legend_x, legend_y - legend_height,
       legend_x + legend_width, legend_y,
       border = "black")
  
  text(legend_x + legend_width + 0.002, legend_y,
       sprintf("%.0f%% of level error $", ceiling_pct * 100), pos = 4, cex = cex)
  text(legend_x + legend_width + 0.002, legend_y - legend_height,
       "0%", pos = 4, cex = cex)
  text(legend_x + legend_width + 0.001, legend_y + 0.04,
       "% of error $ at this tree level", cex = cex, font = 2)
}


# ══════════════════════════════════════════════════════════════════════════════
# plot_state_tree
#
# Plots a regression tree for a single state.
#
# Node label : avg error $ (all cases) on top, n below
# Node color : % of total error dollars among sibling nodes at the same depth
#              green (low %) → yellow → red (high %)
# ══════════════════════════════════════════════════════════════════════════════
plot_state_tree <- function(tree_models,
                            state_name,
                            main_title    = NULL,
                            save_path     = NULL,
                            width_inches  = 30,
                            height_inches = 12,
                            dpi           = 300) {
  
  if (!state_name %in% names(tree_models)) {
    stop(paste("State", state_name, "not found in tree models"))
  }
  
  tree_model <- tree_models[[state_name]]
  
  if (is.null(main_title)) {
    main_title <- paste("Regression Tree for Error Amount at Certification in",
                        state_name, "2017-2022")
  }
  
  # ── Compute node colors (% of level's total error $) ──────────────────────
  frame           <- tree_model$frame
  node_ids        <- as.integer(rownames(frame))
  node_depth      <- floor(log2(node_ids))
  level_pct       <- .node_level_pct(frame)
  color_result    <- .pct_to_colors(level_pct, node_depth)
  node_colors     <- color_result$colors
  ceiling_pct     <- color_result$ceiling_pct
  
  # ── Custom node label: avg error $ + n ────────────────────────────────────
  avg_vec <- frame$yval   # mean predicted error amount (all cases)
  n_vec   <- frame$n
  
  node_label_fn <- function(x, labs, digits, varlen) {
    sprintf("$%s\nn = %s",
            formatC(round(avg_vec), format = "d", big.mark = ","),
            formatC(n_vec,          format = "d", big.mark = ","))
  }
  
  # ── Inner draw function ────────────────────────────────────────────────────
  draw_plot <- function() {
    rpart.plot(tree_model,
               main          = main_title,
               type          = 4,
               extra         = 0,           # labels handled by node.fun
               fallen.leaves = FALSE,
               branch.lty    = 1,
               shadow.col    = "gray",
               box.col       = node_colors,
               box.palette   = NULL,        # disable built-in palette
               node.fun      = node_label_fn,
               cex           = 1.2,
               tweak         = 1.3)
    
    .draw_legend(ceiling_pct = ceiling_pct, cex = 1.1)
    
    text(x = 0.90, y = 0.98,
         paste0("How to read this: each node shows the avg error amount\n",
                "(in dollars, all cases) on top and n below.\n",
                "Color reflects each node's share of total error dollars\n",
                "among nodes at the same tree depth (green = low %, red = high %)."),
         cex = 1.1, adj = c(1, 1))
  }
  
  # ── Output ─────────────────────────────────────────────────────────────────
  if (!is.null(save_path)) {
    png(save_path,
        width  = width_inches * dpi,
        height = height_inches * dpi,
        res    = dpi)
    draw_plot()
    dev.off()
    cat("Tree plot saved to:", save_path, "\n")
  } else {
    draw_plot()
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# plot_pooled_tree
#
# Plots a single pooled regression tree (model object passed directly).
#
# Node label : avg error $ (all cases) on top, n below
# Node color : % of total error dollars among sibling nodes at the same depth
#              green (low %) → yellow → red (high %)
#
# subset_data / predictor_vars kept as parameters for API compatibility but
# are no longer used for coloring (color is now level-% based, all cases).
# ══════════════════════════════════════════════════════════════════════════════
plot_pooled_tree <- function(tree_model,
                             main_title     = "Regression Tree — Pooled All States",
                             predictor_vars = NULL,   # retained for compatibility
                             subset_data    = NULL,   # retained for compatibility
                             save_path      = NULL,
                             pdf_path       = NULL,
                             width_inches   = 30,
                             height_inches  = 16,
                             dpi            = 300) {
  
  # ── Compute node colors (% of level's total error $) ──────────────────────
  frame           <- tree_model$frame
  node_ids        <- as.integer(rownames(frame))
  node_depth      <- floor(log2(node_ids))
  level_pct       <- .node_level_pct(frame)
  color_result    <- .pct_to_colors(level_pct, node_depth)
  node_colors     <- color_result$colors
  ceiling_pct     <- color_result$ceiling_pct
  
  # ── Custom node label: avg error $ + n ────────────────────────────────────
  avg_vec <- frame$yval   # mean predicted error amount (all cases)
  n_vec   <- frame$n
  
  node_label_fn <- function(x, labs, digits, varlen) {
    sprintf("$%s\nn = %s",
            formatC(round(avg_vec), format = "d", big.mark = ","),
            formatC(n_vec,          format = "d", big.mark = ","))
  }
  
  # ── Inner draw function ────────────────────────────────────────────────────
  draw_plot <- function() {
    rpart.plot(tree_model,
               main          = main_title,
               type          = 4,
               extra         = 0,
               fallen.leaves = FALSE,
               branch.lty    = 1,
               shadow.col    = "gray",
               box.col       = node_colors,
               box.palette   = NULL,
               node.fun      = node_label_fn,
               cex           = 1,
               tweak         = 1.2)
    
    .draw_legend(legend_x = 0.04, ceiling_pct = ceiling_pct, cex = 1.1)
    
    text(x = 0.98, y = 0.98,
         paste0("How to read this: each node shows the avg error dollar\n",
                "amount (all cases) and total n.\n",
                "Color reflects each node's share of total error dollars\n",
                "among nodes at the same tree depth (green = low %, red = high %).\n",
                "Only public data excluding ineligible cases is used."),
         cex = 1.1, adj = c(1, 1))
  }
  
  # ── PNG output ─────────────────────────────────────────────────────────────
  if (!is.null(save_path)) {
    png(save_path,
        width  = width_inches * dpi,
        height = height_inches * dpi,
        res    = dpi)
    draw_plot()
    dev.off()
    cat("PNG saved to:", save_path, "\n")
  }
  
  # ── PDF output ─────────────────────────────────────────────────────────────
  if (!is.null(pdf_path)) {
    pdf(pdf_path, width = width_inches, height = height_inches)
    draw_plot()
    dev.off()
    cat("PDF saved to:", pdf_path, "\n")
  }
  
  # ── Screen output ──────────────────────────────────────────────────────────
  if (is.null(save_path) && is.null(pdf_path)) {
    draw_plot()
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# plot_all_trees  (unchanged — delegates to plot_state_tree)
# ══════════════════════════════════════════════════════════════════════════════
plot_all_trees <- function(tree_models,
                           output_dir    = "tree_plots",
                           width_inches  = 30,
                           height_inches = 12,
                           dpi           = 300) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (state_name in names(tree_models)) {
    file_path <- file.path(output_dir,
                           paste0(state_name,
                                  "_2017_2023_error_amount_regression_tree.png"))
    plot_state_tree(tree_models, state_name,
                    save_path     = file_path,
                    width_inches  = width_inches,
                    height_inches = height_inches,
                    dpi           = dpi)
  }
  
  cat("All tree plots saved to:", output_dir, "\n")
}