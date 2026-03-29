# ── Helper: map avg error $ per node to a green→yellow→red color ─────────────
#
# Color is based on each node's mean predicted error amount (yval), using a
# square root transform before scaling. This spreads out the low end of the
# distribution so that mid-range values (e.g. ~$125) appear orange rather
# than light green, which would happen with a linear scale when a few large
# outliers pull the max high.
#
# Scaling is from $0 (green) to the observed max (red).
#
# Returns a list with:
#   $colors  - character vector of hex colors, one per node
#   $max_avg - dollar value that maps to red (for legend)
#
# Low avg $  -> green  (#1a9641)
# Mid avg $  -> yellow (#ffffbf)
# High avg $ -> red    (#d7191c)
.avg_to_colors <- function(frame) {
  pal     <- colorRampPalette(c("#1a9641", "#ffffbf", "#d7191c"))(101)
  avg     <- pmax(frame$yval, 0)   # ensure non-negative before sqrt
  max_avg <- max(avg, na.rm = TRUE)
  scaled  <- if (max_avg == 0) rep(0, length(avg)) else sqrt(avg) / sqrt(max_avg)
  scaled  <- pmin(pmax(scaled, 0), 1)
  list(
    colors  = pal[round(scaled * 100) + 1],
    max_avg = max_avg
  )
}


# ── Helper: draw the color-scale legend ──────────────────────────────────────
.draw_legend_simplified <- function(legend_x      = 0.03,
                                    legend_y      = 0.975,
                                    legend_width  = 0.015,
                                    legend_height = 0.20,
                                    cex           = 1.1) {
  # Legend bar must reflect the sqrt-transformed scale visually:
  # position i/100 along the bar corresponds to scaled = i/100,
  # which represents a dollar value of (i/100)^2 * max_avg.
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
       "High", pos = 4, cex = cex)
  text(legend_x + legend_width + 0.002, legend_y - legend_height,
       "$0", pos = 4, cex = cex)
  text(legend_x + legend_width + 0.001, legend_y + 0.04,
       "Avg error $ per node", cex = cex, font = 2)
}


# ══════════════════════════════════════════════════════════════════════════════
# plot_state_tree
#
# Plots a regression tree for a single state.
#
# Node label : avg error $ (all cases) on top, n below
# Node color : avg error $ in that node — green (low $) → yellow → red (high $)
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
    main_title <- paste("Regression Tree for Income Errors in",
                        state_name, "2017-2023")
  }
  
  # ── Compute node colors (avg error $) ─────────────────────────────────────
  frame       <- tree_model$frame
  node_colors <- .avg_to_colors(frame)$colors
  
  # ── Custom node label: avg error $ + n ────────────────────────────────────
  avg_vec <- frame$yval
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
               cex           = 1.2,
               tweak         = 1.3)
    
    .draw_legend_simplified(cex = 1.1)
    
    text(x = 0.90, y = 0.98,
         paste0("How to read this: each node shows the avg error amount\n",
                "(in dollars, all cases) on top and n below.\n",
                "Color reflects the avg error dollar amount in that node\n",
                "Child-parent nodes combining >=1 and <1 due to rounding\n",
                "(green = low $, red = high $)."),
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
# Node color : avg error $ in that node — green (low $) → yellow → red (high $)
#
# subset_data / predictor_vars retained for API compatibility but unused.
# ══════════════════════════════════════════════════════════════════════════════
plot_pooled_tree <- function(tree_model,
                             main_title     = "Regression Tree — Income Errors Pooled Across States",
                             predictor_vars = NULL,
                             subset_data    = NULL,
                             save_path      = NULL,
                             pdf_path       = NULL,
                             width_inches   = 30,
                             height_inches  = 16,
                             dpi            = 300) {
  
  # ── Compute node colors (avg error $) ─────────────────────────────────────
  frame       <- tree_model$frame
  node_colors <- .avg_to_colors(frame)$colors
  
  # ── Custom node label: avg error $ + n ────────────────────────────────────
  avg_vec <- frame$yval
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
    
    .draw_legend_simplified(legend_x = 0.04, cex = 1.1)
    
    text(x = 0.98, y = 0.98,
         paste0("How to read this: each node shows the avg error dollar\n",
                "amount (all cases) and total n.\n",
                "Color reflects the avg error dollar amount in that node\n",
                "(green = low $, red = high $).\n",
                "Child-parent nodes combining >=1 and <1 due to rounding\n",
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
# plot_all_trees  (delegates to plot_state_tree)
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