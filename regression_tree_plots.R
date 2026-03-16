plot_state_tree <- function(tree_models, state_name,
                            main_title = NULL,
                            save_path = NULL,
                            width_inches = 30,
                            height_inches = 12,
                            dpi = 300) {
  
  if (!state_name %in% names(tree_models)) {
    stop(paste("State", state_name, "not found in tree models"))
  }
  
  tree_model <- tree_models[[state_name]]
  
  if (is.null(main_title)) {
    main_title <- paste("Regression Tree for Error Amount at Certification in", state_name, "2017-2022")
  }
  
  if (!is.null(save_path)) {
    width_px  <- width_inches * dpi
    height_px <- height_inches * dpi
    png(save_path, width = width_px, height = height_px, res = dpi)
  }
  
  # For regression trees:
  #   extra = 1  → show number of observations in each node
  #   extra = 101 → show n and mean response (most useful here)
  # type  = 4  → label all nodes with split variable and value
  # box.palette: blue gradient suits a continuous dollar amount;
  #   low values (near $0) are light, high values are dark blue.
  rpart.plot(tree_model,
             main          = main_title,
             type          = 4,
             extra         = 101,        # n + mean predicted error amount
             fallen.leaves = FALSE,
             branch.lty    = 1,
             shadow.col    = "gray",
             box.palette   = "Blues",    # continuous palette for dollar amounts
             cex           = 1.2,
             tweak         = 1.3)
  
  # ── Colour-scale legend (maps to mean predicted error amount) ──────────────
  blues <- colorRampPalette(c("#08306b", "#deebf7"))(100)  # dark at top (i=1), light at bottom (i=100)
  
  legend_x      <- 0.03
  legend_y      <- 0.975
  legend_width  <- 0.015
  legend_height <- 0.20
  
  for (i in 1:100) {
    rect(legend_x,
         legend_y - legend_height * (i / 100),
         legend_x + legend_width,
         legend_y - legend_height * ((i - 1) / 100),
         col    = blues[i],
         border = NA)
  }
  rect(legend_x, legend_y - legend_height,
       legend_x + legend_width, legend_y,
       border = "black")
  
  text(legend_x + legend_width + 0.002, legend_y,
       "High error $", pos = 4, cex = 1.1)
  text(legend_x + legend_width + 0.002, legend_y - legend_height,
       "Low / $0", pos = 4, cex = 1.1)
  text(legend_x + legend_width + 0.001, legend_y + 0.04,
       "Mean error amount", cex = 1.1, font = 2)
  
  # ── How-to-read annotation ────────────────────────────────────────────────
  text(x = 0.90, y = 0.98,
       paste0('How to read this: each node shows the mean error\n',
              'amount (in dollars) on top and the number of cases (n) below.\n',
              'Splits route cases toward higher or lower error amounts.\n',
              'Only public data excluding ineligible cases is used.'),
       cex = 1.1, adj = c(1, 1))
  
  if (!is.null(save_path)) {
    dev.off()
    cat("Tree plot saved to:", save_path, "\n")
  }
}


# Plot a single pooled tree (takes a model object directly, not a named list)
# Nodes show avg error $ among over_threshold==1 cases + total n.
# Color: green (low avg $) to red (high avg $).
# Saves PNG (save_path) and/or PDF (pdf_path) — both optional.
plot_pooled_tree <- function(tree_model,
                             main_title     = "Regression Tree — Pooled All States",
                             predictor_vars = NULL,
                             subset_data    = NULL,   # full state data (must include over_threshold)
                             save_path      = NULL,   # PNG output path
                             pdf_path       = NULL,   # PDF output path
                             width_inches   = 30,
                             height_inches  = 16,
                             dpi            = 300) {

  # ── Step 1: compute per-node avg error $ among over_threshold==1 cases ──
  frame   <- tree_model$frame
  n_nodes <- nrow(frame)

  # Default: fall back to rpart's yval if subset_data not supplied
  node_avg_thresh <- frame$yval

  if (!is.null(subset_data) && "over_threshold" %in% names(subset_data)) {
    # tree_model$where gives the row index in frame for each training observation
    leaf_row_idx <- tree_model$where

    node_avg_thresh <- vapply(seq_len(n_nodes), function(i) {
      in_node <- (leaf_row_idx == i)
      sub     <- subset_data[in_node &
                               !is.na(subset_data$over_threshold) &
                               subset_data$over_threshold == 1, ]
      if (nrow(sub) == 0) return(0)
      mean(sub$total_error_amount, na.rm = TRUE)
    }, numeric(1))
  }

  # ── Step 2: green→red palette mapped to avg dollar amount ───────────────
  min_avg <- min(node_avg_thresh, na.rm = TRUE)
  max_avg <- max(node_avg_thresh, na.rm = TRUE)
  rng     <- max_avg - min_avg
  scaled  <- if (rng == 0) rep(0.5, n_nodes) else (node_avg_thresh - min_avg) / rng
  scaled  <- pmin(pmax(scaled, 0), 1)
  pal     <- colorRampPalette(c("#1a9641", "#ffffbf", "#d7191c"))(101)
  node_colors <- pal[round(scaled * 100) + 1]

  # ── Step 3: custom node-label: "$X,XXX\nn = X,XXX" ──────────────────────
  avg_vec <- node_avg_thresh
  n_vec   <- frame$n

  node_label_fn <- function(x, labs, digits, varlen) {
    sprintf("$%s\nn = %s",
            formatC(round(avg_vec), format = "d", big.mark = ","),
            formatC(n_vec,          format = "d", big.mark = ","))
  }

  # ── Step 4: inner draw function (shared by PNG and PDF) ─────────────────
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

    legend_x      <- 0.04
    legend_y      <- 0.975
    legend_width  <- 0.015
    legend_height <- 0.20
    legend_pal    <- colorRampPalette(c("#1a9641", "#ffffbf", "#d7191c"))(100)

    for (i in 1:100) {
      rect(legend_x,
           legend_y - legend_height * (i / 100),
           legend_x + legend_width,
           legend_y - legend_height * ((i - 1) / 100),
           col    = legend_pal[i],
           border = NA)
    }
    rect(legend_x, legend_y - legend_height,
         legend_x + legend_width, legend_y,
         border = "black")

    text(legend_x + legend_width + 0.002, legend_y,
         "High avg error $", pos = 4, cex = 1.1)
    text(legend_x + legend_width + 0.002, legend_y - legend_height,
         "Low avg error $", pos = 4, cex = 1.1)
    text(legend_x + 0.001, legend_y + 0.04,
         "Avg error $ (over_threshold = 1)", cex = 1.1, font = 2)

    text(x = 0.98, y = 0.98,
         paste0('How to read this: each node shows the avg error dollar\n',
                'amount (over_threshold = 1 cases only) and total n.\n',
                'Green = low avg error $; Red = high avg error $.\n',
                'Splits route cases toward higher or lower error amounts.\n',
                'Only public data excluding ineligible cases is used.'),
         cex = 1.1, adj = c(1, 1))
  }

  # ── Step 5: PNG output ───────────────────────────────────────────────────
  if (!is.null(save_path)) {
    png(save_path, width = width_inches * dpi, height = height_inches * dpi, res = dpi)
    draw_plot()
    dev.off()
    cat("PNG saved to:", save_path, "\n")
  }

  # ── Step 6: PDF output ───────────────────────────────────────────────────
  if (!is.null(pdf_path)) {
    pdf(pdf_path, width = width_inches, height = height_inches)
    draw_plot()
    dev.off()
    cat("PDF saved to:", pdf_path, "\n")
  }

  # ── Step 7: draw to screen if no output paths given ──────────────────────
  if (is.null(save_path) && is.null(pdf_path)) {
    draw_plot()
  }
}


plot_all_trees <- function(tree_models, output_dir = "tree_plots",
                           width_inches = 30,
                           height_inches = 12,
                           dpi = 300) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (state_name in names(tree_models)) {
    file_path <- file.path(output_dir,
                           paste0(state_name, "_2017_2023_error_amount_regression_tree.png"))
    plot_state_tree(tree_models, state_name,
                    save_path     = file_path,
                    width_inches  = width_inches,
                    height_inches = height_inches,
                    dpi           = dpi)
  }
  
  cat("All tree plots saved to:", output_dir, "\n")
}
