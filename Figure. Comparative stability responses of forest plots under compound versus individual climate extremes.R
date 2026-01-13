# ============================================================
# Goal
# ------------------------------------------------------------
# Create the figure "Comparative stability responses of forest plots
# under compound versus individual climate extremes".
#
# Panel (a): Resistance (Rs)
# Panel (b): Recovery  (Le, equivalent to Rc in the figure)
#
# Key design:
# Pairwise comparisons are conducted within the same forest plots by
# matching compound and individual summaries using (plot_ID, ECOSUBCD).
# Statistical significance is evaluated using a paired Wilcoxon
# signed-rank test on the log2 scale, and p-values are annotated in the
# upper-left corner of each panel.
# ============================================================

library(dplyr)
library(ggplot2)
library(grid)

# ============================================================
# 1) Helper function: summarize data to plot-level
# ------------------------------------------------------------
# For a given extreme type (ext_type), multiple observations (e.g., years
# or events) within the same forest plot are aggregated to a single
# plot-level mean value. This ensures that each plot contributes only
# one paired observation in subsequent analyses.
#
# Arguments:
#   df       : input data frame (compound or individual extremes)
#   ext_type : character string specifying the extreme type
#              (e.g., "pup_tup", "pup", "tup")
#   suffix   : "com" for compound extremes, "sin" for individual extremes
#
# Returns:
#   A data frame with one row per (plot_ID, ECOSUBCD) containing
#   Rs_com / Le_com or Rs_sin / Le_sin.
# ============================================================
summarise_plot <- function(df, ext_type, suffix = c("com", "sin")) {
  suffix <- match.arg(suffix)
  
  df %>%
    # Keep only records corresponding to the target extreme type
    filter(ext_type == !!ext_type) %>%
    # Aggregate to plot-level using plot_ID and ECOSUBCD
    group_by(plot_ID, ECOSUBCD) %>%
    summarise(
      # Plot-level mean resistance and recovery (NA values removed)
      Rs = mean(.data[[paste0("Rs_", suffix)]], na.rm = TRUE),
      Le = mean(.data[[paste0("Le_", suffix)]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Rename columns to indicate compound or individual extremes
    rename_with(~ paste0(., "_", suffix), c("Rs", "Le"))
}

# ============================================================
# 2) Helper function: pairwise merge by forest plot
# ------------------------------------------------------------
# Retains only forest plots that have both compound and individual
# extreme summaries, ensuring strict within-plot pairing.
# ============================================================
pair_merge <- function(com_df, sin_df) {
  com_df %>% inner_join(sin_df, by = c("plot_ID", "ECOSUBCD"))
}

# ============================================================
# 3) Helper function: paired Wilcoxon signed-rank test (log2 scale)
# ------------------------------------------------------------
# Performs a paired Wilcoxon signed-rank test on log2-transformed values.
# The alternative hypothesis "less" tests whether stability under
# compound extremes is significantly lower than under individual
# extremes.
#
# Arguments:
#   dat : paired data frame
#   var : variable name, "Rs" (Resistance) or "Le" (Recovery)
#
# Returns:
#   p-value from the Wilcoxon test
# ============================================================
wilcox_p <- function(dat, var) {
  x <- dat[[paste0(var, "_com")]]  # stability under compound extremes
  y <- dat[[paste0(var, "_sin")]]  # stability under individual extremes
  
  wilcox.test(log2(x), log2(y),
              paired = TRUE,
              alternative = "less")$p.value
}

# ============================================================
# 4) Helper function: prepare data for plotting
# ------------------------------------------------------------
# Converts paired stability metrics to log2 scale and classifies the
# direction of difference relative to the 1:1 line (y = x).
#
# Direction coding:
#   "small_com" : compound < individual (points above y = x)
#   "large_com" : compound > individual (points below y = x)
#   "equal"     : compound = individual
#
# Returns:
#   A data frame with log2-transformed x (compound) and y (individual).
# ============================================================
prep_metric <- function(dat, var) {
  x <- dat[[paste0(var, "_com")]]
  y <- dat[[paste0(var, "_sin")]]
  
  dat %>%
    transmute(
      plot_ID,
      x = log2(x),  # x-axis: compound extremes
      y = log2(y),  # y-axis: individual extremes
      dir = case_when(
        x > y ~ "large_com",
        x < y ~ "small_com",
        TRUE  ~ "equal"
      )
    )
}

# ============================================================
# 5) Helper function: format mean ± SD label
# ------------------------------------------------------------
# Generates formatted text used to annotate the mean ± standard deviation
# in the center of the upper (individual) and lower (compound) triangles.
# ============================================================
mean_sd_label <- function(v) {
  paste0(
    format(round(mean(v, na.rm = TRUE), 3), nsmall = 3),
    " ± ",
    format(round(sd(v,   na.rm = TRUE), 3), nsmall = 3)
  )
}

# ============================================================
# 6) Helper function: draw 2D binned comparison plot
# ------------------------------------------------------------
# Creates a 2D binned (geom_bin2d) plot comparing compound and individual
# extremes with:
#   - dashed 1:1 reference line (y = x)
#   - shaded upper and lower triangular regions
#   - p-value annotation (upper-left corner)
#   - mean ± SD annotations (upper-center: individual; lower-center: compound)
#
# Arguments:
#   df_xy     : output from prep_metric()
#   bins      : number of bins for geom_bin2d
#   xlab_expr : expression for x-axis label
#   ylab_expr : expression for y-axis label
#   p_value   : p-value to annotate
# ============================================================
plot_bin2d <- function(df_xy,
                       bins,
                       xlab_expr,
                       ylab_expr,
                       p_value) {
  
  # Fixed axis limits to match the original figure design
  lim <- c(-3.5, 21.5)
  
  # Coordinates defining the upper and lower triangular regions
  trsup <- data.frame(
    x = c(lim[1], lim[1], lim[2]),
    y = c(lim[1], lim[2], lim[2])
  )  # region where individual > compound
  
  trinf <- data.frame(
    x = c(lim[1], lim[2], lim[2]),
    y = c(lim[1], lim[1], lim[2])
  )  # region where compound > individual
  
  # Mean ± SD labels
  top_lab    <- mean_sd_label(df_xy$y)  # individual extremes
  bottom_lab <- mean_sd_label(df_xy$x)  # compound extremes
  
  # Format p-value annotation
  p_lab <- ifelse(
    p_value < 0.001,
    "italic(p)<0.001",
    paste0("italic(p)==", format(round(p_value, 3), nsmall = 3))
  )
  
  ggplot(df_xy, aes(x = x, y = y)) +

    # 2D binned count of forest plots
    geom_bin2d(bins = bins, alpha = 0.98) +
    scale_fill_gradientn(
      colors = c("#3235A4", "#21B1A6", "#FEF70F"),
      name   = "Number of\nplots (n)",
      breaks = c(1, 25, 50),
      guide  = guide_colorbar(draw.ulim = FALSE,
                              draw.llim = FALSE)
    ) +
    
    # 1:1 reference line
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed",
                size = 0.4,
                alpha = 0.8) +
    
    # Axis settings
    scale_x_continuous(name = xlab_expr,
                       breaks = seq(-3, 21, 6),
                       limits = lim,
                       expand = c(0, 0)) +
    scale_y_continuous(name = ylab_expr,
                       breaks = seq(-3, 21, 6),
                       limits = lim,
                       expand = c(0, 0)) +
    
    # Annotations: p-value and mean ± SD
    annotate("text", x = -2.5, y = 20.5,
             parse = TRUE, label = p_lab,
             hjust = 0, vjust = 1, size = 4.5) +
    annotate("text", x = 9, y = 20.25,
             label = top_lab,
             vjust = 1, size = 4.5) +
    annotate("text", x = 9, y = -2.25,
             label = bottom_lab,
             vjust = 0, size = 4.5) +
    theme_bw()
}

# ============================================================
# 7) Define compound–individual extreme pairs to be compared
# ------------------------------------------------------------
# Each row defines one paired comparison between a compound extreme
# (from data_com) and an individual extreme (from data_sin).
# ============================================================
pairs <- tibble::tribble(
  ~com_ext,    ~sin_ext,
  "pup_tup",   "pup",    # wet–hot compound vs wet individual
  "pup_tup",   "tup",    # wet–hot compound vs hot individual
  "pdown_tup", "pdown",  # dry–hot compound vs dry individual
  "pdown_tup", "tup"     # dry–hot compound vs hot individual
)

# ============================================================
# 8) Build the full paired dataset across all comparisons
# ------------------------------------------------------------
# For each defined pair:
#   1) summarize compound extremes to plot-level
#   2) summarize individual extremes to plot-level
#   3) merge them by plot_ID and ECOSUBCD
# All paired datasets are then combined into a single data frame.
# ============================================================
pair_list <- lapply(seq_len(nrow(pairs)), function(i) {
  com <- summarise_plot(data_com, pairs$com_ext[i], suffix = "com")
  sin <- summarise_plot(data_sin, pairs$sin_ext[i], suffix = "sin")
  pair_merge(com, sin)
})
dat_all <- bind_rows(pair_list)

# ============================================================
# 9) Panel (a): Resistance (Rs)
# ------------------------------------------------------------
# Compute the paired Wilcoxon p-value and generate the Resistance panel.
# ============================================================
p_rs  <- wilcox_p(dat_all, "Rs")
df_rs <- prep_metric(dat_all, "Rs")

p1_1 <- plot_bin2d(
  df_xy = df_rs,
  bins  = 195,
  xlab_expr = expression(atop(Resistance~(ln(italic(R)[s])),
                              paste(under~compound~extremes))),
  ylab_expr = expression(atop(Resistance~(ln(italic(R)[s])),
                              paste(under~individual~extremes))),
  p_value = p_rs
)
p1_1

# ============================================================
# 10) Panel (b): Recovery (Le)
# ------------------------------------------------------------
# Compute the paired Wilcoxon p-value and generate the Recovery panel.
# ============================================================
p_le  <- wilcox_p(dat_all, "Le")
df_le <- prep_metric(dat_all, "Le")

p2_1 <- plot_bin2d(
  df_xy = df_le,
  bins  = 175,
  xlab_expr = expression(atop(Recovery~(ln(italic(R)[c])),
                              paste(under~compound~extremes))),
  ylab_expr = expression(atop(Recovery~(ln(italic(R)[c])),
                              paste(under~individual~extremes))),
  p_value = p_le
)
p2_1
