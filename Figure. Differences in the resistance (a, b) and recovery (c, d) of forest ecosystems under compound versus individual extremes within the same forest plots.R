# ============================================================
# Figure 3 (clean / non-redundant version)
# ------------------------------------------------------------
# Panels:
# (a) Rs: compound heat + moisture excess vs (moisture excess alone, heat alone)
# (b) Rs: compound heat + drought         vs (drought alone, heat alone)
# (c) Le: compound heat + moisture excess vs (moisture excess alone, heat alone)
# (d) Le: compound heat + drought         vs (drought alone, heat alone)
#
# What this script does (per panel):
# 1) Summarize each extreme type to plot-level means (plot_ID + ECOSUBCD)
# 2) Pair the same plots across compound and individual extremes (inner join)
# 3) Convert to long format (3 distributions: compound + 2 individuals)
# 4) Draw: histogram (counts) + scaled kernel density + mean dashed lines + mean labels
# 5) Run paired Wilcoxon tests: compound vs each individual (alternative = "less")
#
# Notes:
# - This version removes repeated code blocks, repeated library calls, repeated
#   range/dim/head prints, and optional t-tests.
# - Pairing is done by BOTH plot_ID and ECOSUBCD (strict within-plot pairing).
#   If you want to match your old behavior, change join keys to "plot_ID" only.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("D:/1Postdoctoral/paper/paper1/20251018_计算复合极端气候/数据")
data_com <- read.csv("data_com_5.csv",header = T)
data_sin <- read.csv("data_sin_5.csv",header = T)

# -----------------------------
# 1) Utilities
# -----------------------------

# Summarize one extreme type to plot-level means for a given metric (Rs or Le).
summarise_metric <- function(df, ext_type, metric, suffix) {
  df %>%
    filter(ext_type == !!ext_type) %>%
    group_by(plot_ID, ECOSUBCD) %>%
    summarise(
      value = mean(.data[[paste0(metric, "_", suffix)]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(!!paste0(metric, "_", suffix) := value)
}

# Build a paired dataset containing compound + two individual extremes.
# Returns a wide data frame with:
#   metric_com (compound), metric_sin.x (ind1), metric_sin.y (ind2)
build_pair3 <- function(data_com, data_sin,
                        com_ext, ind1_ext, ind2_ext,
                        metric,
                        join_keys = c("plot_ID", "ECOSUBCD")) {
  
  com <- summarise_metric(data_com, com_ext, metric, "com")
  ind1 <- summarise_metric(data_sin, ind1_ext, metric, "sin")
  ind2 <- summarise_metric(data_sin, ind2_ext, metric, "sin")
  
  # Joining twice adds suffixes ".x" and ".y" to individual columns:
  #   metric_sin.x -> ind1_ext
  #   metric_sin.y -> ind2_ext
  com %>%
    inner_join(ind1, by = join_keys) %>%
    inner_join(ind2, by = join_keys)
}

# Convert paired wide data to long format for plotting:
# three groups: compound + ind1 + ind2, all on log2 scale.
to_long3_log2 <- function(dat_wide, metric, lab_com, lab_ind1, lab_ind2) {
  dat_wide %>%
    transmute(
      plot_ID,
      `compound` = log2(.data[[paste0(metric, "_com")]]),
      `ind1`     = log2(.data[[paste0(metric, "_sin.x")]]),
      `ind2`     = log2(.data[[paste0(metric, "_sin.y")]])
    ) %>%
    rename(
      !!lab_com  := compound,
      !!lab_ind1 := ind1,
      !!lab_ind2 := ind2
    ) %>%
    pivot_longer(
      cols = c(!!lab_com, !!lab_ind1, !!lab_ind2),
      names_to = "Variable",
      values_to = "Value"
    )
}

# Compute scaling factor so density can be overlaid on count histogram.
# density_scaled = density * (max_count / max_density)
density_count_scaler <- function(data_long, binwidth = 0.1) {
  max_density <- max(density(data_long$Value)$y)
  max_count <- max(
    ggplot_build(
      ggplot(data_long, aes(x = Value)) +
        geom_histogram(binwidth = binwidth)
    )$data[[1]]$count
  )
  list(max_density = max_density, max_count = max_count)
}

# Panel plotting function: histogram + scaled density + mean lines + mean labels + p label.
plot_hist_density_means <- function(data_long, scaler,
                                    x_lab_expr, x_limits = c(-3.5, 21.5),
                                    y_limits, y_breaks,
                                    legend_pos = c(0.28, 0.90),
                                    mean_line_yend,
                                    p_label_expr = "italic(p)<0.001",
                                    fill_vals = c("#F5C266", "#7DA7CA", "#66B966"),
                                    color_vals = c("#efa112", "#4682b4", "#219a21"),
                                    level_order) {
  
  # Set legend/draw order
  data_long$Variable <- factor(data_long$Variable, levels = level_order)
  
  # Means for dashed lines and numeric labels
  mean_df <- data_long %>%
    group_by(Variable) %>%
    summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_value) %>%
    mutate(y_position = seq(y_limits[2] * 0.375, y_limits[2] * 0.50, length.out = n()))
  
  ggplot(data_long, aes(x = Value)) +
    geom_histogram(aes(y = ..count.., fill = Variable),
                   binwidth = 0.3, alpha = 0.8,
                   position = "identity", color = "white") +
    geom_density(aes(y = ..density.. * scaler$max_count / scaler$max_density,
                     color = Variable),
                 size = 0.6) +
    scale_x_continuous(breaks = seq(-3, 21, 6), limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(
      name = "Number of plots (n)",
      limits = y_limits,
      breaks = y_breaks,
      expand = expansion(mult = c(0, 0.05)),
      sec.axis = sec_axis(~ . * scaler$max_density / scaler$max_count,
                          name = "Kernel density")
    ) +
    geom_segment(data = mean_df,
                 aes(x = mean_value, xend = mean_value, y = 0, yend = mean_line_yend, color = Variable),
                 linetype = "dashed", size = 0.6, show.legend = FALSE) +
    geom_text(data = mean_df,
              aes(x = mean_value, y = y_position, label = sprintf("%.3f", mean_value), color = Variable),
              hjust = 2.5, size = 5, show.legend = FALSE) +
    annotate("text", x = x_limits[1] + 0.5, y = y_limits[1] + 25,
             label = p_label_expr, parse = TRUE, size = 4.5, hjust = 0, vjust = 0.5) +
    scale_fill_manual(values = fill_vals, name = NULL) +
    scale_color_manual(values = color_vals, name = NULL) +
    labs(x = x_lab_expr) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0.3, 0.35, 0.3, 0.35), "cm"),
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.key.size = unit(0.3, "cm"),
      legend.spacing = unit(0.4, "cm"),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(hjust = 0.5, size = 16, angle = 90),
      axis.text.y.right = element_text(hjust = 0.5, size = 16, angle = 270),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      legend.position = legend_pos,
      legend.background = element_blank(),
      legend.box.background = element_blank()
    )
}

# Paired Wilcoxon tests: compound vs ind1 and compound vs ind2 (alternative = "less")
wilcox_two <- function(dat_wide, metric) {
  com <- dat_wide[[paste0(metric, "_com")]]
  ind1 <- dat_wide[[paste0(metric, "_sin.x")]]
  ind2 <- dat_wide[[paste0(metric, "_sin.y")]]
  
  list(
    p_ind1 = wilcox.test(com, ind1, paired = TRUE, alternative = "less")$p.value,
    p_ind2 = wilcox.test(com, ind2, paired = TRUE, alternative = "less")$p.value
  )
}

# -----------------------------
# 2) Panel specifications
# -----------------------------
# Each panel is defined by:
# - compound extreme type
# - two individual extreme types
# - metric (Rs or Le)
# - labels used in the plot
# - y-axis settings (counts) and legend position
panels <- list(
  a = list(
    com_ext = "pup_tup", ind1_ext = "pup",  ind2_ext = "tup",
    metric = "Rs",
    lab_com  = "Compound heat and moisture excess",
    lab_ind1 = "Moisture excess alone",
    lab_ind2 = "Heat alone",
    x_lab = expression(Resistance ~ (ln(italic(R)[s]))),
    y_limits = c(0, 500), y_breaks = seq(0, 500, 100),
    legend_pos = c(0.29, 0.90), mean_yend = 420
  ),
  b = list(
    com_ext = "pdown_tup", ind1_ext = "pdown", ind2_ext = "tup",
    metric = "Rs",
    lab_com  = "Compound heat and drought",
    lab_ind1 = "Drought alone",
    lab_ind2 = "Heat alone",
    x_lab = expression(Resistance ~ (ln(italic(R)[s]))),
    y_limits = c(0, 400), y_breaks = seq(0, 400, 100),
    legend_pos = c(0.23, 0.90), mean_yend = 350
  ),
  c = list(
    com_ext = "pup_tup", ind1_ext = "pup",  ind2_ext = "tup",
    metric = "Le",
    lab_com  = "Compound heat and moisture excess",
    lab_ind1 = "Moisture excess alone",
    lab_ind2 = "Heat alone",
    x_lab = expression(Recovery ~ (ln(italic(R)[c]))),
    y_limits = c(0, 500), y_breaks = seq(0, 500, 100),
    legend_pos = c(0.29, 0.90), mean_yend = 420
  ),
  d = list(
    com_ext = "pdown_tup", ind1_ext = "pdown", ind2_ext = "tup",
    metric = "Le",
    lab_com  = "Compound heat and drought",
    lab_ind1 = "Drought alone",
    lab_ind2 = "Heat alone",
    x_lab = expression(Recovery ~ (ln(italic(R)[c]))),
    y_limits = c(0, 400), y_breaks = seq(0, 400, 100),
    legend_pos = c(0.23, 0.90), mean_yend = 350
  )
)

# -----------------------------
# 3) Run one panel (reusable)
# -----------------------------
run_panel <- function(spec, data_com, data_sin,
                      join_keys = c("plot_ID", "ECOSUBCD"),
                      p_label_expr = "italic(p)<0.001") {
  
  # Build paired wide dataset: compound + 2 individual extremes
  dat_wide <- build_pair3(
    data_com = data_com,
    data_sin = data_sin,
    com_ext  = spec$com_ext,
    ind1_ext = spec$ind1_ext,
    ind2_ext = spec$ind2_ext,
    metric   = spec$metric,
    join_keys = join_keys
  )
  
  # Wilcoxon tests (compound vs each individual)
  pvals <- wilcox_two(dat_wide, spec$metric)
  
  # Prepare long dataset on log2 scale for plotting
  dat_long <- to_long3_log2(dat_wide, spec$metric, spec$lab_com, spec$lab_ind1, spec$lab_ind2)
  
  # Compute density scaling to overlay on histogram counts
  scaler <- density_count_scaler(dat_long, binwidth = 0.1)
  
  # Make plot
  p <- plot_hist_density_means(
    data_long = dat_long,
    scaler = scaler,
    x_lab_expr = spec$x_lab,
    y_limits = spec$y_limits,
    y_breaks = spec$y_breaks,
    legend_pos = spec$legend_pos,
    mean_line_yend = spec$mean_yend,
    p_label_expr = p_label_expr,
    level_order = c(spec$lab_com, spec$lab_ind2, spec$lab_ind1)  # matches your original ordering
  )
  
  # Return both plot and p-values (useful for left-side symbols)
  list(plot = p, pvals = pvals, n = nrow(dat_wide))
}

# -----------------------------
# 4) Generate Figure 3 panels
# -----------------------------
# If you want to match your original behavior (pair by plot_ID only), set:
# join_keys = "plot_ID"
out_a <- run_panel(panels$a, data_com, data_sin, join_keys = c("plot_ID", "ECOSUBCD"))
out_b <- run_panel(panels$b, data_com, data_sin, join_keys = c("plot_ID", "ECOSUBCD"))
out_c <- run_panel(panels$c, data_com, data_sin, join_keys = c("plot_ID", "ECOSUBCD"))
out_d <- run_panel(panels$d, data_com, data_sin, join_keys = c("plot_ID", "ECOSUBCD"))

# Plots
pa_1 <- out_a$plot
pb_1 <- out_b$plot
pc_1 <- out_c$plot
pd_1 <- out_d$plot

pa_1
pb_1
pc_1
pd_1

# Paired Wilcoxon p-values (compound vs each individual) and sample sizes
out_a$pvals; out_a$n
out_b$pvals; out_b$n
out_c$pvals; out_c$n
out_d$pvals; out_d$n
