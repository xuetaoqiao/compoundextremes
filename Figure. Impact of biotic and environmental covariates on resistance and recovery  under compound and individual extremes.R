# ============================================================
# Figure 6 (clean version)
# Impact of standardized biotic + environmental covariates on
# Resistance (a) and Recovery (b) under compound vs. individual extremes
# ============================================================

# -----------------------------
# Packages (keep only what is used)
# -----------------------------
rm(list=ls())

library(nlme)
library(car)
library(dplyr)
library(tibble)
library(ggplot2)

setwd("D:/1Postdoctoral/paper/paper1/20251018_计算复合极端气候/数据")
data_com <- read.csv("data_com_5.csv",header = T)
data_sin <- read.csv("data_sin_5.csv",header = T)

# ============================================================
# 1) Helper functions
# ============================================================

# Z-score standardization (mean = 0, SD = 1), robust to NA
scale_z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# log2 for species richness (if SR can be 0, use log2(sr + 1))
log2_sr <- function(sr) log2(sr)

# Add standardized predictors (and optionally standardized Rs for recovery models)
add_scaled_predictors <- function(df, rs_col_for_recovery = NULL) {
  df <- df %>%
    mutate(
      logSR_scale  = scale_z(log2_sr(species_richness)),
      STDAGE_scale = scale_z(STDAGE),
      ELEV_scale   = scale_z(ELEV),
      ph_scale     = scale_z(ph),
      totaln_scale = scale_z(totaln),
      bulk_scale   = scale_z(bulk),
      clay_scale   = scale_z(clay),
      pre_scale    = scale_z(pre),
      tem_scale    = scale_z(tem),
      time    = as.integer(year) -2001
    )
  
  if (!is.null(rs_col_for_recovery)) {
    df <- df %>% mutate(Rs_scale = scale_z(log2(.data[[rs_col_for_recovery]])))
  }
  df
}

# Tidy fixed effects table: β, 95% CI, p, significance flag
tidy_lme_table <- function(lme_model, extreme_type_label) {
  summary(lme_model)$tTable %>%
    as.data.frame() %>%
    rownames_to_column(var = "factor_raw") %>%
    filter(factor_raw != "(Intercept)") %>%
    mutate(
      factor      = sub("_scale$", "", factor_raw),   # nicer names on axis
      extreme.type = extreme_type_label,
      conf.low    = Value - 1.96 * Std.Error,
      conf.high   = Value + 1.96 * Std.Error,
      sig         = (`p-value` < 0.05)
    ) %>%
    select(factor, Value, Std.Error, DF, `p-value`, conf.low, conf.high, extreme.type, sig)
}

# Rename predictors to publication-ready labels (shared by Rs and Le panels)
label_factor <- function(x) {
  dplyr::case_when(
    x == "logSR"  ~ "Species richness",
    x == "STDAGE" ~ "Stand age",
    x == "pre"    ~ "Mean annual precipitation",
    x == "tem"    ~ "Mean annual temperature",
    x == "ph"     ~ "pH",
    x == "bulk"   ~ "Bulk density",
    x == "totaln" ~ "Total nitrogen content",
    x == "clay"   ~ "Clay content",
    x == "ELEV"   ~ "Elevation",
    x == "Rs"     ~ "Resistance",
    x == "time"     ~ "Occurrence timing",
    TRUE ~ x
  )
}

# Plot coefficient panel (CI bars + points; non-sig points hollow)
plot_coef_panel <- function(df, title_expr, y_limits, y_breaks, shade_n) {
  # df must already have: factor (as factor with desired order), Value, conf.low/conf.high, extreme.type, sig
  ns_df <- df %>% filter(sig == FALSE)
  
  p <- ggplot(df, aes(x = factor, y = Value, ymin = conf.low, ymax = conf.high,
                      color = extreme.type, fill = extreme.type)) +
    geom_linerange(size = 1, position = position_dodge(width = 0.5), show.legend = FALSE) +
    geom_point(size = 3.2, shape = 19, 
               position = position_dodge(width = 0.5)) +
    # hollow markers for non-significant coefficients
    geom_point(data = ns_df, aes(x = factor, y = Value),
               inherit.aes = FALSE, shape = 21, color = "white", fill = "white", size = 2) +
    geom_hline(yintercept = 0, lty = 2, color = "grey") +
    labs(title = title_expr, x = NULL, y = "Standardized regression coefficient (β)") +
    scale_y_continuous(limits = y_limits, breaks = y_breaks, expand = c(0, 0)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 14, angle = 50, vjust = 0.99, hjust = 1,
                                 family = "Arial", color = "black"),
      axis.text.y = element_text(size = 14, hjust = 0.5, angle = 90)
    )
  
  # alternating shaded vertical bands (every other covariate)
  # shade_n = number of covariates on x-axis
  # shade blocks: [1.5,2.5], [3.5,4.5], ...
  for (i in seq(2, shade_n, by = 2)) {
    p <- p + annotate("rect",
                      xmin = i - 0.5, xmax = i + 0.5,
                      ymin = y_limits[1] + 0.01, ymax = y_limits[2] - 0.01,
                      alpha = 0.2, fill = "lightgrey")
  }
  p
}

# ============================================================
# 2) Standardize predictors (create *_scale datasets)
# ============================================================
data_com_scale <- add_scaled_predictors(data_com, rs_col_for_recovery = "Rs_com")
data_sin_scale <- add_scaled_predictors(data_sin, rs_col_for_recovery = "Rs_sin")

# ============================================================
# 3) Fit models + extract coefficients
# ============================================================

# --- Recovery models (Le): include Rs_scale as buffer capacity covariate
model_Le_com <- lme(
  log2(Le_com) ~ logSR_scale + STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale + Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data   = data_com_scale
)
model_Le_sin <- lme(
  log2(Le_sin) ~ logSR_scale + STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale + Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data   = data_sin_scale
)

# --- Resistance models (Rs): no Rs_scale term
model_Rs_com <- lme(
  log2(Rs_com) ~ logSR_scale + STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data   = data_com_scale
)
model_Rs_sin <- lme(
  log2(Rs_sin) ~ logSR_scale + STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data   = data_sin_scale
)

# Optional diagnostics (keep if you need them; otherwise comment out)
vif(model_Le_com); vif(model_Le_sin)
vif(model_Rs_com); vif(model_Rs_sin)

# Extract coefficient tables
tab_Le <- bind_rows(
  tidy_lme_table(model_Le_com, "Compound extremes"),
  tidy_lme_table(model_Le_sin, "Individual extremes")
) %>%
  mutate(factor = label_factor(factor))

tab_Rs <- bind_rows(
  tidy_lme_table(model_Rs_com, "Compound extremes"),
  tidy_lme_table(model_Rs_sin, "Individual extremes")
) %>%
  mutate(factor = label_factor(factor))

# ============================================================
# 4) Set axis order (match the paper figure)
# ============================================================

order_Rs <- c(
  "Species richness", "Stand age","Occurrence timing",
  "Mean annual precipitation", "Mean annual temperature",
  "pH", "Bulk density", "Total nitrogen content", "Clay content",
  "Elevation"
)

order_Le <- c(order_Rs, "Resistance")

tab_Rs$factor <- factor(tab_Rs$factor, levels = order_Rs)
tab_Le$factor <- factor(tab_Le$factor, levels = order_Le)

tab_Rs$extreme.type <- factor(tab_Rs$extreme.type, levels = c("Compound extremes", "Individual extremes"))
tab_Le$extreme.type <- factor(tab_Le$extreme.type, levels = c("Compound extremes", "Individual extremes"))

# Colors (keep your original palette)
barCOLS <- c("#efa112", "#219a21")
dotCOLS <- c("#f5c266", "#66b966")

# Apply palette to ggplot defaults via scales inside plots
# (kept minimal: set once here and reuse by adding to plots)
scale_cols <- list(
  scale_fill_manual(values = barCOLS),
  scale_color_manual(values = dotCOLS)
)

# ============================================================
# 5) Plot Figure 6a (Resistance) and 6b (Recovery)
# ============================================================

p_a <- plot_coef_panel(
  df        = tab_Rs,
  title_expr = expression(Resistance~(ln(italic(R)[s]))),
  y_limits  = c(-0.62, 0.62),
  y_breaks  = c(-0.6, -0.3, 0, 0.3, 0.6),
  shade_n   = length(order_Rs)
) + scale_cols

p_b <- plot_coef_panel(
  df        = tab_Le,
  title_expr = expression(Recovery~(ln(italic(R)[c]))),
  y_limits  = c(-0.42, 0.42),
  y_breaks  = c(-0.4, -0.2, 0, 0.2, 0.4),
  shade_n   = length(order_Le)
) + scale_cols

p_a
p_b
