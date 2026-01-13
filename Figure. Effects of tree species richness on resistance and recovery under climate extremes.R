# ============================================================
# Figure 5 (REVISED v2): Pre-scale predictors into *_scale datasets
# ------------------------------------------------------------
# Your adjustment:
#   - Keep original datasets unchanged (data100, data300, data_com, ...)
#   - Create scaled copies: data100_scale, data300_scale, data_com_scale, ...
#   - Fit all models using *_scale datasets
# ============================================================

# -----------------------------
# Packages
# -----------------------------
library(nlme)
library(car)
library(sjPlot)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(broom)
library(ggplot2)
library(scales)
library(car)

# ============================================================
# 1) Helper functions
# ============================================================

# Z-score standardization with NA protection
scale_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Safe log2 for SR (switch to log2(sr + 1) if SR can be 0)
log2_sr <- function(sr) {
  log2(sr)
}

# Add standardized predictor columns to a dataset
# - Creates:
#     logSR_scale, STDAGE_scale, ELEV_scale, ph_scale, totaln_scale,
#     bulk_scale, clay_scale, pre_scale, tem_scale
# - For recovery models also adds:
#     Rs_scale  (standardized Rs within the dataset)
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
      tem_scale    = scale_z(tem)
    )
  
  if (!is.null(rs_col_for_recovery)) {
    df <- df %>%
      mutate(Rs_scale = scale_z(log2(.data[[rs_col_for_recovery]])))
  }
  
  df
}

# Extract coefficient + CI for a specific term from sjPlot::plot_model()
extract_term_ci <- function(pm, term_name = "logSR_scale") {
  pm$data %>%
    filter(term == term_name) %>%
    transmute(Estimate = estimate, lower = conf.low, uper = conf.high)
}

# ============================================================
# 2) Subset datasets (keep your own objects)
# ------------------------------------------------------------
# Assumed objects already exist in your workspace:
#   data_com, data_sin
# You may have renamed these to:
#   data_com, data_sin   (as in your adjustment)
# ============================================================

# If you still use data_com / data_sin, define aliases once:


data_com$year <- as.integer(data_com$year)       # 2001..2020
data_com$time <- data_com$year - 2001            # 0..19

data_sin$year <- as.integer(data_sin$year)       # 2001..2020
data_sin$time <- data_sin$year - 2001            # 0..19

# Compound extremes subsets
data100 <- data_com[data_com$ext_type == "pup_tup", ]     # compound wet–hot
data300 <- data_com[data_com$ext_type == "pdown_tup", ]   # compound dry–hot

# Individual extremes subsets
data1000 <- data_sin[data_sin$ext_type == "pup", ]        # moisture excess alone
data3000 <- data_sin[data_sin$ext_type == "tup", ]        # heat alone
data4000 <- data_sin[data_sin$ext_type == "pdown", ]      # drought alone

# ============================================================
# 3) Your adjusted scaling step: create *_scale copies
# ============================================================

# --- Compound datasets ---
data100_scale   <- add_scaled_predictors(data100,   rs_col_for_recovery = "Rs_com")
data300_scale   <- add_scaled_predictors(data300,   rs_col_for_recovery = "Rs_com")
data_com_scale  <- add_scaled_predictors(data_com,  rs_col_for_recovery = "Rs_com")

# --- Individual datasets ---
data1000_scale  <- add_scaled_predictors(data1000,  rs_col_for_recovery = "Rs_sin")
data3000_scale  <- add_scaled_predictors(data3000,  rs_col_for_recovery = "Rs_sin")
data4000_scale  <- add_scaled_predictors(data4000,  rs_col_for_recovery = "Rs_sin")
data_sin_scale  <- add_scaled_predictors(data_sin,  rs_col_for_recovery = "Rs_sin")

# ============================================================
# 4) PB models: Resistance (Rs) ~ standardized predictors
# ------------------------------------------------------------
# Fit with *_scale datasets only
# ============================================================

# Compound wet–hot
model_nlme_mode1 <- lme(
  log2(Rs_com) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + totaln_scale + bulk_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data100_scale
)
vif(model_nlme_mode1)
summary(model_nlme_mode1)

# Compound dry–hot
model_nlme_mode3 <- lme(
  log2(Rs_com) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data300_scale
)
vif(model_nlme_mode3)
summary(model_nlme_mode3)

# All compound extremes pooled
model_nlme_mode_com_total <- lme(
  log2(Rs_com) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data_com_scale
)
vif(model_nlme_mode_com_total)
summary(model_nlme_mode_com_total)

# Individual moisture excess
model_nlme_mode10 <- lme(
  log2(Rs_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + totaln_scale + bulk_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data1000_scale
)
vif(model_nlme_mode10)
summary(model_nlme_mode10)

# Individual heat
model_nlme_mode30 <- lme(
  log2(Rs_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + totaln_scale + bulk_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data3000_scale
)
vif(model_nlme_mode30)
summary(model_nlme_mode30)

# Individual drought
model_nlme_mode40 <- lme(
  log2(Rs_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + totaln_scale + bulk_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data4000_scale
)
vif(model_nlme_mode40)
summary(model_nlme_mode40)

# All individual extremes pooled
model_nlme_mode_sin_total <- lme(
  log2(Rs_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + totaln_scale + bulk_scale + clay_scale +
    pre_scale + tem_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data_sin_scale
)
vif(model_nlme_mode_sin_total)
summary(model_nlme_mode_sin_total)

# ============================================================
# 5) PB plot data: extract SR effect (θ) and 95% CI
# ============================================================

pm_com_total_Rs <- plot_model(model_nlme_mode_com_total)
pm_com_1_Rs     <- plot_model(model_nlme_mode1)
pm_com_3_Rs     <- plot_model(model_nlme_mode3)

pm_sin_total_Rs <- plot_model(model_nlme_mode_sin_total)
pm_sin_1_Rs     <- plot_model(model_nlme_mode10)
pm_sin_3_Rs     <- plot_model(model_nlme_mode30)
pm_sin_4_Rs     <- plot_model(model_nlme_mode40)

Rs_tab <- bind_rows(
  extract_term_ci(pm_com_total_Rs, "logSR_scale"),
  extract_term_ci(pm_com_1_Rs,     "logSR_scale"),
  extract_term_ci(pm_com_3_Rs,     "logSR_scale"),
  extract_term_ci(pm_sin_total_Rs, "logSR_scale"),
  extract_term_ci(pm_sin_1_Rs,     "logSR_scale"),
  extract_term_ci(pm_sin_3_Rs,     "logSR_scale"),
  extract_term_ci(pm_sin_4_Rs,     "logSR_scale")
)

extrem_type <- c("com","pre_up&tem_up","pre_down&tem_up",
                 "sin","pre_up","tem_up","pre_down")

com_type <- c("Compound extremes","Compound extremes","Compound extremes",
              "Individual extremes","Individual extremes","Individual extremes","Individual extremes")

newtemn3 <- Rs_tab %>%
  mutate(com_type = com_type, extrem_type = extrem_type) %>%
  mutate(
    extrem_type = factor(extrem_type,
                         levels = c("com","pre_up&tem_up","pre_down&tem_up",
                                    "sin","tem_up","pre_up","pre_down")),
    extrem_type = fct_recode(extrem_type,
                             "Compound\nextremes" = "com",
                             "Compound\nheat and moisture\nexcess" = "pre_up&tem_up",
                             "Compound\nheat and drought" = "pre_down&tem_up",
                             "Individual\nextremes" = "sin",
                             "Heat\nalone" = "tem_up",
                             "Moisture excess\nalone" = "pre_up",
                             "Drought\nalone" = "pre_down")
  )

# Sample sizes under each category (based on your original subsets)
plot_num <- data.frame(
  extreme_type = c("compound extremes",
                   "compound heat and moisture excess extremes",
                   "compound heat and drought extremes",
                   "individual extremes",
                   "individual moisture excess extremes",
                   "individual heat extremes",
                   "individual drought extremes"),
  lbl = c(nrow(data_com_scale), nrow(data100_scale), nrow(data300_scale),
          nrow(data_sin_scale), nrow(data1000_scale), nrow(data3000_scale), nrow(data4000_scale))
) %>%
  mutate(
    extreme_type = fct_recode(extreme_type,
                              "Compound\nextremes" = "compound extremes",
                              "Compound\nheat and moisture\nexcess" = "compound heat and moisture excess extremes",
                              "Compound\nheat and drought" = "compound heat and drought extremes",
                              "Individual\nextremes" = "individual extremes",
                              "Heat\nalone" = "individual heat extremes",
                              "Moisture excess\nalone" = "individual moisture excess extremes",
                              "Drought\nalone" = "individual drought extremes")
  )

barCOLS <- c("#efa112","#219a21")
dotCOLS <- c("#f5c266","#66b966")

pb <- ggplot(newtemn3, aes(x = extrem_type, y = Estimate, ymin = lower, ymax = uper,
                           col = com_type, fill = com_type)) +

  geom_linerange(size = 1, position = position_dodge(width = 0.5), show.legend = FALSE) +###
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  geom_point(size = 3.2, shape = 19, 
             position = position_dodge(width = 0.5)) +###
  
  
  scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2), limits = c(-0.12, 0.22)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey") +
  labs(x = NULL,
       y = expression(atop(Species~diversity~effect~(italic(θ))~on,
                           paste(resistance~(ln(italic(R)[s])))))) +
  annotate("rect", xmin = 0.4, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#e9e2c8") +
  annotate("rect", xmin = 3.5, xmax = 7.6, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#bbdcc5") +
  theme_bw() +
  geom_text(data = plot_num,
            aes(x = extreme_type, y = -0.1, label = paste0("(", lbl, ")")),
            inherit.aes = FALSE, size = 6, color = "grey50")
pb

# ============================================================
# 6) PD models: Recovery (Le) ~ standardized predictors + Rs_scale
# ============================================================

# Compound wet–hot
model_Le_mode1 <- lme(
  log2(Le_com) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + totaln_scale + bulk_scale + clay_scale +
    pre_scale + tem_scale +
    Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data100_scale
)
vif(model_Le_mode1)
summary(model_Le_mode1)

# Compound dry–hot
model_Le_mode3 <- lme(
  log2(Le_com) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale +
    Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data300_scale
)
vif(model_Le_mode3)
summary(model_Le_mode3)

# All compound extremes pooled
model_Le_mode_com_total <- lme(
  log2(Le_com) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale +
    Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data_com_scale
)
vif(model_Le_mode_com_total)
summary(model_Le_mode_com_total)

# Individual moisture excess
model_Le_mode10 <- lme(
  log2(Le_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale +
    Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data1000_scale
)
vif(model_Le_mode10)
summary(model_Le_mode10)

# Individual heat
model_Le_mode30 <- lme(
  log2(Le_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale +
    Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data3000_scale
)
vif(model_Le_mode30)
summary(model_Le_mode30)

# Individual drought
model_Le_mode40 <- lme(
  log2(Le_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale +
    Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data4000_scale
)
vif(model_Le_mode40)
summary(model_Le_mode40)

# All individual extremes pooled
model_Le_mode_sin_total <- lme(
  log2(Le_sin) ~
    logSR_scale +
    STDAGE_scale + ELEV_scale +
    ph_scale + bulk_scale + totaln_scale + clay_scale +
    pre_scale + tem_scale +
    Rs_scale + time,
  random = ~ 1 | ECOSUBCD/plot_ID,
  data = data_sin_scale
)
vif(model_Le_mode_sin_total)
summary(model_Le_mode_sin_total)

# ============================================================
# 7) PD plot data: extract SR effect (θ) and 95% CI for Le models
# ============================================================

pm_com_total_Le <- plot_model(model_Le_mode_com_total)
pm_com_1_Le     <- plot_model(model_Le_mode1)
pm_com_3_Le     <- plot_model(model_Le_mode3)

pm_sin_total_Le <- plot_model(model_Le_mode_sin_total)
pm_sin_1_Le     <- plot_model(model_Le_mode10)
pm_sin_3_Le     <- plot_model(model_Le_mode30)
pm_sin_4_Le     <- plot_model(model_Le_mode40)

Le_tab <- bind_rows(
  extract_term_ci(pm_com_total_Le, "logSR_scale"),
  extract_term_ci(pm_com_1_Le,     "logSR_scale"),
  extract_term_ci(pm_com_3_Le,     "logSR_scale"),
  extract_term_ci(pm_sin_total_Le, "logSR_scale"),
  extract_term_ci(pm_sin_1_Le,     "logSR_scale"),
  extract_term_ci(pm_sin_3_Le,     "logSR_scale"),
  extract_term_ci(pm_sin_4_Le,     "logSR_scale")
)

newtemn_Le3 <- Le_tab %>%
  mutate(com_type = com_type, extrem_type = extrem_type) %>%
  mutate(
    extrem_type = factor(extrem_type,
                         levels = c("com","pre_up&tem_up","pre_down&tem_up",
                                    "sin","tem_up","pre_up","pre_down")),
    extrem_type = fct_recode(extrem_type,
                             "Compound\nextremes" = "com",
                             "Compound\nheat and moisture\nexcess" = "pre_up&tem_up",
                             "Compound\nheat and drought" = "pre_down&tem_up",
                             "Individual\nextremes" = "sin",
                             "Heat\nalone" = "tem_up",
                             "Moisture excess\nalone" = "pre_up",
                             "Drought\nalone" = "pre_down")
  )

pd <- ggplot(newtemn_Le3, aes(x = extrem_type, y = Estimate, ymin = lower, ymax = uper,
                              col = com_type, fill = com_type)) +
  geom_linerange(size = 1, position = position_dodge(width = 0.5), show.legend = FALSE) +###
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  geom_point(size = 3.2, shape = 19, 
             position = position_dodge(width = 0.5)) +###
  scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2), limits = c(-0.12, 0.22)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey") +
  labs(x = NULL,
       y = expression(atop(Species~diversity~effect~(italic(θ))~on,
                           paste(recovery~(ln(italic(R)[c])))))) +
  annotate("rect", xmin = 0.4, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#e9e2c8") +
  annotate("rect", xmin = 3.5, xmax = 7.6, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#bbdcc5") +
  theme_bw() +
  geom_text(data = plot_num,
            aes(x = extreme_type, y = -0.1, label = paste0("(", lbl, ")")),
            inherit.aes = FALSE, size = 6, color = "grey50")
pd

# ============================================================
# 8) PA & PC: raw bivariate plots (keep as descriptive)
# ------------------------------------------------------------
# Use ORIGINAL datasets (not scaled copies) because PA/PC are log–log
# scatterplots and linear fits of the raw relationship.
# ============================================================

data_sin_7 <- data_sin %>%
  select(species_richness, Rs_sin, Le_sin) %>%
  rename(Rs = Rs_sin, Le = Le_sin) %>%
  mutate(factor = "Individual extremes")

data_com_7 <- data_com %>%
  select(species_richness, Rs_com, Le_com) %>%
  rename(Rs = Rs_com, Le = Le_com) %>%
  mutate(factor = "Compound extremes")

data_combined <- bind_rows(data_sin_7, data_com_7)

# --- PA label (Rs) ---
data_stats_p1 <- data_combined %>%
  group_by(factor) %>%
  summarise(model = list(lm(log2(Rs) ~ log2_sr(species_richness), data = cur_data())),
            .groups = "drop") %>%
  mutate(
    g = map(model, broom::glance),
    df_num = map_dbl(model, ~ length(coef(.x)) - 1),
    df_den = map_dbl(model, ~ df.residual(.x))
  ) %>%
  unnest(g) %>%
  rename(F_value = statistic, p_value = p.value) %>%
  mutate(
    label = ifelse(
      p_value < 0.001,
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) < 0.001"),
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) == ", formatC(p_value, format = "e", digits = 2))
    ),
    x = 4.585, y = 20
  )

pa <- ggplot(data_combined, aes(x = log2_sr(species_richness), y = log2(Rs),
                                color = factor, fill = factor)) +
  geom_point(alpha = 0.95, size = 2, shape = 19, show.legend = FALSE) +
  geom_smooth(data = filter(data_combined, factor == "Compound extremes"),
              color = "#efa112", fill = "#efa112",
              method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.6) +
  geom_smooth(data = filter(data_combined, factor == "Individual extremes"),
              color = "#219a21", fill = "#219a21",
              method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.6) +
  scale_x_continuous(name = expression(Species~richness~(italic(S))),
                     limits = c(0, 4.585),
                     breaks = c(0, 1, 2, 3, 4, 4.585),
                     labels = c("1", "2", "4", "8", "16", "24"),
                     expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(name = expression(Resistance~(ln(italic(R)[s]))),
                     breaks = c(0, 5, 10, 15, 20),
                     limits = c(0, 20),
                     expand = expansion(mult = c(0.1, 0.1))) +
  facet_wrap(~ fct_relevel(factor, "Compound extremes", "Individual extremes"),
             ncol = 1, strip.position = "right") +
  geom_text(data = data_stats_p1,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 1, size = 5.5,
            color = "black", parse = TRUE) +
  scale_colour_manual(values = c("#e9e2c8", "#bbdcc5")) +
  scale_fill_manual(values = c("#e9e2c8", "#bbdcc5")) +
  theme_bw() +
  theme(
    strip.background.x = element_rect(color = "white", fill = "white"),
    strip.background.y = element_rect(color = "white", fill = "white"),
    strip.placement = "outside",
    panel.spacing.x = unit(0.3, "cm"),
    panel.spacing.y = unit(0.3, "cm"),
    legend.position = "none"
  )
pa

# --- PC label (Le) ---
data_stats_p2 <- data_combined %>%
  group_by(factor) %>%
  summarise(model = list(lm(log2(Le) ~ log2_sr(species_richness), data = cur_data())),
            .groups = "drop") %>%
  mutate(
    g = map(model, broom::glance),
    df_num = map_dbl(model, ~ length(coef(.x)) - 1),
    df_den = map_dbl(model, ~ df.residual(.x))
  ) %>%
  unnest(g) %>%
  rename(F_value = statistic, p_value = p.value) %>%
  mutate(
    label = ifelse(
      p_value < 0.001,
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) < 0.001"),
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) == ", formatC(p_value, format = "e", digits = 2))
    ),
    x = 4.585, y = 20
  )

pc <- ggplot(data_combined, aes(x = log2_sr(species_richness), y = log2(Le),
                                color = factor, fill = factor)) +
  geom_point(alpha = 0.95, size = 2, shape = 19, show.legend = FALSE) +
  geom_smooth(data = filter(data_combined, factor == "Compound extremes"),
              color = "#efa112", fill = "#efa112",
              method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.6) +
  geom_smooth(data = filter(data_combined, factor == "Individual extremes"),
              color = "#219a21", fill = "#219a21",
              method = "lm", formula = y ~ x, se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.6) +
  scale_x_continuous(name = expression(Species~richness~(italic(S))),
                     limits = c(0, 4.585),
                     breaks = c(0, 1, 2, 3, 4, 4.585),
                     labels = c("1", "2", "4", "8", "16", "24"),
                     expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(name = expression(Recovery~(ln(italic(R)[c]))),
                     breaks = c(0, 5, 10, 15, 20),
                     limits = c(0, 20),
                     expand = expansion(mult = c(0.1, 0.1))) +
  facet_wrap(~ fct_relevel(factor, "Compound extremes", "Individual extremes"),
             ncol = 1, strip.position = "right") +
  geom_text(data = data_stats_p2,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 1, size = 5.5,
            color = "black", parse = TRUE) +
  scale_colour_manual(values = c("#e9e2c8", "#bbdcc5")) +
  scale_fill_manual(values = c("#e9e2c8", "#bbdcc5")) +
  theme_bw() +
  theme(
    strip.background.x = element_rect(color = "white", fill = "white"),
    strip.background.y = element_rect(color = "white", fill = "white"),
    strip.placement = "outside",
    panel.spacing.x = unit(0.3, "cm"),
    panel.spacing.y = unit(0.3, "cm"),
    legend.position = "none"
  )
pc
