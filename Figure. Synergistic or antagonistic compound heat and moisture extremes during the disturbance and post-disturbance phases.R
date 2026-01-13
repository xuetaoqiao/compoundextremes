# ============================================================
# Figure 4
# ------------------------------------------------------------
# Synergistic or antagonistic compound heat–moisture extremes during:
#   (1) Disturbance phase  (PGR_dis_*)
#   (2) Post-disturbance phase (PGR_leg_*)
#
# Plot interpretation:
# - Left bars (negative x)  = % of plots showing SYNERGISTIC effects
# - Right bars (positive x) = % of plots showing ANTAGONISTIC effects
# - Two compound types are compared:
#     * "pup_tup"   = compound heat + moisture excess (wet–hot)
#     * "pdown_tup" = compound heat + drought         (dry–hot)
#
# How synergy/antagonism is defined here:
# - Let S = (individual effect #1) + (individual effect #2)
# - Let C = compound effect
# - We first classify the sign-pattern of the two individual effects:
#     Scenario A: both individuals positive
#     Scenario B: opposite signs or one is zero
#     Scenario C: both individuals negative
# - Then we compare S against C:
#     In Scenarios A and B:
#        S < C  -> antagonistic
#        S > C  -> synergistic
#        S == C -> additive
#     In Scenario C (both negative):
#        S < C  -> synergistic  (compound is “more negative” than expected)
#        S > C  -> antagonistic
#        S == C -> additive
#
# IMPORTANT:
# - Your joins are by plot_ID only (not ECOSUBCD). This means all records
#   sharing plot_ID are matched regardless of ecosubregion.
# - density/plotting packages are loaded only once (removed redundancy).
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================================
# 1) Helper: summarize a dataset to plot-level means for selected variables
# ------------------------------------------------------------
# df       : input data (compound or individual)
# ext_type : target extreme type (e.g., "pup_tup", "pup", "tup", "pdown_tup", "pdown")
# suffix   : "com" or "sin" to address variable names (e.g., PGR_dis_com vs PGR_dis_sin)
#
# Returns a plot-level table containing:
#   plot_ID, Rs_*, Le_*, PGR_dis_*, PGR_leg_*
# ============================================================
summarise_plot_means <- function(df, ext_type, suffix, na_rm_pgr = TRUE) {
  df %>%
    filter(ext_type == !!ext_type) %>%
    group_by(plot_ID) %>%
    summarise(
      Rs = mean(.data[[paste0("Rs_", suffix)]], na.rm = TRUE),
      Le = mean(.data[[paste0("Le_", suffix)]], na.rm = TRUE),
      PGR_dis = mean(.data[[paste0("PGR_dis_", suffix)]], na.rm = na_rm_pgr),
      PGR_leg = mean(.data[[paste0("PGR_leg_", suffix)]], na.rm = na_rm_pgr),
      .groups = "drop"
    ) %>%
    rename_with(~ paste0(., "_", suffix), c("Rs", "Le", "PGR_dis", "PGR_leg"))
}

# ============================================================
# 2) Helper: classify synergistic / antagonistic / additive
# ------------------------------------------------------------
# phase_var must be either "PGR_dis" (disturbance) or "PGR_leg" (post-disturbance).
#
# The input must be a wide table that has:
#   <phase_var>_com   : compound value
#   <phase_var>_sin.x : individual #1
#   <phase_var>_sin.y : individual #2
#
# Output:
#   Adds two columns:
#     scenario: A / B / C depending on signs of individual effects
#     effect  : synergistic / antagonistic / additive according to rules above
# ============================================================
classify_effect <- function(dat_wide, phase_var = c("PGR_dis", "PGR_leg")) {
  phase_var <- match.arg(phase_var)
  
  ind1 <- dat_wide[[paste0(phase_var, "_sin.x")]]
  ind2 <- dat_wide[[paste0(phase_var, "_sin.y")]]
  com  <- dat_wide[[paste0(phase_var, "_com")]]
  
  dat_wide %>%
    mutate(
      # Scenario classification based on the signs of individual effects
      scenario = case_when(
        ind1 > 0 & ind2 > 0 ~ "A",  # both positive
        (ind1 >= 0 & ind2 <= 0) | (ind1 <= 0 & ind2 >= 0) ~ "B", # mixed / includes zero
        ind1 < 0 & ind2 < 0 ~ "C"   # both negative
      ),
      # Combined expectation from individual effects
      sum_individual = ind1 + ind2,
      # Effect type (synergistic/antagonistic/additive) depends on scenario
      effect = case_when(
        scenario %in% c("A", "B") & sum_individual < com  ~ "antagonistic",
        scenario %in% c("A", "B") & sum_individual > com  ~ "synergistic",
        scenario %in% c("A", "B") & sum_individual == com ~ "additive",
        
        # When both individual effects are negative, the “direction” flips
        scenario == "C" & sum_individual < com  ~ "synergistic",
        scenario == "C" & sum_individual > com  ~ "antagonistic",
        scenario == "C" & sum_individual == com ~ "additive",
        
        TRUE ~ NA_character_
      )
    )
}

# ============================================================
# 3) Helper: build a paired wide table for one compound type
# ------------------------------------------------------------
# com_ext : compound type  (e.g., "pup_tup" or "pdown_tup")
# ind1_ext: individual #1  (e.g., "pup" or "pdown")
# ind2_ext: individual #2  (e.g., "tup")
#
# Returns a wide table joined by plot_ID:
#   ..._com plus ..._sin.x and ..._sin.y
# ============================================================
build_wide_pair <- function(com_ext, ind1_ext, ind2_ext) {
  com  <- summarise_plot_means(data_com, com_ext,  suffix = "com", na_rm_pgr = TRUE)
  ind1 <- summarise_plot_means(data_sin, ind1_ext, suffix = "sin", na_rm_pgr = TRUE)
  ind2 <- summarise_plot_means(data_sin, ind2_ext, suffix = "sin", na_rm_pgr = TRUE)
  
  com %>%
    inner_join(ind1, by = "plot_ID") %>%
    inner_join(ind2, by = "plot_ID")
}

# ============================================================
# 4) Compute synergistic / antagonistic counts for BOTH phases
# ------------------------------------------------------------
# We do this for:
#   - wet–hot compound (pup_tup) = (pup + tup)
#   - dry–hot compound (pdown_tup) = (pdown + tup)
# ============================================================

# ---- Wet–hot compound (pup_tup) wide paired data ----
wide_pup_tup <- build_wide_pair(com_ext = "pup_tup", ind1_ext = "pup", ind2_ext = "tup")

# Disturbance phase classification (PGR_dis)
pup_tup_dis <- classify_effect(wide_pup_tup, phase_var = "PGR_dis")

# Post-disturbance phase classification (PGR_leg)
pup_tup_leg <- classify_effect(wide_pup_tup, phase_var = "PGR_leg")

# ---- Dry–hot compound (pdown_tup) wide paired data ----
wide_pdown_tup <- build_wide_pair(com_ext = "pdown_tup", ind1_ext = "pdown", ind2_ext = "tup")

pdown_tup_dis <- classify_effect(wide_pdown_tup, phase_var = "PGR_dis")
pdown_tup_leg <- classify_effect(wide_pdown_tup, phase_var = "PGR_leg")

# ============================================================
# 5) Helper: extract synergistic/antagonistic percentages for plotting
# ------------------------------------------------------------
# Returns a one-row summary containing:
#   Synergistic_effect (%), Antagonistic_effect (%),
#   Synergistic_num (n), Antagonistic_num (n)
# ============================================================
summarise_effect_share <- function(dat, pair_label, phase_title) {
  syn_n <- sum(dat$effect == "synergistic", na.rm = TRUE)
  ant_n <- sum(dat$effect == "antagonistic", na.rm = TRUE)
  total <- syn_n + ant_n  # additive is excluded from the denominator in your code
  
  tibble(
    Pair = pair_label,
    title = phase_title,
    Synergistic_effect = syn_n / total * 100,
    Synergistic_num    = syn_n,
    Antagonistic_effect = ant_n / total * 100,
    Antagonistic_num    = ant_n
  )
}

# Build the plotting table (two pairs × two phases)
data_effect <- bind_rows(
  summarise_effect_share(pup_tup_dis,  "Compound heat and moisture excess", "Disturbance phase"),
  summarise_effect_share(pdown_tup_dis,"Compound heat and drought",         "Disturbance phase"),
  summarise_effect_share(pup_tup_leg,  "Compound heat and moisture excess", "Post-disturbance phase"),
  summarise_effect_share(pdown_tup_leg,"Compound heat and drought",         "Post-disturbance phase")
) %>%
  mutate(
    # Order pairs to match your original display
    Pair = factor(Pair, levels = c("Compound heat and drought",
                                   "Compound heat and moisture excess"))
  )

# ============================================================
# 6) Convert to long format for a diverging (symmetric) bar chart
# ------------------------------------------------------------
# Design:
# - Synergistic is plotted on the LEFT by making values negative.
# - Antagonistic is plotted on the RIGHT by keeping values positive.
# - We keep both percentage and count; only percentage is used for bar lengths.
# ============================================================

# Long format: percentages
data_long_pct <- data_effect %>%
  pivot_longer(
    cols = c(Synergistic_effect, Antagonistic_effect),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    # Put synergistic bars on the left side (negative)
    value = if_else(variable == "Synergistic_effect", -value, value)
  )

# Long format: counts (n)
data_long_n <- data_effect %>%
  pivot_longer(
    cols = c(Synergistic_num, Antagonistic_num),
    names_to = "variable",
    values_to = "num"
  ) %>%
  mutate(
    # Align variable names with the percentage table
    variable = recode(variable,
                      Synergistic_num = "Synergistic_effect",
                      Antagonistic_num = "Antagonistic_effect"),
    # Keep sign consistent with plotting direction (optional)
    num = if_else(variable == "Synergistic_effect", -num, num)
  )

# Merge percent and count
data_long_total <- data_long_pct %>%
  left_join(data_long_n, by = c("Pair", "title", "variable"))

# ============================================================
# 7) Plot: diverging bar chart (Figure 4)
# ------------------------------------------------------------
# Left side  (negative): synergistic percentage
# Right side (positive): antagonistic percentage
# Facets: disturbance vs post-disturbance
# Text inside bars: (xx%) with vertical orientation
# ============================================================

p1 <- ggplot(data_long_total,
             aes(x = value, y = Pair, fill = variable, color = variable)) +
  geom_bar(stat = "identity",
           position = "identity",
           width = 0.8,
           alpha = 0.6,
           size = 0.8) +
  
  scale_fill_manual(values = c(Synergistic_effect = "#A41D1A",
                               Antagonistic_effect = "#D26A18")) +
  scale_color_manual(values = c(Synergistic_effect = "#A41D1A",
                                Antagonistic_effect = "#D26A18")) +
  
  xlab("Percentage of combined effect type (%)") +
  # Show symmetric axis labels as positive numbers on both sides
  scale_x_continuous(
    breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
    limits = c(-102, 102),
    labels = c(100, 75, 50, 25, 0, 25, 50, 75, 100)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "black", size = 0.6, alpha = 0.75) +
  
  theme_bw() +
  theme(
    plot.margin = unit(c(0.3, 0.35, 0.3, 0.35), "cm"),
    legend.position = "none",
    axis.title.x = element_text(size = 16),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 16),
    strip.background = element_rect(color = alpha("white", 0.01),
                                    fill = "white", size = 2),
    strip.placement = "outside",
    panel.spacing.x = unit(0.3, "cm"),
    panel.spacing.y = unit(0.3, "cm")
  ) +
  
  # Percent labels inside bars (synergistic on left, antagonistic on right)
  geom_text(
    aes(label = ifelse(variable == "Synergistic_effect",
                       sprintf("(%.0f%%)", abs(value)), "")),
    vjust = 1.5, size = 4, colour = "grey95", angle = 90
  ) +
  geom_text(
    aes(label = ifelse(variable == "Antagonistic_effect",
                       sprintf("(%.0f%%)", abs(value)), "")),
    vjust = 1.5, size = 4, colour = "grey95", angle = 270
  ) +
  
  facet_grid(. ~ title, scales = "free_y")

# Add side annotations describing left vs right meaning
p1_1 <- p1 +
  annotate("text", x = -97, y = 1.5, label = "Synergistic effect",
           size = 5, angle = 90,  hjust = 0.5, colour = "#A41D1A") +
  annotate("text", x =  97, y = 1.5, label = "Antagonistic effect",
           size = 5, angle = 270, hjust = 0.5, colour = "#D26A18")
p1_1
