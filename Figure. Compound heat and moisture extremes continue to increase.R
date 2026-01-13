# ==========================================================
# Figure 1 (c)(d): Compound heat–moisture extremes
# Scenarios: SSP1-2.6 / SSP2-4.5 / SSP5-8.5
#
# (c) Decadal means of the annual proportion (%) of plots
#     experiencing compound extremes
# (d) Decadal frequency of compound extremes per plot
#     (mean ± SD across plots)
# ==========================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1) Read data -----------------------------------------------------------
# Each row represents one compound extreme event
# Columns assumed: year, pltID (plot ID)
files <- c(
  "SSP1-2.6" = "D:/1Postdoctoral/paper/paper1/20251018_计算复合极端气候/数据/com_126_2021_2100_20251221.csv",
  "SSP2-4.5" = "D:/1Postdoctoral/paper/paper1/20251018_计算复合极端气候/数据/com_245_2021_2100_20251221.csv",
  "SSP5-8.5" = "D:/1Postdoctoral/paper/paper1/20251018_计算复合极端气候/数据/com_585_2021_2100_20251221.csv"
)

dat_list <- lapply(names(files), function(spp){
  read.csv(files[[spp]], header = TRUE) %>%
    mutate(spp_type = spp)
})
dat <- bind_rows(dat_list)

# Total number of plots
# Assumption: the set of plots is identical across SSPs
total_plotnum <- dat %>%
  filter(spp_type == "SSP5-8.5") %>%
  pull(pltID) %>%
  unique() %>%
  length()

# ---- 2) Common definitions for decadal grouping -----------------------------
dec_breaks  <- c(2021,2031,2041,2051,2061,2071,2081,2091,2101)
dec_labels  <- c("2021–2030","2031–2040","2041–2050","2051–2060",
                 "2061–2070","2071–2080","2081–2090","2091–2100")
dec_centers <- c(2025.5,2035.5,2045.5,2055.5,
                 2065.5,2075.5,2085.5,2095.5)

# Grey background bands highlighting alternating decades
grey_bands <- data.frame(
  xmin = c(2021, 2041, 2061, 2081),
  xmax = c(2030, 2050, 2070, 2090)
)

add_grey_bands <- function(ymin, ymax){
  geom_rect(
    data = grey_bands,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "lightgrey",
    alpha = 0.20
  )
}


# Consistent colour palette for SSPs
cols <- c(
  "SSP1-2.6" = "#8DB3D2",
  "SSP2-4.5" = "#3D5B92",
  "SSP5-8.5" = "#C74A45"
)

# =====================================================================
# (c) Decadal mean proportion of plots experiencing compound extremes
# =====================================================================
# Step 1: For each year and scenario, calculate the proportion of plots
#         experiencing at least one compound extreme
#         (n_distinct(pltID) avoids double counting multiple events
#          within the same plot and year)
# Step 2: Aggregate annual proportions to decadal means
c_dec <- dat %>%
  group_by(spp_type, year) %>%
  summarise(
    prop = n_distinct(pltID) / total_plotnum * 100,
    .groups = "drop"
  ) %>%
  mutate(dec_id = (year - 2021) %/% 10) %>%
  group_by(spp_type, dec_id) %>%
  summarise(
    year      = 2025.5 + dec_id * 10,  # centre of each decade
    mean_prop = mean(prop),
    sd_prop   = sd(prop),
    .groups   = "drop"
  ) %>%
  mutate(spp_type = factor(spp_type, levels = names(files)))

# Plot (c)
p_c <- ggplot(c_dec, aes(year, mean_prop, colour = spp_type)) +
  geom_line(linewidth = 0.6) +
  add_grey_bands(ymin = -2, ymax = 50) +
  scale_x_continuous(
    limits = c(2020, 2101),
    breaks = dec_centers,
    labels = dec_labels,
    expand = c(0.03, 0.03)
  ) +
  scale_y_continuous(
    "Proportion of\ncompound extremes extent (% yr⁻¹)",
    limits = c(-4, 62),
    breaks = c(0, 20, 40, 60),
    expand = c(0, 0)
  ) +
  scale_colour_manual(values = cols) +
  annotate(
    "text",
    x = 2061,
    y = 55,
    label = "Future projections\n2021–2100",
    alpha = 0.6,
    size = 4
  )

p_c

# =====================================================================
# (d) Decadal frequency of compound extremes per plot
# =====================================================================
# Step 1: Count the number of compound extreme events per plot
#         within each decadal period
# Step 2: Calculate the mean and standard deviation of frequencies
#         across all plots for each scenario and decade
d_dec <- dat %>%
  mutate(
    period = cut(
      year,
      breaks = dec_breaks,
      labels = c("2030","2040","2050","2060",
                 "2070","2080","2090","2100"),
      right = FALSE
    )
  ) %>%
  group_by(spp_type, pltID, period) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(spp_type, period) %>%
  summarise(
    mean = mean(freq),
    sd   = sd(freq),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.numeric(as.character(period)) - 4.5,  # decadal centre
    spp_type = factor(spp_type, levels = names(files))
  )

# Plot (d)
p_d <- ggplot(d_dec, aes(year, mean, colour = spp_type, group = spp_type)) +
  add_grey_bands(ymin = -0.5, ymax = 7) +
  geom_line(
    linetype = "dashed",
    alpha = 0.75,
    position = position_dodge(width = 8)
  ) +
  geom_point(
    aes(fill = spp_type),
    shape = 21,
    size = 3,
    colour = "black",
    position = position_dodge(width = 8),
    alpha = 0.9
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 4,
    colour = "black",
    alpha = 0.4,
    position = position_dodge(width = 8),
    show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = c(2020, 2101),
    breaks = dec_centers,
    labels = dec_labels,
    expand = c(0.03, 0.03)
  ) +
  scale_y_continuous(
    expression(atop(Frequency~of,
                    paste(compound~extremes~(10~italic(yr)^-1)))),
    limits = c(-1, 8.6),
    breaks = c(0, 2, 4, 6, 8),
    expand = c(0, 0)
  ) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  annotate(
    "text",
    x = 2061,
    y = 7.8,
    label = "Future projections\n2021–2100",
    alpha = 0.6,
    size = 4
  )

p_d
