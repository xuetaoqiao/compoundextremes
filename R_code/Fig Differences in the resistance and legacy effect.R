# ---------------------------------------------
# 1. Processing "pup_tup" (wet-hot and hot-wet) Extreme Types
# ---------------------------------------------

# Load necessary libraries
library(dplyr)    # Data manipulation
library(tidyr)    # Data tidying

# Filter 'data_com_5' for rows where 'ext_type' is "pup_tup",
# then group by 'plot_ID' and 'ECOSUBCD', and calculate the mean of selected columns
data_com_pup_tup = data_com_5[data_com_5$ext_type == "pup_tup", ] %>% 
  group_by(plot_ID, ECOSUBCD) %>% 
  summarise(
    Rs_com = mean(Rs_com),
    Rl_com = mean(Rl_com),
    Le_com = mean(Le_com),
    PGR_dis_com = mean(PGR_dis_com),
    PGR_leg_com = mean(PGR_leg_com)
  )
# Display the dimensions of the summarized data
dim(data_com_pup_tup) 

# ---------------------------------------------
# 2. Processing Moisture Excess ("pup") Data
# ---------------------------------------------

# Filter 'data_sin_5' for rows where 'ext_type' is "pup",
# then group by 'plot_ID' and 'ECOSUBCD', and calculate the mean of selected columns
data_sin_pup = data_sin_5[data_sin_5$ext_type == "pup", ] %>% 
  group_by(plot_ID, ECOSUBCD) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )
# Display the dimensions of the summarized data
dim(data_sin_pup) 

# ---------------------------------------------
# 3. Processing Heat Alone ("tup") Data
# ---------------------------------------------

# Filter 'data_sin_5' for rows where 'ext_type' is "tup",
# then group by 'plot_ID' and 'ECOSUBCD', and calculate the mean of selected columns
data_sin_tup = data_sin_5[data_sin_5$ext_type == "tup", ] %>% 
  group_by(plot_ID, ECOSUBCD) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )
# Display the dimensions of the summarized data
dim(data_sin_tup) 

# ---------------------------------------------
# 4. Resistance (Rs) Analysis for "pup_tup"
# ---------------------------------------------

# Join the summarized 'com' and 'sin' data for "pup_tup" by 'plot_ID'
data_ceshi_1 = data_com_pup_tup %>%
  inner_join(data_sin_pup, by = "plot_ID") %>%    # Merge with moisture excess data
  inner_join(data_sin_tup, by = "plot_ID") %>%    # Merge with heat alone data
  mutate(
    delt_Rs_pup = log2(Rs_sin.x + 1) - log2(Rs_com + 1)  # Calculate log2 difference for Rs (pup)
  ) %>%
  mutate(
    factor_pup = case_when(
      Rs_sin.x > Rs_com ~ "small_com",  # If sin Rs > com Rs
      Rs_sin.x < Rs_com ~ "large_com",  # If sin Rs < com Rs
      TRUE ~ "equal"                     # If equal
    )
  ) %>%
  mutate(
    delt_Rs_tup = log2(Rs_sin.y + 1) - log2(Rs_com + 1)  # Calculate log2 difference for Rs (tup)
  ) %>%
  mutate(
    factor_tup = case_when(
      Rs_sin.y > Rs_com ~ "small_com",  # If sin Rs > com Rs
      Rs_sin.y < Rs_com ~ "large_com",  # If sin Rs < com Rs
      TRUE ~ "equal"                     # If equal
    )
  )
# Display the first few rows of the processed data
head(data_ceshi_1)
# Display the dimensions of the processed data
dim(data_ceshi_1)

# Perform paired t-tests to compare log2-transformed Rs_com with Rs_sin.x and Rs_sin.y
t.test(log2(data_ceshi_1$Rs_com + 1), log2(data_ceshi_1$Rs_sin.x + 1))
t.test(log2(data_ceshi_1$Rs_com + 1), log2(data_ceshi_1$Rs_sin.y + 1))

# ---------------------------------------------
# 5. Reshaping Data for ggplot (Resistance)
# ---------------------------------------------

# Reshape the data to a long format suitable for ggplot
data_long_1 <- data_ceshi_1 %>%
  dplyr::select(plot_ID, Rs_com, Rs_sin.x, Rs_sin.y) %>%  # Select relevant columns
  mutate(
    Rs_com = log2(Rs_com + 1),        # Log2 transform Rs_com
    Rs_sin.x = log2(Rs_sin.x + 1),    # Log2 transform Rs_sin.x (Moisture excess alone)
    Rs_sin.y = log2(Rs_sin.y + 1)     # Log2 transform Rs_sin.y (Heat alone)
  ) %>%
  rename(
    `Compound heat and moisture excess` = Rs_com,  # Rename for clarity
    `Moisture excess alone` = Rs_sin.x,
    `Heat alone` = Rs_sin.y
  ) %>%
  pivot_longer(
    cols = c(`Compound heat and moisture excess`, `Moisture excess alone`, `Heat alone`), 
    names_to = "Variable", 
    values_to = "Value"
  )

# Check the range of the transformed Rs values
range(data_long_1$Value)

# Load additional libraries for plotting
library(scales)  # For scaling functions

# Calculate the maximum density value for scaling the density axis
max_density_1 <- max(density(data_long_1$Value)$y)
# Calculate the maximum count from the histogram for scaling
max_count_1 <- max(ggplot_build(
  ggplot(data_long_1, aes(x = Value)) + 
    geom_histogram(binwidth = 0.1)
)$data[[1]]$count)

# Reorder the 'Variable' factor for consistent plotting
data_long_1$Variable <- factor(data_long_1$Variable, 
                               levels = c("Compound heat and moisture excess",
                                          "Heat alone",
                                          "Moisture excess alone"))

# ---------------------------------------------
# 6. Creating Resistance Plot (p1_1)
# ---------------------------------------------

# Calculate the mean values for each Variable to annotate on the plot
mean_values <- data_long_1 %>%
  group_by(Variable) %>%
  summarize(mean_value = mean(Value)) %>%
  arrange(mean_value) %>%  # Sort by mean value
  mutate(y_position = seq(187.5, 250, length.out = n()))  # Assign y positions for labels

# Create the Resistance plot
pa_1 = ggplot(data_long_1, aes(x = Value)) +
  geom_histogram(
    aes(y = ..count.., fill = Variable), 
    binwidth = 0.3, 
    alpha = 0.4, 
    position = "identity", 
    color = "white"
  ) +
  geom_density(
    aes(y = ..density.. * max_count_1 / max_density_1, color = Variable),  
    size = 0.6  # Scale density to match histogram
  ) +
  scale_x_continuous(
    breaks = seq(-3, 21, 6), 
    limits = c(-3.5, 21.5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Number of plots (n)", 
    limits = c(0, 500), 
    breaks = seq(0, 500, by = 100), 
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * max_density_1 / max_count_1, name = "Kernel density")
  ) + 
  # Add dashed vertical lines at mean values
  geom_segment(
    data = mean_values, 
    aes(x = mean_value, xend = mean_value, y = 0, yend = 500, color = Variable), 
    linetype = "dashed", 
    size = 0.6, 
    show.legend = FALSE
  ) +  
  # Add text labels for mean values
  geom_text(
    data = mean_values, 
    aes(x = mean_value, y = y_position, label = sprintf("%.3f", mean_value), color = Variable), 
    hjust = 2.5, 
    size = 5, 
    show.legend = FALSE
  ) +
  # Add annotation for p-value
  annotate("label", x = -1, y = 25,
           label = "italic(p)<0.001",
           parse = TRUE, size = 4.5, hjust = 0.5, vjust = 0.5,
           color = "black",
           fill = "white",
           label.size = NA) +
  # Define custom fill colors
  scale_fill_manual(
    name = NULL, 
    values = c("#F5C266",  "#7DA7CA","#66B966")
  ) +
  # Define custom line colors
  scale_color_manual(
    name = NULL, 
    values = c("#efa112",  "#4682b4","#219a21")
  ) +
  theme_bw() +  # Apply a clean theme
  # Add axis label with mathematical expressions
  labs(x = expression(Resistance ~ (ln(italic(R)[s] + 1))))

# Display the Resistance plot
pa_1

# ---------------------------------------------
# 7. Counting and Calculating Percentages for "pup_tup"
# ---------------------------------------------

# Count the number of plots classified as "large_com" and "small_com" for 'pup'
w_1_1 = dim(data_ceshi_1[data_ceshi_1$factor_pup == "large_com", ])  # Count of large_com
w_1_2 = dim(data_ceshi_1[data_ceshi_1$factor_pup == "small_com", ])  # Count of small_com

# Calculate the percentage of "small_com" and "large_com" plots
y_1_1 = paste("(+)", format(round(w_1_2[1] / (w_1_1[1] + w_1_2[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')
y_1_2 = paste("(-)", format(round(w_1_1[1] / (w_1_1[1] + w_1_2[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')

# Perform Wilcoxon signed-rank tests comparing Rs_com with Rs_sin.x and Rs_sin.y
wilcox.test(data_ceshi_1$Rs_com, data_ceshi_1$Rs_sin.x, paired = TRUE, alternative = "less")
wilcox.test(data_ceshi_1$Rs_com, data_ceshi_1$Rs_sin.y, paired = TRUE, alternative = "less") 

# ---------------------------------------------
# 8. Preparing Data for Boxplot (Difference in Rs)
# ---------------------------------------------

# Combine the Rs comparisons into a long format for boxplot visualization
data_1_b = data_ceshi_1 %>%   
  dplyr::select(plot_ID, delt_Rs_pup, delt_Rs_tup) %>%  # Select relevant columns
  dplyr::rename(
    'Moisture excess alone' = 'delt_Rs_pup',
    'Heat alone' = 'delt_Rs_tup'
  ) %>% 
  tidyr::gather(key = "extrem", value = "stability", -plot_ID) %>% 
  mutate(extreme_type = "Compound heat and moisture excess")  # Add a new column for extreme type

# Reorder the 'extrem' factor for consistent plotting
data_1_b$extrem <- factor(data_1_b$extrem, 
                          levels = c("Moisture excess alone","Heat alone"))

# Load additional library for factor manipulation
library(forcats)

# Create data frames for annotating the plot with percentages
left_up_a <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_1_3)  # y_1_3 not defined in this snippet; ensure it's defined appropriately
) 

left_down_a <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_1_4)  # y_1_4 not defined in this snippet; ensure it's defined appropriately
) 

right_up_a <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_1_1)  # Percentage for "Heat alone"
) 

right_down_a <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_1_2)  # Percentage for "Moisture excess alone"
) 

# Create a label for the total number of plots
w_1_34 = paste("n = ", format(round(w_1_3[1] + w_1_4[1], 0), nsmall = 0), sep = '')  # w_1_3 and w_1_4 not defined here

middle_a <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(w_1_34)
) %>%
  mutate(
    extreme_type = fct_recode(extreme_type,
                              "Compound \nheat and moisture excess" = "Compound heat and moisture excess")
  )

# Recode 'extreme_type' in 'data_1_b_2' for consistency
data_1_b_2 <- data_1_b %>%
  mutate(
    extreme_type = fct_recode(extreme_type,
                              "Compound \nheat and moisture excess" = "Compound heat and moisture excess")
  )

# ---------------------------------------------
# 9. Creating Boxplot for Rs Differences (pa_2)
# ---------------------------------------------

# Create the boxplot for Rs differences
pa_2 = ggplot(data_1_b, aes(x = extrem, y = stability)) + 
  geom_hline(yintercept = 0, color = "#efa112", size = 0.7) +  # Add a horizontal line at y=0
  geom_boxplot(
    aes(color = extrem, fill = extrem), 
    position = position_dodge(0.1),
    width = 0.2, 
    size = 0.6, 
    alpha = 0.4, 
    show.legend = FALSE, 
    outlier.shape = NA,
    fatten = 0.8
  ) +  # Add boxplots with custom aesthetics
  stat_summary(
    aes(color = extrem, fill = extrem), 
    fun.y = mean, 
    geom = "point",
    shape = 21, 
    size = 2
  ) +  # Add mean points
  scale_y_continuous(
    name = NULL,
    breaks = c(-5, 0, 5),
    limits = c(-7, 7)
  ) +
  scale_fill_manual(
    values = c(
      "Heat alone" = "#4682b4", 
      "Moisture excess alone" = "#219a21"
    )
  ) +  # Define custom fill colors
  scale_color_manual(
    values = c(
      "Heat alone" = "#4682b4", 
      "Moisture excess alone" = "#219a21"
    )
  ) +  # Define custom line colors
  labs(
    title = expression(Difference~'in'~ln(italic(R)[s] + 1)),
    x = NULL
  ) + 
  theme_bw() +  # Apply a clean theme
  # Add text annotations for percentages
  geom_text(
    data = right_up_a, 
    aes(x = 0.75, y = 5, label = lbl), 
    size = 5, 
    color = "#66B966", 
    hjust = 0.5
  ) +
  geom_text(
    data = right_down_a, 
    aes(x = 0.75, y = -5, label = lbl), 
    size = 5, 
    color = "#66B966", 
    hjust = 0.5
  ) +
  geom_text(
    data = left_up_a, 
    aes(x = 1.75, y = 5, label = lbl), 
    size = 5, 
    color = "#7DA7CA", 
    hjust = 0.5
  ) +
  geom_text(
    data = left_down_a, 
    aes(x = 1.75, y = -5, label = lbl), 
    size = 5, 
    color = "#7DA7CA", 
    hjust = 0.5
  ) +
  # Add asterisks for significance
  annotate("text", x = 1, y = 7, label = "*", size = 7, color = "grey50") +
  annotate("text", x = 2, y = 7, label = "*", size = 7, color = "grey50") +
  coord_flip()  # Flip coordinates for horizontal boxplots

# Display the Rs differences boxplot
pa_2

# ---------------------------------------------
# 10. Creating Legacy Effect Analysis Plot (p2_1)
# ---------------------------------------------

# Prepare data for Legacy Effect plot
data_Le = data_ceshi_Rs %>% 
  dplyr::select(plot_ID, Le_com, Le_sin) %>%  # Select relevant columns
  mutate(
    delt_Le = case_when(
      Le_com > Le_sin ~ "large_com",  # If com Le > sin Le
      Le_com < Le_sin ~ "small_com",  # If com Le < sin Le
      TRUE ~ "equal"                    # If equal
    )
  )

# Find maximum and minimum values for Le_com and Le_sin
max(data_Le$Le_com)
min(data_Le$Le_com)
max(data_Le$Le_sin)
min(data_Le$Le_sin)

# Count the number of plots in each category
dim(data_Le[data_Le$delt_Le == "large_com", ])
dim(data_Le[data_Le$delt_Le == "small_com", ])

# Calculate range of log2-transformed Le_com and Le_sin
range(log2(data_Le$Le_com))
range(log2(data_Le$Le_sin))

# Perform paired t-test on log2-transformed Le_com and Le_sin
t.test(log2(data_Le$Le_com + 1), log2(data_Le$Le_sin + 1))

# Calculate mean and standard deviation for Le_sin and Le_com
Le_top = paste(
  format(round(mean(log2(data_Le$Le_sin + 1)), 3), nsmall = 3), 
  " ± ", 
  format(round(sd(log2(data_Le$Le_sin + 1)), 3), nsmall = 3), 
  sep = ''
) 
Le_bottom = paste(
  format(round(mean(log2(data_Le$Le_com + 1)), 3), nsmall = 3), 
  " ± ", 
  format(round(sd(log2(data_Le$Le_com + 1)), 3), nsmall = 3), 
  sep = ''
) 

# Create formatted strings with counts and percentages for Le_com and Le_sin
Le_com_n = paste(
  Le_bottom, "\n",
  dim(data_Le[data_Le$delt_Le == "large_com", ])[1], 
  " plots (", 
  format(
    round(
      dim(data_Le[data_Le$delt_Le == "large_com", ])[1] / 
        (dim(data_Le[data_Le$delt_Le == "large_com", ])[1] + dim(data_Le[data_Le$delt_Le == "small_com", ])[1]) * 100, 
      0
    ), 
    nsmall = 0
  ),
  "%)",
  sep = ''
) 

Le_sin_n = paste(
  dim(data_Le[data_Le$delt_Le == "small_com", ])[1], 
  " plots (", 
  format(
    round(
      dim(data_Le[data_Le$delt_Le == "small_com", ])[1] / 
        (dim(data_Le[data_Le$delt_Le == "large_com", ])[1] + dim(data_Le[data_Le$delt_Le == "small_com", ])[1]) * 100, 
      0
    ), 
    nsmall = 0
  ),
  "%)", 
  "\n",
  Le_top,
  sep = ''
) 

# Define coordinates for upper and lower annotation areas (optional, not used further)
trsup_Le <- data.frame(x = c(-0.15, -0.15, 0.95), y = c(-0.15, 0.95, 0.95))
trinf_Le <- data.frame(x = c(-0.15, 0.95, 0.95), y = c(-0.15, -0.15, 0.95))

# Display the first few rows of 'data_Rs'
head(data_Rs)

# Separate 'data_Le' into small_com and large_com categories
data_Le_sin = data_Le[data_Le$delt_Le == "small_com", ]
data_Le_com = data_Le[data_Le$delt_Le == "large_com", ]

# Load additional libraries for plotting
library(viridis)  # Color scales
library(metR)      # Meteorological data analysis (if needed)

# Create the Legacy Effect plot
p2_1 = ggplot(data_Le, aes(x = log2(Le_com + 1), y = log2(Le_sin + 1))) +
  # Add annotation for p-value
  annotate("label", x = -0.1, y = 0.895,
           label = "italic(p)<0.001",
           parse = TRUE, size = 4.5, hjust = 0, vjust = 1,
           color = "black",
           fill = "white",
           label.size = NA) +
  # Create a 2D bin plot
  geom_bin2d(bins = 145, alpha = 0.98) +
  # Define color gradient for the bins
  scale_fill_gradientn(
    colors = c('#3235A4', '#21B1A6', '#FEF70F'), 
    name = "Number of\nplots (n)",
    breaks = c(1, 250, 500),
    guide = guide_colorbar(
      draw.ulim = FALSE, 
      draw.llim = FALSE
    )
  ) +
  # Customize y-axis with mathematical expressions
  scale_y_continuous(
    name = expression(atop(Legacy~effect~(ln(italic(L)[e] + 1)), paste(under~individual~extremes))),
    breaks = seq(-0.1, 0.9, 0.2), 
    limits = c(-0.15, 0.95),
    expand = c(0, 0)
  ) +
  # Customize x-axis with mathematical expressions
  scale_x_continuous(
    name = expression(atop(Legacy~effect~(ln(italic(L)[e] + 1)), paste(under~compound~extremes))),
    breaks = seq(-0.1, 0.9, 0.2), 
    limits = c(-0.15, 0.95),
    expand = c(0, 0)
  ) +
  # Add a diagonal dashed line for reference
  geom_abline(
    intercept = 0, 
    color = "black", 
    linetype = "dashed", 
    size = 0.4, 
    slope = 1, 
    alpha = 0.8
  ) +
  theme_bw() +  # Apply a clean theme
  # Add text annotations for counts and percentages
  annotate("text", x = 0.4, y = 0.895, label = Le_sin_n, size = 4.5, color = "black", vjust = 1) + 
  annotate("text", x = 0.4, y = -0.095, label = Le_com_n, size = 4.5, color = "black", vjust = 0) 

# Display the Legacy Effect plot
p2_1

# ---------------------------------------------
# 11. Creating Legacy Effect Boxplot (pc_1)
# ---------------------------------------------

# Calculate the mean values for each Variable to annotate on the plot
mean_values_3 <- data_long_3 %>%
  group_by(Variable) %>%
  summarize(mean_value = mean(Value)) %>%
  arrange(mean_value) %>%  # Sort by mean value
  mutate(y_position = seq(150, 200, length.out = n()))  # Assign y positions for labels

# Create the Legacy Effect plot for "pdown_tup"
pb_1 = ggplot(data_long_3, aes(x = Value)) +
  geom_histogram(
    aes(y = ..count.., fill = Variable), 
    binwidth = 0.01, 
    alpha = 0.4, 
    position = "identity", 
    color = "white"
  ) +
  geom_density(
    aes(y = ..density.. * max_count_3 / max_density_3, color = Variable),  
    size = 0.6  # Scale density to match histogram
  ) +
  scale_x_continuous(
    breaks = seq(-3, 21, 6), 
    limits = c(-3.5, 21.5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Number of plots (n)", 
    limits = c(0, 400), 
    breaks = seq(0, 400, by = 100), 
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * max_density_3 / max_count_3, name = "Kernel density")
  ) + 
  # Add dashed vertical lines at mean values
  geom_segment(
    data = mean_values_3, 
    aes(x = mean_value, xend = mean_value, y = 0, yend = 400, color = Variable), 
    linetype = "dashed", 
    size = 0.6, 
    show.legend = FALSE
  ) +  
  # Add text labels for mean values
  geom_text(
    data = mean_values_3, 
    aes(x = mean_value, y = y_position, label = sprintf("%.3f", mean_value), color = Variable), 
    hjust = 3, 
    size = 5, 
    show.legend = FALSE
  ) +
  # Add annotation for p-value
  annotate("label", x = -1, y = 60,
           label = "italic(p)<0.001",
           parse = TRUE, size = 4.5, hjust = 0.5, vjust = 0.5,
           color = "black",
           fill = "white",
           label.size = NA) +
  # Define custom fill colors
  scale_fill_manual(
    name = NULL, 
    values = c("#F5C266",  "#7DA7CA","#66B966")
  ) +
  # Define custom line colors
  scale_color_manual(
    name = NULL, 
    values = c("#efa112",  "#4682b4","#219a21")
  ) +
  theme_bw() +  # Apply a clean theme
  # Add axis label with mathematical expressions
  labs(x = expression(Legacy ~ effect ~ (ln(italic(L)[e] + 1))))

# Display the Legacy Effect plot for "pdown_tup"
pb_1

# ---------------------------------------------
# 12. Calculating and Annotating Percentages for "pdown_tup"
# ---------------------------------------------

# Count the number of plots classified as "large_com" and "small_com" for 'pdown'
w_5_1 = dim(data_ceshi_5[data_ceshi_5$factor_pdown == "large_com", ])  # Count of large_com
w_5_2 = dim(data_ceshi_5[data_ceshi_5$factor_pdown == "small_com", ])  # Count of small_com
w_5_3 = dim(data_ceshi_5[data_ceshi_5$factor_tup == "large_com", ])    # Count of large_com for tup
w_5_4 = dim(data_ceshi_5[data_ceshi_5$factor_tup == "small_com", ])    # Count of small_com for tup

# Calculate the percentage of "small_com" and "large_com" for 'pdown' and 'tup'
y_5_1 = paste("(+)", format(round(w_5_2[1] / (w_5_1[1] + w_5_2[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')
y_5_2 = paste("(-)", format(round(w_5_1[1] / (w_5_1[1] + w_5_2[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')
y_5_3 = paste("(+)", format(round(w_5_4[1] / (w_5_3[1] + w_5_4[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')
y_5_4 = paste("(-)", format(round(w_5_3[1] / (w_5_3[1] + w_5_4[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')

# Perform Wilcoxon signed-rank tests comparing Le_com with Le_sin.x and Le_sin.y
wilcox.test(data_ceshi_5$Le_com, data_ceshi_5$Le_sin.x, paired = TRUE, alternative = "greater")  # Moisture excess alone
wilcox.test(data_ceshi_5$Le_com, data_ceshi_5$Le_sin.y, paired = TRUE, alternative = "greater")  # Heat alone

# ---------------------------------------------
# 13. Preparing Data for Boxplot (Difference in Le)
# ---------------------------------------------

# Combine the Le comparisons into a long format for boxplot visualization
data_5_b = data_ceshi_5 %>% 
  dplyr::select(plot_ID, delt_Le_pup, delt_Le_tup) %>% 
  dplyr::rename(
    'Moisture excess alone' = 'delt_Le_pup', 
    'Heat alone' = 'delt_Le_tup'
  ) %>%          
  tidyr::gather(key = "extrem", value = "stability", -plot_ID) %>% 
  mutate(extreme_type = "Compound heat and moisture excess")  # Add a new column for extreme type

# Reorder the 'extrem' factor for consistent plotting
data_5_b$extrem <- factor(data_5_b$extrem, 
                          levels = c("Moisture excess alone","Heat alone"))

# Load additional library for factor manipulation
library(forcats)

# Create data frames for annotating the plot with percentages
left_up_c <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_5_3)  # Percentage for "Heat alone"
) 

left_down_c <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_5_4)  # Percentage for "Moisture excess alone"
) 

right_up_c <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_5_1)  # Percentage for "Heat alone"
) 

right_down_d <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(y_5_2)  # Percentage for "Moisture excess alone"
) 

# Create a label for the total number of plots
w_5_34 = paste("n = ", format(round(w_5_3[1] + w_5_4[1], 0), nsmall = 0), sep = '') 

middle_c <- data.frame(
  extreme_type = c("Compound heat and moisture excess"), 
  lbl = c(w_5_34)
)

# Recode 'extreme_type' in 'data_5_b' for consistency
data_5_b$extrem <- factor(data_5_b$extrem, 
                          levels = c("Drought alone","Heat alone"))

# ---------------------------------------------
# 14. Creating Boxplot for Le Differences (pc_2)
# ---------------------------------------------

# Create the boxplot for Le differences
pc_2 = ggplot(data_5_b, aes(x = extrem, y = stability)) + 
  geom_hline(yintercept = 0, color = "#efa112", size = 0.7) +  # Add a horizontal line at y=0
  geom_boxplot(
    aes(color = extrem, fill = extrem), 
    position = position_dodge(0.1),
    width = 0.2, 
    size = 0.6, 
    alpha = 0.4, 
    show.legend = FALSE, 
    outlier.shape = NA,
    fatten = 0.8
  ) +  # Add boxplots with custom aesthetics
  stat_summary(
    aes(color = extrem, fill = extrem), 
    fun.y = mean, 
    geom = "point",
    shape = 21, 
    size = 2
  ) +  # Add mean points
  scale_y_continuous(
    name = NULL,
    breaks = c(-0.05, 0.05),
    limits = c(-0.1, 0.1)
  ) +
  scale_fill_manual(
    values = c(
      "Heat alone" = "#4682b4", 
      "Moisture excess alone" = "#219a21"
    )
  ) +  # Define custom fill colors
  scale_color_manual(
    values = c(
      "Heat alone" = "#4682b4", 
      "Moisture excess alone" = "#219a21"
    )
  ) +  # Define custom line colors
  labs(
    title = expression(Difference~'in'~ln(italic(L)[e] + 1)),
    x = NULL
  ) + 
  theme_bw() +  # Apply a clean theme
  # Add text annotations for percentages
  geom_text(
    data = right_up_d, 
    aes(x = 0.75, y = 0.07, label = lbl), 
    size = 5, 
    color = "#66B966", 
    hjust = 0.5
  ) +
  geom_text(
    data = right_down_d, 
    aes(x = 0.75, y = -0.07, label = lbl), 
    size = 5, 
    color = "#66B966", 
    hjust = 0.5
  ) +
  geom_text(
    data = left_up_d, 
    aes(x = 1.75, y = 0.07, label = lbl), 
    size = 5, 
    color = "#7DA7CA", 
    hjust = 0.5
  ) +
  geom_text(
    data = left_down_d, 
    aes(x = 1.75, y = -0.07, label = lbl), 
    size = 5, 
    color = "#7DA7CA", 
    hjust = 0.5
  ) +
  # Add asterisks for significance
  annotate("text", x = 1, y = 0.1, label = "*", size = 7, color = "grey50") +
  annotate("text", x = 2, y = 0.1, label = "*", size = 7, color = "grey50") +
  coord_flip()  # Flip coordinates for horizontal boxplots

# Display the Le differences boxplot
pc_2

# ---------------------------------------------
# 15. Processing "pdown_tup" (dry-hot and hot-dry) Extreme Types
# ---------------------------------------------

# Repeat similar processing steps for "pdown_tup" as done for "pup_tup"

# Filter 'data_com_5' for rows where 'ext_type' is "pdown_tup",
# then group by 'plot_ID' and 'ECOSUBCD', and calculate the mean of selected columns
data_com_pdown_tup = data_com_5[data_com_5$ext_type == "pdown_tup", ] %>% 
  group_by(plot_ID, ECOSUBCD) %>% 
  summarise(
    Rs_com = mean(Rs_com),
    Rl_com = mean(Rl_com),
    Le_com = mean(Le_com),
    PGR_dis_com = mean(PGR_dis_com),
    PGR_leg_com = mean(PGR_leg_com)
  )

# Display the dimensions of the summarized data
dim(data_com_pdown_tup)  # Expected output: 22889

# ---------------------------------------------
# 16. Processing Moisture Excess ("pdown") Data
# ---------------------------------------------

# Filter 'data_sin_5' for rows where 'ext_type' is "pdown",
# then group by 'plot_ID' and 'ECOSUBCD', and calculate the mean of selected columns
data_sin_pdown = data_sin_5[data_sin_5$ext_type == "pdown", ] %>% 
  group_by(plot_ID, ECOSUBCD) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )

# ---------------------------------------------
# 17. Processing Heat Alone ("tup") Data for "pdown_tup"
# ---------------------------------------------

# Filter 'data_sin_5' for rows where 'ext_type' is "tup",
# then group by 'plot_ID' and 'ECOSUBCD', and calculate the mean of selected columns
data_sin_tup = data_sin_5[data_sin_5$ext_type == "tup", ] %>% 
  group_by(plot_ID, ECOSUBCD) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )

# ---------------------------------------------
# 18. Resistance (Rs) Analysis for "pdown_tup"
# ---------------------------------------------

# Join the summarized 'com' and 'sin' data for "pdown_tup" by 'plot_ID'
data_ceshi_7 = data_com_pdown_tup %>%
  inner_join(data_sin_pdown, by = "plot_ID") %>%    # Merge with drought alone data
  inner_join(data_sin_tup, by = "plot_ID") %>%      # Merge with heat alone data
  mutate(
    delt_Le_pdown = log2(Le_sin.x + 1) - log2(Le_com + 1)  # Calculate log2 difference for Le (pdown)
  ) %>%
  mutate(
    factor_pdown = case_when(
      Le_sin.x > Le_com ~ "small_com",  # If sin Le > com Le
      Le_sin.x < Le_com ~ "large_com",  # If sin Le < com Le
      TRUE ~ "equal"                     # If equal
    )
  ) %>%
  mutate(
    delt_Le_tup = log2(Le_sin.y + 1) - log2(Le_com + 1)  # Calculate log2 difference for Le (tup)
  ) %>%
  mutate(
    factor_tup = case_when(
      Le_sin.y > Le_com ~ "small_com",  # If sin Le > com Le
      Le_sin.y < Le_com ~ "large_com",  # If sin Le < com Le
      TRUE ~ "equal"                     # If equal
    )
  )
# Display the first few rows of the processed data
head(data_ceshi_7)
# Display the dimensions of the processed data
dim(data_ceshi_7)

# Perform paired t-tests to compare Le_com with Le_sin.x and Le_sin.y
t.test(log2(data_ceshi_7$Le_com + 1), log2(data_ceshi_7$Le_sin.x + 1))
t.test(log2(data_ceshi_7$Le_com + 1), log2(data_ceshi_7$Le_sin.y + 1))

# ---------------------------------------------
# 19. Reshaping Data for ggplot (Legacy Effect)
# ---------------------------------------------

# Reshape the data to a long format suitable for ggplot
data_long_7 <- data_ceshi_7 %>%
  dplyr::select(plot_ID, Le_com, Le_sin.x, Le_sin.y) %>%  # Select relevant columns
  mutate(
    Le_com = log2(Le_com + 1),      # Log2 transform Le_com
    Le_sin.x = log2(Le_sin.x + 1),  # Log2 transform Le_sin.x (Drought alone)
    Le_sin.y = log2(Le_sin.y + 1)   # Log2 transform Le_sin.y (Heat alone)
  ) %>%
  rename(
    `Compound heat and drought` = Le_com,  # Rename for clarity
    `Drought alone` = Le_sin.x,
    `Heat alone` = Le_sin.y
  ) %>%
  pivot_longer(
    cols = c(`Compound heat and drought`, `Drought alone`, `Heat alone`), 
    names_to = "Variable", 
    values_to = "Value"
  )

# Check the range of the transformed Le values
range(data_long_7$Value)

# Load additional libraries for plotting
library(scales)  # For scaling functions

# Calculate the maximum count from the histogram for scaling
max_count_7 <- max(ggplot_build(
  ggplot(data_long_7, aes(x = Value)) + 
    geom_histogram(aes(y = ..count.., fill = Variable), binwidth = 0.01, position = "identity")
)$data[[1]]$count)

# Calculate the maximum density value for scaling the density axis
max_density_7 <- max(density(data_long_7$Value)$y)

# Reorder the 'Variable' factor for consistent plotting
data_long_7$Variable <- factor(data_long_7$Variable, 
                               levels = c("Compound heat and drought",
                                          "Heat alone",
                                          "Drought alone"))

# Calculate the mean values for each Variable to annotate on the plot
mean_values_7 <- data_long_7 %>%
  group_by(Variable) %>%
  summarize(mean_value = mean(Value)) %>%
  arrange(mean_value) %>%  # Sort by mean value
  mutate(y_position = seq(225, 300, length.out = n()))  # Assign y positions for labels

# ---------------------------------------------
# 20. Creating Legacy Effect Plot for "pdown_tup" (pd_1)
# ---------------------------------------------

# Create the Legacy Effect plot for "pdown_tup"
pd_1 = ggplot(data_long_7, aes(x = Value)) +
  geom_histogram(
    aes(y = ..count.., fill = Variable), 
    binwidth = 0.01, 
    alpha = 0.4, 
    position = "identity", 
    color = "white"
  ) +
  geom_density(
    aes(y = ..density.. * max_count_7 / max_density_7, color = Variable),  
    size = 0.6  # Scale density to match histogram
  ) +
  scale_x_continuous(
    breaks = seq(-0.1, 0.5, 0.2), 
    limits = c(-0.15, 0.55),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Number of plots (n)", 
    limits = c(0, 600), 
    breaks = seq(0, 600, by = 200), 
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * max_density_7 / max_count_7, name = "Kernel density")
  ) + 
  # Add dashed vertical lines at mean values
  geom_segment(
    data = mean_values_7, 
    aes(x = mean_value, xend = mean_value, y = 0, yend = 600, color = Variable), 
    linetype = "dashed", 
    size = 0.6, 
    show.legend = FALSE
  ) +  
  # Add text labels for mean values
  geom_text(
    data = mean_values_7, 
    aes(x = mean_value, y = y_position, label = round(mean_value, 3), color = Variable), 
    hjust = 3, 
    size = 5, 
    show.legend = FALSE
  ) +
  # Add annotation for p-value
  annotate("label", x = -0.09, y = 30,
           label = "italic(p)<0.001",
           parse = TRUE, size = 4.5, hjust = 0.5, vjust = 0.5,
           color = "black",
           fill = "white",
           label.size = NA) +
  # Define custom fill colors
  scale_fill_manual(
    name = NULL, 
    values = c("#F5C266",  "#7DA7CA","#66B966")
  ) +
  # Define custom line colors
  scale_color_manual(
    name = NULL, 
    values = c("#efa112",  "#4682b4","#219a21")
  ) +
  theme_bw() +  # Apply a clean theme
  # Add axis label with mathematical expressions
  labs(x = expression(Legacy ~ effect ~ (ln(italic(L)[e] + 1))))

# Display the Legacy Effect plot for "pdown_tup"
pd_1

# ---------------------------------------------
# 21. Counting and Calculating Percentages for "pdown_tup"
# ---------------------------------------------

# Count the number of plots classified as "large_com" and "small_com" for 'pdown'
w_7_1 = dim(data_ceshi_7[data_ceshi_7$factor_pdown == "large_com", ])  # Count of large_com
w_7_2 = dim(data_ceshi_7[data_ceshi_7$factor_pdown == "small_com", ])  # Count of small_com
w_7_3 = dim(data_ceshi_7[data_ceshi_7$factor_tup == "large_com", ])    # Count of large_com for tup
w_7_4 = dim(data_ceshi_7[data_ceshi_7$factor_tup == "small_com", ])    # Count of small_com for tup

# Calculate the percentage of "small_com" and "large_com" for 'pdown' and 'tup'
y_7_1 = paste("(+)", format(round(w_7_2[1] / (w_7_1[1] + w_7_2[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')
y_7_2 = paste("(-)", format(round(w_7_1[1] / (w_7_1[1] + w_7_2[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')
y_7_3 = paste("(+)", format(round(w_7_4[1] / (w_7_3[1] + w_7_4[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')
y_7_4 = paste("(-)", format(round(w_7_3[1] / (w_7_3[1] + w_7_4[1]) * 100, 0), nsmall = 0), sep = ' ') %>%
  paste("%", sep = '')

# Perform Wilcoxon signed-rank tests comparing Le_com with Le_sin.x and Le_sin.y
wilcox.test(data_ceshi_7$Le_com, data_ceshi_7$Le_sin.x, paired = TRUE, alternative = "greater")  # Drought alone
wilcox.test(data_ceshi_7$Le_com, data_ceshi_7$Le_sin.y, paired = TRUE, alternative = "greater")  # Heat alone

# ---------------------------------------------
# 22. Preparing Data for Boxplot (Difference in Le for "pdown_tup")
# ---------------------------------------------

# Combine the Le comparisons into a long format for boxplot visualization
data_7_b = data_ceshi_7 %>% 
  dplyr::select(plot_ID, delt_Le_pdown, delt_Le_tup) %>% 
  dplyr::rename(
    'Drought alone' = 'delt_Le_pdown',
    'Heat alone' = 'delt_Le_tup'
  ) %>% 
  tidyr::gather(key = "extrem", value = "stability", -plot_ID) %>% 
  mutate(extreme_type = "Compound heat and drought")  # Add a new column for extreme type

# Reorder the 'extrem' factor for consistent plotting
data_7_b$extrem <- factor(data_7_b$extrem, 
                          levels = c("Drought alone","Heat alone"))

# Create data frames for annotating the plot with percentages
left_up_d <- data.frame(
  extreme_type = c("Compound heat and drought"), 
  lbl = c(y_7_3)  # Percentage for "Heat alone"
) 

left_down_d <- data.frame(
  extreme_type = c("Compound heat and drought"), 
  lbl = c(y_7_4)  # Percentage for "Drought alone"
) 

right_up_d <- data.frame(
  extreme_type = c("Compound heat and drought"), 
  lbl = c(y_7_1)  # Percentage for "Heat alone"
) 

right_down_d <- data.frame(
  extreme_type = c("Compound heat and drought"), 
  lbl = c(y_7_2)  # Percentage for "Drought alone"
) 

# Create a label for the total number of plots
w_7_34 = paste("n = ", format(round(w_7_3[1] + w_7_4[1], 0), nsmall = 0), sep = '') 

middle_d <- data.frame(
  extreme_type = c("Compound heat and drought"), 
  lbl = c(w_7_34)
)

# Recode 'extreme_type' in 'data_7_b' for consistency
data_7_b$extrem <- factor(data_7_b$extrem, 
                          levels = c("Drought alone","Heat alone"))

# ---------------------------------------------
# 23. Creating Boxplot for Le Differences (pd_2)
# ---------------------------------------------

# Create the boxplot for Le differences
pd_2 = ggplot(data_7_b, aes(x = extrem, y = stability)) + 
  geom_hline(yintercept = 0, color = "#efa112", size = 0.7) +  # Add a horizontal line at y=0
  geom_boxplot(
    aes(color = extrem, fill = extrem), 
    position = position_dodge(0.1),
    width = 0.2, 
    size = 0.6, 
    alpha = 0.4, 
    show.legend = FALSE, 
    outlier.shape = NA,
    fatten = 0.8
  ) +  # Add boxplots with custom aesthetics
  stat_summary(
    aes(color = extrem, fill = extrem), 
    fun.y = mean, 
    geom = "point",
    shape = 21, 
    size = 2
  ) +  # Add mean points
  scale_y_continuous(
    name = NULL,
    breaks = c(-0.05, 0.05),
    limits = c(-0.1, 0.1)
  ) +
  scale_fill_manual(
    values = c(
      "Heat alone" = "#4682b4", 
      "Drought alone" = "#219a21"
    )
  ) +  # Define custom fill colors
  scale_color_manual(
    values = c(
      "Heat alone" = "#4682b4", 
      "Drought alone" = "#219a21"
    )
  ) +  # Define custom line colors
  labs(
    title = expression(Difference~'in'~ln(italic(L)[e] + 1)),
    x = NULL
  ) + 
  theme_bw() +  # Apply a clean theme
  # Add text annotations for percentages
  geom_text(
    data = right_up_d, 
    aes(x = 0.75, y = 0.07, label = lbl), 
    size = 5, 
    color = "#66B966", 
    hjust = 0.5
  ) +
  geom_text(
    data = right_down_d, 
    aes(x = 0.75, y = -0.07, label = lbl), 
    size = 5, 
    color = "#66B966", 
    hjust = 0.5
  ) +
  geom_text(
    data = left_up_d, 
    aes(x = 1.75, y = 0.07, label = lbl), 
    size = 5, 
    color = "#7DA7CA", 
    hjust = 0.5
  ) +
  geom_text(
    data = left_down_d, 
    aes(x = 1.75, y = -0.07, label = lbl), 
    size = 5, 
    color = "#7DA7CA", 
    hjust = 0.5
  ) +
  # Add asterisks for significance
  annotate("text", x = 1, y = 0.1, label = "*", size = 7, color = "grey50") +
  annotate("text", x = 2, y = 0.1, label = "*", size = 7, color = "grey50") +
  coord_flip()  # Flip coordinates for horizontal boxplots

# Display the Legacy Effect boxplot for "pdown_tup"
pd_2

# ---------------------------------------------
# 24. Creating Combined Plots (Optional)
# ---------------------------------------------

# You can combine the plots (pa_1, pa_2, p2_1, pb_1, pd_1, pc_1, pd_2) using grid.arrange or cowplot if desired.
# Example using cowplot:
# library(cowplot)
# combined_plot = plot_grid(pa_1, pa_2, p2_1, pb_1, pd_1, pc_1, pd_2, labels = "AUTO")
# print(combined_plot)
