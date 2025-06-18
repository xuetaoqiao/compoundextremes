# Filter data_com_all for ext_type "pup_tup" and calculate mean values for each plot_ID
data_com_pup_tup = data_com_all[data_com_all$ext_type == "pup_tup",] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_com = mean(Rs_com),
    Rl_com = mean(Rl_com),
    Le_com = mean(Le_com),
    PGR_dis_com = mean(PGR_dis_com),
    PGR_leg_com = mean(PGR_leg_com)
  )
# Display the dimensions of the summarized data
dim(data_com_pup_tup) 

# Filter data_sin_all for ext_type "pup" and calculate mean values for each plot_ID
data_sin_pup = data_sin_all[data_sin_all$ext_type == "pup",] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )
# Display the dimensions of the summarized data
dim(data_sin_pup) 

# Filter data_sin_all for ext_type "tup" and calculate mean values for each plot_ID
data_sin_tup = data_sin_all[data_sin_all$ext_type == "tup",] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )
# Display the dimensions of the summarized data
dim(data_sin_tup) 

# Rs Analysis for "pup_tup"
# Join com and sin data for "pup_tup" and calculate differences and factors
data_ceshi_1 = data_com_pup_tup %>%
  inner_join(data_sin_pup, by = "plot_ID") %>%  # Merge datasets by plot_ID
  mutate(
    delt_Rs_pup = log2(Rs_sin) - log2(Rs_com)  # Calculate log2 difference for Rs
  ) %>%
  mutate(
    factor_pup = case_when(
      Rs_sin > Rs_com ~ "small_com",  # If sin Rs > com Rs
      Rs_sin < Rs_com ~ "large_com",  # If sin Rs < com Rs
      TRUE ~ "equal"  # If equal
    )
  ) %>%
  mutate(
    Le_factor_pup = case_when(
      Le_sin > Le_com ~ "small_com",  # If sin Le > com Le
      Le_sin < Le_com ~ "large_com",  # If sin Le < com Le
      TRUE ~ "equal"  # If equal
    )
  )
# Display the dimensions of the processed data
dim(data_ceshi_1)

# Wet Analysis
# Count the number of plots with "large_com" and "small_com" factors
w_1_1 = dim(data_ceshi_1[data_ceshi_1$factor_pup == "large_com",])  # Large_com count
w_1_2 = dim(data_ceshi_1[data_ceshi_1$factor_pup == "small_com",])  # Small_com count

# Calculate percentage of small_com and large_com
y_1_1 = paste("(+)", format(round(w_1_2[1] / (w_1_1[1] + w_1_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
y_1_2 = paste("(-)", format(round(w_1_1[1] / (w_1_1[1] + w_1_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
# Display the calculated percentages
y_1_1
y_1_2

# Perform Wilcoxon signed-rank test for Rs_com vs Rs_sin
wilcox.test(data_ceshi_1$Rs_com, data_ceshi_1$Rs_sin, paired = TRUE, alternative = "less")

# Additional Analysis

# Join com and sin data for "tup" and calculate differences and factors
data_ceshi_2 = data_com_pup_tup %>%
  inner_join(data_sin_tup, by = "plot_ID") %>%  # Merge datasets by plot_ID
  mutate(
    factor_tup = case_when(
      Rs_sin > Rs_com ~ "small_com",  # If sin Rs > com Rs
      Rs_sin < Rs_com ~ "large_com",  # If sin Rs < com Rs
      TRUE ~ "equal"  # If equal
    )
  ) %>%
  mutate(
    Le_factor_tup = case_when(
      Le_sin > Le_com ~ "small_com",  # If sin Le > com Le
      Le_sin < Le_com ~ "large_com",  # If sin Le < com Le
      TRUE ~ "equal"  # If equal
    )
  )
# Display the dimensions of the processed data
dim(data_ceshi_2)

# Hot Analysis
# Count the number of plots with "large_com" and "small_com" factors
w_2_1 = dim(data_ceshi_2[data_ceshi_2$factor_tup == "large_com",])  # Large_com count
w_2_2 = dim(data_ceshi_2[data_ceshi_2$factor_tup == "small_com",])  # Small_com count

# Calculate percentage of small_com and large_com
y_2_1 = paste("(+)", format(round(w_2_2[1] / (w_2_1[1] + w_2_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
y_2_2 = paste("(-)", format(round(w_2_1[1] / (w_2_1[1] + w_2_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
# Display the calculated percentages
y_2_1
y_2_2

# Perform Wilcoxon signed-rank test for Rs_com vs Rs_sin
wilcox.test(data_ceshi_2$Rs_com, data_ceshi_2$Rs_sin, paired = TRUE, alternative = "less") 

# 3. Processing "pdown_tup" (dry-hot and hot-dry) Extreme Types

# Filter data_com_all for ext_type "pdown_tup" and calculate mean values for each plot_ID
data_com_pdown_tup = data_com_all[data_com_all$ext_type == "pdown_tup",] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_com = mean(Rs_com),
    Rl_com = mean(Rl_com),
    Le_com = mean(Le_com),
    PGR_dis_com = mean(PGR_dis_com),
    PGR_leg_com = mean(PGR_leg_com)
  )

# Display the dimensions of the summarized data
dim(data_com_pdown_tup) 

# Filter data_sin_all for ext_type "pdown" and calculate mean values for each plot_ID
data_sin_pdown = data_sin_all[data_sin_all$ext_type == "pdown",] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )

# Filter data_sin_all for ext_type "tup" and calculate mean values for each plot_ID
data_sin_tup = data_sin_all[data_sin_all$ext_type == "tup",] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )

# Rs Analysis for "pdown_tup"
# Join com and sin data for "pdown_tup" and calculate differences and factors
data_ceshi_3 = data_com_pdown_tup %>%
  inner_join(data_sin_pdown, by = "plot_ID") %>%  # Merge datasets by plot_ID
  mutate(
    delt_Rs_pdown = log2(Rs_sin) - log2(Rs_com)  # Calculate log2 difference for Rs
  ) %>%
  mutate(
    factor_pdown = case_when(
      Rs_sin > Rs_com ~ "small_com",  # If sin Rs > com Rs
      Rs_sin < Rs_com ~ "large_com",  # If sin Rs < com Rs
      TRUE ~ "equal"  # If equal
    )
  ) %>%
  mutate(
    Le_factor_pdown = case_when(
      Le_sin > Le_com ~ "small_com",  # If sin Le > com Le
      Le_sin < Le_com ~ "large_com",  # If sin Le < com Le
      TRUE ~ "equal"  # If equal
    )
  )
# Count the number of plots with "large_com" and "small_com" factors
w_3_1 = dim(data_ceshi_3[data_ceshi_3$factor_pdown == "large_com",])  # Large_com count
w_3_2 = dim(data_ceshi_3[data_ceshi_3$factor_pdown == "small_com",])  # Small_com count

# Calculate percentage of small_com and large_com
y_3_1 = paste("(+)", format(round(w_3_2[1] / (w_3_1[1] + w_3_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
y_3_2 = paste("(-)", format(round(w_3_1[1] / (w_3_1[1] + w_3_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
# Display the calculated percentages
y_3_1
y_3_2

# Perform Wilcoxon signed-rank test for Rs_com vs Rs_sin
wilcox.test(data_ceshi_3$Rs_com, data_ceshi_3$Rs_sin, paired = TRUE, alternative = "less")

# Additional Analysis

# Join com and sin data for "tup" and calculate differences and factors
data_ceshi_4 = data_com_pdown_tup %>%
  inner_join(data_sin_tup, by = "plot_ID") %>%  # Merge datasets by plot_ID
  mutate(
    delt_Rs_tup = log2(Rs_sin) - log2(Rs_com)  # Calculate log2 difference for Rs
  ) %>%
  mutate(
    factor_tup  = case_when(
      Rs_sin > Rs_com ~ "small_com",  # If sin Rs > com Rs
      Rs_sin < Rs_com ~ "large_com",  # If sin Rs < com Rs
      TRUE ~ "equal"  # If equal
    )
  ) %>%
  mutate(
    Le_factor_tup = case_when(
      Le_sin > Le_com ~ "small_com",  # If sin Le > com Le
      Le_sin < Le_com ~ "large_com",  # If sin Le < com Le
      TRUE ~ "equal"  # If equal
    )
  )

# Count the number of plots with "large_com" and "small_com" factors
w_4_1 = dim(data_ceshi_4[data_ceshi_4$factor_tup == "large_com",])  # Large_com count
w_4_2 = dim(data_ceshi_4[data_ceshi_4$factor_tup == "small_com",])  # Small_com count

# Calculate percentage of small_com and large_com
y_4_1 = paste("(+)", format(round(w_4_2[1] / (w_4_1[1] + w_4_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
y_4_2 = paste("(-)", format(round(w_4_1[1] / (w_4_1[1] + w_4_2[1]) * 100, 1), nsmall = 1), sep =' ') %>%
  paste("%", sep ='')
# Display the calculated percentages
y_4_1
y_4_2

# Perform Wilcoxon signed-rank test for Rs_com vs Rs_sin
wilcox.test(data_ceshi_4$Rs_com, data_ceshi_4$Rs_sin, paired = TRUE, alternative = "less")

# Combine all Rs analyses into one dataframe and select relevant columns
data_ceshi_Rs = bind_rows(data_ceshi_1, data_ceshi_2, data_ceshi_3, data_ceshi_4) %>% 
  dplyr::select(plot_ID, Rs_com, Rs_sin, Le_com, Le_sin) 

# Display the dimensions of the combined Rs data
dim(data_ceshi_Rs)

### Plot b: Resistance Analysis ###

# Prepare data for Resistance plot
data_Rs = data_ceshi_Rs %>% 
  dplyr::select(
    plot_ID,
    Rs_com, 
    Rs_sin
  ) %>%
  mutate(
    delt_Rs = case_when(
      Rs_com > Rs_sin ~ "large_com",  # If com Rs > sin Rs
      Rs_com < Rs_sin ~ "small_com",  # If com Rs < sin Rs
      TRUE ~ "equal"  # If equal
    )
  )

# Find maximum and minimum values for Rs_com and Rs_sin
max(data_Rs$Rs_com)
min(data_Rs$Rs_com)
max(data_Rs$Rs_sin)
min(data_Rs$Rs_sin)

# Count the number of plots in each category
dim(data_Rs[data_Rs$delt_Rs == "large_com",])
dim(data_Rs[data_Rs$delt_Rs == "small_com",])

# Perform paired t-test on log2-transformed Rs_com and Rs_sin
t.test(log2(data_Rs$Rs_com), log2(data_Rs$Rs_sin))

# Calculate mean and standard deviation for Rs_sin and Rs_com
Rs_top = paste(
  format(round(mean(log2(data_Rs$Rs_sin)), 3), nsmall = 3),
  " ± ", 
  format(round(sd(log2(data_Rs$Rs_sin)), 3), nsmall = 3), 
  sep =''
) 
Rs_bottom = paste(
  format(round(mean(log2(data_Rs$Rs_com)), 3), nsmall = 3), 
  " ± ", 
  format(round(sd(log2(data_Rs$Rs_com)), 3), nsmall = 3), 
  sep =''
) 

# Create formatted strings with counts and percentages for Rs_com and Rs_sin
Rs_com_n = paste(
  Rs_bottom, "\n",
  dim(data_Rs[data_Rs$delt_Rs == "large_com",])[1], 
  " plots (", 
  format(
    round(
      dim(data_Rs[data_Rs$delt_Rs == "large_com",])[1] / 
        (dim(data_Rs[data_Rs$delt_Rs == "large_com",])[1] + dim(data_Rs[data_Rs$delt_Rs == "small_com",])[1]) * 100, 
      0
    ), 
    nsmall = 0
  ),
  "%)",
  sep =''
) 

Rs_sin_n = paste(
  dim(data_Rs[data_Rs$delt_Rs == "small_com",])[1], 
  " plots (", 
  format(
    round(
      dim(data_Rs[data_Rs$delt_Rs == "small_com",])[1] / 
        (dim(data_Rs[data_Rs$delt_Rs == "large_com",])[1] + dim(data_Rs[data_Rs$delt_Rs == "small_com",])[1]) * 100, 
      0
    ), 
    nsmall = 0
  ),
  "%)", 
  "\n",
  Rs_top,
  sep =''
) 

# Coordinates for upper and lower annotation areas (not used further in the code)
trsup <- data.frame(x = c(-3.5, -3.5, 21.5), y = c(-3.5, 21.5, 21.5))
trinf <- data.frame(x = c(-3.5, 21.5, 21.5), y = c(-3.5, -3.5, 21.5))

# Display the first few rows of data_Rs
head(data_Rs)

# Separate data_Rs into small_com and large_com categories
data_Rs_sin = data_Rs[data_Rs$delt_Rs == "small_com",]
data_Rs_com = data_Rs[data_Rs$delt_Rs == "large_com",]

# Add a title column for plotting purposes
data_Rs$title = "Resistance"

# Create the Resistance plot (p1_1)
p1_1 = ggplot(data_Rs, aes(x = log2(Rs_com + 1), y = log2(Rs_sin + 1))) +
  # Add annotation for p-value
  annotate("label", x = -2.5, y = 20.5,
           label = "italic(p)<0.001",
           parse = TRUE, size = 4.5, hjust = 0, vjust = 1,
           color = "black",
           fill = "white",
           label.size = NA) +
  # Create a 2D bin plot
  geom_bin2d(bins = 136.5, alpha = 0.98) +
  # Define color gradient for the bins
  scale_fill_gradientn(
    colors = c('#3235A4', '#21B1A6', '#FEF70F'), 
    name = "Number of\nplots (n)",
    breaks = c(1, 50, 100),
    guide = guide_colorbar(
      draw.ulim = FALSE, 
      draw.llim = FALSE
    )
  ) +
  # Customize y-axis
  scale_y_continuous(
    name = expression(atop(Resistance~(ln(italic(R)[s])), paste(under~individual~extremes))),
    breaks = seq(-3, 21, 6), 
    limits = c(-3.5, 21.5),
    expand = c(0, 0)
  ) +
  # Customize x-axis
  scale_x_continuous(
    name = expression(atop(Resistance~(ln(italic(R)[s])), paste(under~compound~extremes))),
    breaks = seq(-3, 21, 6), 
    limits = c(-3.5, 21.5),
    expand = c(0, 0)
  ) +
  # Add a diagonal dashed line
  geom_abline(intercept = 0, color = "black", linetype = "dashed", size = 0.4, slope = 1, alpha = 0.8) +
  # Apply a clean theme
  theme_bw() +
  # Add annotations with counts and percentages
  annotate("text", x = 9, y = 20.25, label = Rs_sin_n, size = 4.5, color = "black", vjust = 1) +
  annotate("text", x = 9, y = -2.25, label = Rs_com_n, size = 4.5, color = "black", vjust = 0)  

### Plot c: Legacy Effect Analysis ###

# Prepare data for Legacy Effect plot
data_Le = data_ceshi_Rs %>% 
  dplyr::select(
    plot_ID,
    Le_com, 
    Le_sin
  ) %>%
  mutate(
    delt_Le = case_when(
      Le_com > Le_sin ~ "large_com",  # If com Le > sin Le
      Le_com < Le_sin ~ "small_com",  # If com Le < sin Le
      TRUE ~ "equal"  # If equal
    )
  )

# Find maximum and minimum values for Le_com and Le_sin
max(data_Le$Le_com)
min(data_Le$Le_com)
max(data_Le$Le_sin)
min(data_Le$Le_sin)

# Count the number of plots in each category
dim(data_Le[data_Le$delt_Le == "large_com",])
dim(data_Le[data_Le$delt_Le == "small_com",])

# Calculate range of log2-transformed Le_com and Le_sin
range(log2(data_Le$Le_com))
range(log2(data_Le$Le_sin))

# Perform paired t-test on log2-transformed Le_com and Le_sin
t.test(log2(data_Le$Le_com), log2(data_Le$Le_sin))

# Calculate mean and standard deviation for Le_sin and Le_com
Le_top = paste(
  format(round(mean(log2(data_Le$Le_sin)), 3), nsmall = 3), 
  " ± ", 
  format(round(sd(log2(data_Le$Le_sin)), 3), nsmall = 3), 
  sep =''
) 
Le_bottom = paste(
  format(round(mean(log2(data_Le$Le_com)), 3), nsmall = 3), 
  " ± ", 
  format(round(sd(log2(data_Le$Le_com)), 3), nsmall = 3), 
  sep =''
) 

# Create formatted strings with counts and percentages for Le_com and Le_sin
Le_com_n = paste(
  Le_bottom, "\n",
  dim(data_Le[data_Le$delt_Le == "large_com",])[1], 
  " plots (", 
  format(
    round(
      dim(data_Le[data_Le$delt_Le == "large_com",])[1] / 
        (dim(data_Le[data_Le$delt_Le == "large_com",])[1] + dim(data_Le[data_Le$delt_Le == "small_com",])[1]) * 100, 
      0
    ), 
    nsmall = 0
  ),
  "%)",
  sep =''
) 

Le_sin_n = paste(
  dim(data_Le[data_Le$delt_Le == "small_com",])[1], 
  " plots (", 
  format(
    round(
      dim(data_Le[data_Le$delt_Le == "small_com",])[1] / 
        (dim(data_Le[data_Le$delt_Le == "large_com",])[1] + dim(data_Le[data_Le$delt_Le == "small_com",])[1]) * 100, 
      0
    ), 
    nsmall = 0
  ),
  "%)", 
  "\n",
  Le_top,
  sep =''
) 

# Coordinates for upper and lower annotation areas (not used further in the code)
trsup_Le <- data.frame(x = c(-0.15, -0.15, 0.95), y = c(-0.15, 0.95, 0.95))
trinf_Le <- data.frame(x = c(-0.15, 0.95, 0.95), y = c(-0.15, -0.15, 0.95))

# Display the first few rows of data_Rs
head(data_Rs)

# Separate data_Le into small_com and large_com categories
data_Le_sin = data_Le[data_Le$delt_Le == "small_com",]
data_Le_com = data_Le[data_Le$delt_Le == "large_com",]

# Load additional libraries for plotting
library(viridis)  # Color scales
library(metR)      # Meteorological data analysis (if needed)

# Create the Legacy Effect plot (p2_1)
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
  # Customize y-axis
  scale_y_continuous(
    name = expression(atop(Legacy~effect~(ln(italic(L)[e])), paste(under~individual~extremes))),
    breaks = seq(-0.1, 0.9, 0.2), 
    limits = c(-0.15, 0.95),
    expand = c(0, 0)
  ) +
  # Customize x-axis
  scale_x_continuous(
    name = expression(atop(Legacy~effect~(ln(italic(L)[e])), paste(under~compound~extremes))),
    breaks = seq(-0.1, 0.9, 0.2), 
    limits = c(-0.15, 0.95),
    expand = c(0, 0)
  ) +
  # Add a diagonal dashed line
  geom_abline(intercept = 0, color = "black", linetype = "dashed", size = 0.4, slope = 1, alpha = 0.8) +
  # Apply a clean theme
  theme_bw() +
  # Add annotations with counts and percentages
  annotate("text", x = 0.4, y = 0.895, label = Le_sin_n, size = 4.5, color = "black", vjust = 1) + 
  annotate("text", x = 0.4, y = -0.095, label = Le_com_n, size = 4.5, color = "black", vjust = 0)
