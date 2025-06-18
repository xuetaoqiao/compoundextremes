# ---------------------------------------------
# 1. Processing "pup_tup" (wet-hot and hot-wet) Extreme Types
# ---------------------------------------------

# Load necessary libraries
library(dplyr)    # For data manipulation
library(tidyr)    # For data tidying

# Filter 'data_com_all' for rows where 'ext_type' is "pup_tup",
# then group by 'plot_ID' and calculate the mean of selected columns.
data_com_pup_tup = data_com_all[data_com_all$ext_type == "pup_tup", ] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_com = mean(Rs_com),
    Rl_com = mean(Rl_com),
    Le_com = mean(Le_com),
    PGR_dis_com = mean(PGR_dis_com, na.rm = TRUE),  # Calculate mean, removing NA values
    PGR_leg_com = mean(PGR_leg_com, na.rm = TRUE)   # Calculate mean, removing NA values
  )
# Display the dimensions of the summarized data
dim(data_com_pup_tup) 

# ---------------------------------------------
# 2. Processing "pup" (Moisture Excess Alone) Data
# ---------------------------------------------

# Filter 'data_sin_all' for rows where 'ext_type' is "pup",
# then group by 'plot_ID' and calculate the mean of selected columns.
data_sin_pup = data_sin_all[data_sin_all$ext_type == "pup", ] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin, na.rm = TRUE),  # Calculate mean, removing NA values
    PGR_leg_sin = mean(PGR_leg_sin, na.rm = TRUE)   # Calculate mean, removing NA values
  ) 
# Display the dimensions of the summarized data
dim(data_sin_pup) 

# ---------------------------------------------
# 3. Processing "tup" (Heat Alone) Data
# ---------------------------------------------

# Filter 'data_sin_all' for rows where 'ext_type' is "tup",
# then group by 'plot_ID' and calculate the mean of selected columns.
data_sin_tup = data_sin_all[data_sin_all$ext_type == "tup", ] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin, na.rm = TRUE),  # Calculate mean, removing NA values
    PGR_leg_sin = mean(PGR_leg_sin, na.rm = TRUE)   # Calculate mean, removing NA values
  )
# Display the dimensions of the summarized data
dim(data_sin_tup) 

# ---------------------------------------------
# 4. Merging Summarized Data for "pup_tup"
# ---------------------------------------------

# Join the summarized 'com' and 'sin' data for "pup_tup" by 'plot_ID'
data_ceshi_1 = data_com_pup_tup %>%
  inner_join(data_sin_pup, by = "plot_ID") %>%  # Merge with "pup" (Moisture Excess Alone) data
  inner_join(data_sin_tup, by = "plot_ID")      # Merge with "tup" (Heat Alone) data
# Display the dimensions of the merged data
dim(data_ceshi_1)

# ---------------------------------------------
# 5. Categorizing Effects Based on PGR_dis
# ---------------------------------------------

# Create a new dataframe with categorized scenarios and effects
data_ceshi_1_zuizhong <- data_ceshi_1 %>%
  mutate(
    # Define 'scenario' based on the values of PGR_dis_sin.x and PGR_dis_sin.y
    scenario = case_when(
      PGR_dis_sin.x > 0 & PGR_dis_sin.y > 0 ~ "A",  # Both PGR_dis_sin.x and PGR_dis_sin.y are positive
      (PGR_dis_sin.x >= 0 & PGR_dis_sin.y <= 0) | (PGR_dis_sin.x <= 0 & PGR_dis_sin.y >= 0) ~ "B",  # One positive, one negative or zero
      PGR_dis_sin.x < 0 & PGR_dis_sin.y < 0 ~ "C"   # Both PGR_dis_sin.x and PGR_dis_sin.y are negative
    ),
    # Define 'effect_A' based on 'scenario' and the relationship between combined PGR_dis and PGR_dis_com
    effect_A = case_when(
      # Scenario A
      scenario == "A" & ((PGR_dis_sin.x + PGR_dis_sin.y) < PGR_dis_com) ~ "antagonistic",
      scenario == "A" & ((PGR_dis_sin.x + PGR_dis_sin.y) > PGR_dis_com) ~ "synergistic",
      scenario == "A" & ((PGR_dis_sin.x + PGR_dis_sin.y) == PGR_dis_com) ~ "additive",
      
      # Scenario B
      scenario == "B" & ((PGR_dis_sin.x + PGR_dis_sin.y) < PGR_dis_com) ~ "antagonistic",
      scenario == "B" & ((PGR_dis_sin.x + PGR_dis_sin.y) > PGR_dis_com) ~ "synergistic",
      scenario == "B" & ((PGR_dis_sin.x + PGR_dis_sin.y) == PGR_dis_com) ~ "additive",
      
      # Scenario C
      scenario == "C" & ((PGR_dis_sin.x + PGR_dis_sin.y) < PGR_dis_com) ~ "synergistic",
      scenario == "C" & ((PGR_dis_sin.x + PGR_dis_sin.y) > PGR_dis_com) ~ "antagonistic",
      scenario == "C" & ((PGR_dis_sin.x + PGR_dis_sin.y) == PGR_dis_com) ~ "additive"
    )
  )

# ---------------------------------------------
# 6. Exploring Scenarios and Handling Missing Data
# ---------------------------------------------

# Display unique scenarios present in the data
unique(data_ceshi_1_zuizhong$scenario)

# Check for any rows where 'scenario' is NA
data_ceshi_1_zuizhong[data_ceshi_1_zuizhong$scenario == "NA", ]

# Display the dimensions of the categorized data
dim(data_ceshi_1_zuizhong)

# ---------------------------------------------
# 7. Calculating Proportions of Synergistic and Antagonistic Effects
# ---------------------------------------------

# Calculate the proportion of "synergistic" effects
dim(data_ceshi_1_zuizhong[data_ceshi_1_zuizhong$effect_A == "synergistic", ])[1] / length(data_ceshi_1_zuizhong$effect_A)  # Approximately 79.13%

# Calculate the proportion of "antagonistic" effects
dim(data_ceshi_1_zuizhong[data_ceshi_1_zuizhong$effect_A == "antagonistic", ])[1] / length(data_ceshi_1_zuizhong$effect_A) 

# Store the counts of "synergistic" and "antagonistic" effects
pup_tup_syn = dim(data_ceshi_1_zuizhong[data_ceshi_1_zuizhong$effect_A == "synergistic", ])
pup_tup_ant = dim(data_ceshi_1_zuizhong[data_ceshi_1_zuizhong$effect_A == "antagonistic", ])

# ---------------------------------------------
# 8. Performing Wilcoxon Signed-Rank Tests
# ---------------------------------------------

# Perform Wilcoxon signed-rank test comparing PGR_dis_com with PGR_dis_sin.x
wilcox.test(
  data_ceshi_1_zuizhong$PGR_dis_com, 
  data_ceshi_1_zuizhong$PGR_dis_sin.x, 
  paired = TRUE, 
  alternative = "greater"
) 

# Perform Wilcoxon signed-rank test comparing PGR_dis_com with PGR_dis_sin.y
wilcox.test(
  data_ceshi_1_zuizhong$PGR_dis_com, 
  data_ceshi_1_zuizhong$PGR_dis_sin.y, 
  paired = TRUE, 
  alternative = "greater"
) 

# ---------------------------------------------
# 9. Processing "pdown_tup" (dry-hot and hot-dry) Extreme Types
# ---------------------------------------------

# Filter 'data_com_all' for rows where 'ext_type' is "pdown_tup",
# then group by 'plot_ID' and calculate the mean of selected columns.
data_com_pdown_tup = data_com_all[data_com_all$ext_type == "pdown_tup", ] %>% 
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

# ---------------------------------------------
# 10. Processing "pdown" (Drought Alone) Data
# ---------------------------------------------

# Filter 'data_sin_all' for rows where 'ext_type' is "pdown",
# then group by 'plot_ID' and calculate the mean of selected columns.
data_sin_pdown = data_sin_all[data_sin_all$ext_type == "pdown", ] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  )

# ---------------------------------------------
# 11. Processing "tup" (Heat Alone) Data for "pdown_tup"
# ---------------------------------------------

# Filter 'data_sin_all' for rows where 'ext_type' is "tup",
# then group by 'plot_ID' and calculate the mean of selected columns.
data_sin_tup = data_sin_all[data_sin_all$ext_type == "tup", ] %>% 
  group_by(plot_ID) %>% 
  summarise(
    Rs_sin = mean(Rs_sin),
    Rl_sin = mean(Rl_sin),
    Le_sin = mean(Le_sin),
    PGR_dis_sin = mean(PGR_dis_sin),
    PGR_leg_sin = mean(PGR_leg_sin)
  ) 

# ---------------------------------------------
# 12. Merging Summarized Data for "pdown_tup"
# ---------------------------------------------

# Join the summarized 'com' and 'sin' data for "pdown_tup" by 'plot_ID'
data_ceshi_3 = data_com_pdown_tup %>%
  inner_join(data_sin_pdown, by = "plot_ID") %>%  # Merge with "pdown" (Drought Alone) data
  inner_join(data_sin_tup, by = "plot_ID")        # Merge with "tup" (Heat Alone) data  

# ---------------------------------------------
# 13. Categorizing Effects Based on PGR_dis for "pdown_tup"
# ---------------------------------------------

# Create a new dataframe with categorized scenarios and effects for "pdown_tup"
data_ceshi_3_zuizhong <- data_ceshi_3 %>%
  mutate(
    # Define 'scenario' based on the values of PGR_dis_sin.x and PGR_dis_sin.y
    scenario = case_when(
      PGR_dis_sin.x > 0 & PGR_dis_sin.y > 0 ~ "A",  # Both PGR_dis_sin.x and PGR_dis_sin.y are positive
      (PGR_dis_sin.x >= 0 & PGR_dis_sin.y <= 0) | (PGR_dis_sin.x <= 0 & PGR_dis_sin.y >= 0) ~ "B",  # One positive, one negative or zero
      PGR_dis_sin.x < 0 & PGR_dis_sin.y < 0 ~ "C"   # Both PGR_dis_sin.x and PGR_dis_sin.y are negative
    ),
    # Define 'effect_A' based on 'scenario' and the relationship between combined PGR_dis and PGR_dis_com
    effect_A = case_when(
      # Scenario A
      scenario == "A" & ((PGR_dis_sin.x + PGR_dis_sin.y) < PGR_dis_com) ~ "antagonistic",
      scenario == "A" & ((PGR_dis_sin.x + PGR_dis_sin.y) > PGR_dis_com) ~ "synergistic",
      scenario == "A" & ((PGR_dis_sin.x + PGR_dis_sin.y) == PGR_dis_com) ~ "additive",
      
      # Scenario B
      scenario == "B" & ((PGR_dis_sin.x + PGR_dis_sin.y) < PGR_dis_com) ~ "antagonistic",
      scenario == "B" & ((PGR_dis_sin.x + PGR_dis_sin.y) > PGR_dis_com) ~ "synergistic",
      scenario == "B" & ((PGR_dis_sin.x + PGR_dis_sin.y) == PGR_dis_com) ~ "additive",
      
      # Scenario C
      scenario == "C" & ((PGR_dis_sin.x + PGR_dis_sin.y) < PGR_dis_com) ~ "synergistic",
      scenario == "C" & ((PGR_dis_sin.x + PGR_dis_sin.y) > PGR_dis_com) ~ "antagonistic",
      scenario == "C" & ((PGR_dis_sin.x + PGR_dis_sin.y) == PGR_dis_com) ~ "additive"
    )
  )

# ---------------------------------------------
# 14. Exploring Scenarios and Calculating Proportions for "pdown_tup"
# ---------------------------------------------

# Display unique scenarios present in the data
unique(data_ceshi_3_zuizhong$scenario)

# Calculate the proportion of "synergistic" effects
dim(data_ceshi_3_zuizhong[data_ceshi_3_zuizhong$effect_A == "synergistic", ])[1] / length(data_ceshi_3_zuizhong$effect_A) 

# Calculate the proportion of "antagonistic" effects
dim(data_ceshi_3_zuizhong[data_ceshi_3_zuizhong$effect_A == "antagonistic", ])[1] / length(data_ceshi_3_zuizhong$effect_A) 

# Store the counts of "synergistic" and "antagonistic" effects for "pdown_tup"
pdown_tup_syn = dim(data_ceshi_3_zuizhong[data_ceshi_3_zuizhong$effect_A == "synergistic", ])
pdown_tup_ant = dim(data_ceshi_3_zuizhong[data_ceshi_3_zuizhong$effect_A == "antagonistic", ])

# ---------------------------------------------
# 15. Performing Wilcoxon Signed-Rank Tests for "pdown_tup"
# ---------------------------------------------

# Perform Wilcoxon signed-rank test comparing PGR_dis_com with PGR_dis_sin.x
wilcox.test(
  data_ceshi_3_zuizhong$PGR_dis_com, 
  data_ceshi_3_zuizhong$PGR_dis_sin.x, 
  paired = TRUE, 
  alternative = "less"
) 

# Perform Wilcoxon signed-rank test comparing PGR_dis_com with PGR_dis_sin.y
wilcox.test(
  data_ceshi_3_zuizhong$PGR_dis_com, 
  data_ceshi_3_zuizhong$PGR_dis_sin.y, 
  paired = TRUE, 
  alternative = "less"
) 

# ---------------------------------------------
# 16. Processing Legacy Effects ("Le") for "pup_tup"
# ---------------------------------------------

# Repeat similar processing steps for Legacy Effects as done for PGR_dis

# Join the summarized 'com' and 'sin' data for "pup_tup" by 'plot_ID'
data_ceshi_5 = data_com_pup_tup %>%
  inner_join(data_sin_pup, by = "plot_ID") %>%  # Merge with "pup" (Moisture Excess Alone) data
  inner_join(data_sin_tup, by = "plot_ID")      # Merge with "tup" (Heat Alone) data  

# Create a new dataframe with categorized scenarios and effects based on PGR_leg
data_ceshi_5_zuizhong <- data_ceshi_5 %>%
  mutate(
    # Define 'scenario' based on the values of PGR_leg_sin.x and PGR_leg_sin.y
    scenario = case_when(
      PGR_leg_sin.x > 0 & PGR_leg_sin.y > 0 ~ "A",  # Both PGR_leg_sin.x and PGR_leg_sin.y are positive
      (PGR_leg_sin.x >= 0 & PGR_leg_sin.y <= 0) | (PGR_leg_sin.x <= 0 & PGR_leg_sin.y >= 0) ~ "B",  # One positive, one negative or zero
      PGR_leg_sin.x < 0 & PGR_leg_sin.y < 0 ~ "C"   # Both PGR_leg_sin.x and PGR_leg_sin.y are negative
    ),
    # Define 'effect_A' based on 'scenario' and the relationship between combined PGR_leg and PGR_leg_com
    effect_A = case_when(
      # Scenario A
      scenario == "A" & ((PGR_leg_sin.x + PGR_leg_sin.y) < PGR_leg_com) ~ "antagonistic",
      scenario == "A" & ((PGR_leg_sin.x + PGR_leg_sin.y) > PGR_leg_com) ~ "synergistic",
      scenario == "A" & ((PGR_leg_sin.x + PGR_leg_sin.y) == PGR_leg_com) ~ "additive",
      
      # Scenario B
      scenario == "B" & ((PGR_leg_sin.x + PGR_leg_sin.y) < PGR_leg_com) ~ "antagonistic",
      scenario == "B" & ((PGR_leg_sin.x + PGR_leg_sin.y) > PGR_leg_com) ~ "synergistic",
      scenario == "B" & ((PGR_leg_sin.x + PGR_leg_sin.y) == PGR_leg_com) ~ "additive",
      
      # Scenario C
      scenario == "C" & ((PGR_leg_sin.x + PGR_leg_sin.y) < PGR_leg_com) ~ "synergistic",
      scenario == "C" & ((PGR_leg_sin.x + PGR_leg_sin.y) > PGR_leg_com) ~ "antagonistic",
      scenario == "C" & ((PGR_leg_sin.x + PGR_leg_sin.y) == PGR_leg_com) ~ "additive"
    )
  )

# ---------------------------------------------
# 17. Exploring Scenarios and Calculating Proportions for "Le" in "pup_tup"
# ---------------------------------------------

# Display unique scenarios present in the data
unique(data_ceshi_5_zuizhong$scenario)

# Calculate the proportion of "synergistic" effects
dim(data_ceshi_5_zuizhong[data_ceshi_5_zuizhong$effect_A == "synergistic", ])[1] / length(data_ceshi_5_zuizhong$effect_A) 

# Calculate the proportion of "antagonistic" effects
dim(data_ceshi_5_zuizhong[data_ceshi_5_zuizhong$effect_A == "antagonistic", ])[1] / length(data_ceshi_5_zuizhong$effect_A) 

# ---------------------------------------------
# 18. Storing Counts of Synergistic and Antagonistic Effects for "Le" in "pup_tup"
# ---------------------------------------------

# Store the counts of "synergistic" and "antagonistic" effects for "Le" in "pup_tup"
pup_tup_syn2 = dim(data_ceshi_5_zuizhong[data_ceshi_5_zuizhong$effect_A == "synergistic", ])
pup_tup_ant2 = dim(data_ceshi_5_zuizhong[data_ceshi_5_zuizhong$effect_A == "antagonistic", ])

# ---------------------------------------------
# 19. Performing Wilcoxon Signed-Rank Tests for "Le" in "pup_tup"
# ---------------------------------------------

# Perform Wilcoxon signed-rank test comparing PGR_leg_com with PGR_leg_sin.x
wilcox.test(
  data_ceshi_5_zuizhong$PGR_leg_com, 
  data_ceshi_5_zuizhong$PGR_leg_sin.x, 
  paired = TRUE, 
  alternative = "greater"
) 

# Perform Wilcoxon signed-rank test comparing PGR_leg_com with PGR_leg_sin.y
wilcox.test(
  data_ceshi_5_zuizhong$PGR_leg_com, 
  data_ceshi_5_zuizhong$PGR_leg_sin.y, 
  paired = TRUE, 
  alternative = "greater"
) 

# ---------------------------------------------
# 20. Processing "pdown_tup" (dry-hot and hot-dry) Legacy Effects
# ---------------------------------------------

# Repeat similar processing steps for Legacy Effects as done for PGR_dis

# Join the summarized 'com' and 'sin' data for "pdown_tup" by 'plot_ID'
data_ceshi_7 = data_com_pdown_tup %>%
  inner_join(data_sin_pdown, by = "plot_ID") %>%  # Merge with "pdown" (Drought Alone) data
  inner_join(data_sin_tup, by = "plot_ID")        # Merge with "tup" (Heat Alone) data  

# Create a new dataframe with categorized scenarios and effects based on PGR_leg for "pdown_tup"
data_ceshi_7_zuizhong <- data_ceshi_7 %>%
  mutate(
    # Define 'scenario' based on the values of PGR_leg_sin.x and PGR_leg_sin.y
    scenario = case_when(
      PGR_leg_sin.x > 0 & PGR_leg_sin.y > 0 ~ "A",  # Both PGR_leg_sin.x and PGR_leg_sin.y are positive
      (PGR_leg_sin.x >= 0 & PGR_leg_sin.y <= 0) | (PGR_leg_sin.x <= 0 & PGR_leg_sin.y >= 0) ~ "B",  # One positive, one negative or zero
      PGR_leg_sin.x < 0 & PGR_leg_sin.y < 0 ~ "C"   # Both PGR_leg_sin.x and PGR_leg_sin.y are negative
    ),
    # Define 'effect_A' based on 'scenario' and the relationship between combined PGR_leg and PGR_leg_com
    effect_A = case_when(
      # Scenario A
      scenario == "A" & ((PGR_leg_sin.x + PGR_leg_sin.y) < PGR_leg_com) ~ "antagonistic",
      scenario == "A" & ((PGR_leg_sin.x + PGR_leg_sin.y) > PGR_leg_com) ~ "synergistic",
      scenario == "A" & ((PGR_leg_sin.x + PGR_leg_sin.y) == PGR_leg_com) ~ "additive",
      
      # Scenario B
      scenario == "B" & ((PGR_leg_sin.x + PGR_leg_sin.y) < PGR_leg_com) ~ "antagonistic",
      scenario == "B" & ((PGR_leg_sin.x + PGR_leg_sin.y) > PGR_leg_com) ~ "synergistic",
      scenario == "B" & ((PGR_leg_sin.x + PGR_leg_sin.y) == PGR_leg_com) ~ "additive",
      
      # Scenario C
      scenario == "C" & ((PGR_leg_sin.x + PGR_leg_sin.y) < PGR_leg_com) ~ "synergistic",
      scenario == "C" & ((PGR_leg_sin.x + PGR_leg_sin.y) > PGR_leg_com) ~ "antagonistic",
      scenario == "C" & ((PGR_leg_sin.x + PGR_leg_sin.y) == PGR_leg_com) ~ "additive"
    )
  )

# ---------------------------------------------
# 21. Exploring Scenarios and Calculating Proportions for "Le" in "pdown_tup"
# ---------------------------------------------

# Display unique scenarios present in the data
unique(data_ceshi_7_zuizhong$scenario)

# Calculate the proportion of "synergistic" effects
dim(data_ceshi_7_zuizhong[data_ceshi_7_zuizhong$effect_A == "synergistic", ])[1] / length(data_ceshi_7_zuizhong$effect_A) 

# Calculate the proportion of "antagonistic" effects
dim(data_ceshi_7_zuizhong[data_ceshi_7_zuizhong$effect_A == "antagonistic", ])[1] / length(data_ceshi_7_zuizhong$effect_A) 

# ---------------------------------------------
# 22. Storing Counts of Synergistic and Antagonistic Effects for "Le" in "pdown_tup"
# ---------------------------------------------

# Store the counts of "synergistic" and "antagonistic" effects for "Le" in "pdown_tup"
pdown_tup_syn2 = dim(data_ceshi_7_zuizhong[data_ceshi_7_zuizhong$effect_A == "synergistic", ])
pdown_tup_ant2 = dim(data_ceshi_7_zuizhong[data_ceshi_7_zuizhong$effect_A == "antagonistic", ])

# ---------------------------------------------
# 23. Performing Wilcoxon Signed-Rank Tests for "Le" in "pdown_tup"
# ---------------------------------------------

# Perform Wilcoxon signed-rank test comparing PGR_leg_com with PGR_leg_sin.x
wilcox.test(
  data_ceshi_7_zuizhong$PGR_leg_com, 
  data_ceshi_7_zuizhong$PGR_leg_sin.x, 
  paired = TRUE, 
  alternative = "greater"
) 

# Perform Wilcoxon signed-rank test comparing PGR_leg_com with PGR_leg_sin.y
wilcox.test(
  data_ceshi_7_zuizhong$PGR_leg_com, 
  data_ceshi_7_zuizhong$PGR_leg_sin.y, 
  paired = TRUE, 
  alternative = "greater"
) 

# ---------------------------------------------
# 24. Creating Data Frames for Effect Types
# ---------------------------------------------

# Create a data frame for disturbance phase effects
data_effect_dis <- data.frame(
  Pair = factor(c("Compound heat and moisture excess",
                  "Compound heat and drought"), 
                levels = c("Compound heat and drought",
                           "Compound heat and moisture excess")),
  Synergistic_effect = c(
    pup_tup_syn[1] / (pup_tup_syn[1] + pup_tup_ant[1]) * 100,  # Percentage of synergistic effects for "pup_tup"
    pdown_tup_syn[1] / (pdown_tup_syn[1] + pdown_tup_ant[1]) * 100  # Percentage of synergistic effects for "pdown_tup"
  ),
  Synergistic_num = c(
    pup_tup_syn[1],  # Number of synergistic effects for "pup_tup"
    pdown_tup_syn[1]  # Number of synergistic effects for "pdown_tup"
  ),
  
  Antagonistic_effect = c(
    pup_tup_ant[1] / (pup_tup_syn[1] + pup_tup_ant[1]) * 100,  # Percentage of antagonistic effects for "pup_tup"
    pdown_tup_ant[1] / (pdown_tup_syn[1] + pdown_tup_ant[1]) * 100  # Percentage of antagonistic effects for "pdown_tup"
  ),
  Antagonistic_num = c(
    pup_tup_ant[1],  # Number of antagonistic effects for "pup_tup"
    pdown_tup_ant[1]  # Number of antagonistic effects for "pdown_tup"
  )
) %>%
  mutate(title = "Disturbance phase")  # Add a title column

# Create a data frame for post-disturbance phase effects
data_effect_leg <- data.frame(
  Pair = factor(c("Compound heat and moisture excess",
                  "Compound heat and drought"), 
                levels = c("Compound heat and drought",
                           "Compound heat and moisture excess")),
  Synergistic_effect = c(
    pup_tup_syn2[1] / (pup_tup_syn2[1] + pup_tup_ant2[1]) * 100,  # Percentage of synergistic effects for "Le" in "pup_tup"
    pdown_tup_syn2[1] / (pdown_tup_syn2[1] + pdown_tup_ant2[1]) * 100  # Percentage of synergistic effects for "Le" in "pdown_tup"
  ),
  Synergistic_num = c(
    pup_tup_syn2[1],  # Number of synergistic effects for "Le" in "pup_tup"
    pdown_tup_syn2[1]  # Number of synergistic effects for "Le" in "pdown_tup"
  ),
  
  Antagonistic_effect = c(
    pup_tup_ant2[1] / (pup_tup_syn2[1] + pup_tup_ant2[1]) * 100,  # Percentage of antagonistic effects for "Le" in "pup_tup"
    pdown_tup_ant2[1] / (pdown_tup_syn2[1] + pdown_tup_ant2[1]) * 100  # Percentage of antagonistic effects for "Le" in "pdown_tup"
  ),
  Antagonistic_num = c(
    pup_tup_ant2[1],  # Number of antagonistic effects for "Le" in "pup_tup"
    pdown_tup_ant2[1]  # Number of antagonistic effects for "Le" in "pdown_tup"
  )
) %>%
  mutate(title = "Post-disturbance phase")  # Add a title column

# Combine disturbance and post-disturbance phase effects into one data frame
data_effect = rbind(data_effect_dis, data_effect_leg)
# Display the first few rows of the combined data frame
head(data_effect)

# ---------------------------------------------
# 25. Reshaping Data for Plotting
# ---------------------------------------------

# Load additional libraries for data reshaping and plotting
library(reshape2)  # For data reshaping
library(tidyverse)  # For data manipulation and visualization

# Transform 'data_effect' into a long format suitable for plotting
data_long2 <- data_effect[c(1, 3, 5, 6)] %>%
  pivot_longer(
    cols = c(Synergistic_num, Antagonistic_num), 
    names_to = "variable", 
    values_to = "num"
  ) %>%
  mutate(
    variable = if_else(variable == "Synergistic_num", "Synergistic_effect", variable)
  ) %>%
  mutate(
    variable = if_else(variable == "Antagonistic_num", "Antagonistic_effect", variable)
  )
# Invert the 'num' values for synergistic effects to position them on the negative side
data_long2$num[data_long2$variable == "Synergistic_effect"] <- -data_long2$num[data_long2$variable == "Synergistic_effect"]

# Transform 'data_effect' into another long format for percentages
data_long1 <- data_effect[c(1, 2, 4, 6)] %>%
  pivot_longer(
    cols = c(Synergistic_effect, Antagonistic_effect), 
    names_to = "variable", 
    values_to = "value"
  ) 
# Invert the 'value' for synergistic effects to position them on the negative side
data_long1$value[data_long1$variable == "Synergistic_effect"] <- -data_long1$value[data_long1$variable == "Synergistic_effect"]

# Combine the two long data frames into one for plotting
data_long_total = data_long1 %>%
  left_join(data_long2, by = c("Pair", "title", "variable"))

# ---------------------------------------------
# 26. Creating the Combined Effect Plot (p1)
# ---------------------------------------------

# Create a bar plot showing the percentage of synergistic and antagonistic effects
p1 = ggplot(data_long_total, aes(x = value, y = Pair, fill = variable, color = variable)) +
  geom_bar(stat = "identity", position = "identity", width = 0.8, alpha = 0.6, size = 0.8) +  # Create bars
  scale_fill_manual(
    values = c("Synergistic_effect" = "#A41D1A", "Antagonistic_effect" = "#D26A18")
  ) +  # Define custom fill colors
  scale_color_manual(
    values = c("Synergistic_effect" = "#A41D1A", "Antagonistic_effect" = "#D26A18")
  ) +  # Define custom border colors
  xlab("Percentage of combined effect type (%)") +  # Label for x-axis
  scale_x_continuous(
    breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
    limits = c(-102, 102),
    labels = c(100, 75, 50, 25, 0, 25, 50, 75, 100)  # Custom labels to display positive percentages
  ) +  # Define x-axis breaks and labels
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.6, alpha = 0.75) +  # Add a dashed vertical line at x=0
  theme_bw() +  # Apply a clean theme
  # Add text labels for synergistic effects
  geom_text(
    aes(label = ifelse(variable == "Synergistic_effect", sprintf("(%.0f%%)", abs(value)), "")), 
    vjust = 1.5, 
    size = 4, 
    colour = "grey95", 
    angle = 90
  ) + 
  # Add text labels for antagonistic effects
  geom_text(
    aes(label = ifelse(variable == "Antagonistic_effect", sprintf("(%.0f%%)", abs(value)), "")), 
    vjust = 1.5, 
    size = 4, 
    colour = "grey95", 
    angle = 270
  )

# Display the combined effect plot
p1
