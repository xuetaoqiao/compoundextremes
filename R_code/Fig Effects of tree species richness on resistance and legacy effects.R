########
# ---------------------------------------------
# 1. Filtering Data for "pup_tup" and "pdown_tup" Extreme Types
# ---------------------------------------------

# Filter 'data_com_all' for rows where 'ext_type' is "pup_tup" (wet-hot and hot-wet scenarios)
data100 = data_com_all[data_com_all$ext_type == "pup_tup",]   

# Filter 'data_com_all' for rows where 'ext_type' is "pdown_tup" (dry-hot and hot-dry scenarios)
data300 = data_com_all[data_com_all$ext_type == "pdown_tup",] 

# ---------------------------------------------
# 2. Exploring the Range of Variables in "pdown_tup" Data
# ---------------------------------------------

# Check the range of 'Rs_com' in the "pdown_tup" dataset
range(data300$Rs_com)

# Check the range of 'species_richness' in the "pdown_tup" dataset
range(data300$species_richness)

# ---------------------------------------------
# 3. Filtering Data for Individual Extremes
# ---------------------------------------------

# Filter 'data_sin_all' for rows where 'ext_type' is "pup" (Moisture Excess Alone)
data1000 = data_sin_all[data_sin_all$ext_type == "pup",]

# Filter 'data_sin_all' for rows where 'ext_type' is "tup" (Heat Alone)
data3000 = data_sin_all[data_sin_all$ext_type == "tup",]   

# Filter 'data_sin_all' for rows where 'ext_type' is "pdown" (Drought Alone)
data4000 = data_sin_all[data_sin_all$ext_type == "pdown",]

# ---------------------------------------------
# 4. Checking the Dimensions of Filtered Datasets
# ---------------------------------------------

# Display the dimensions of the "pup_tup" and "pdown_tup" datasets
dim(data100); dim(data300)

# Display the dimensions of the individual extremes datasets
dim(data1000); dim(data3000); dim(data4000)

# ---------------------------------------------
# 5. Loading Necessary Libraries for Modeling
# ---------------------------------------------

# Load the 'car' package for calculating Variance Inflation Factors (VIF)
library(car)

# ---------------------------------------------
# 6. Building Linear Mixed-Effects Models for "pup_tup"
# ---------------------------------------------

# Build a linear mixed-effects model for "pup_tup" data
model_nlme_mode1 <- lme(
  log2(Rs_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE) + scale(ELEV) + 
    scale(ph)  + scale(totaln) +  scale(bulk) + scale(clay) +
    scale(pre) + scale(tem), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data100
)
# Calculate Variance Inflation Factors to check for multicollinearity
vif(model_nlme_mode1)
# Display the summary of the model
summary(model_nlme_mode1) 

# Build another linear mixed-effects model for "pdown_tup" data
model_nlme_mode3 <- lme(
  log2(Rs_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE) + scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data300
)
# Calculate VIF
vif(model_nlme_mode3)
# Display the summary of the model
summary(model_nlme_mode3) 

# Build a combined linear mixed-effects model using all "com" data
model_nlme_mode_com_total <- lme(
  log2(Rs_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE) + scale(ELEV) +
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem),
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_com_all
)
# Calculate VIF
vif(model_nlme_mode_com_total)
# Display the summary of the model
summary(model_nlme_mode_com_total) 

# ---------------------------------------------
# 7. Building Linear Mixed-Effects Models for Individual Extremes ("pup", "tup", "pdown")
# ---------------------------------------------

# Build a linear mixed-effects model for "pup" (Moisture Excess Alone) data
model_nlme_mode10 <- lme(
  log2(Rs_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE)  + scale(ELEV) +
    scale(ph)  + scale(totaln) +  scale(bulk) + scale(clay) +
    scale(pre) + scale(tem), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data1000
)
# Calculate VIF
vif(model_nlme_mode10)
# Display the summary of the model
summary(model_nlme_mode10) 

# Build a linear mixed-effects model for "tup" (Heat Alone) data
model_nlme_mode30 <- lme(
  log2(Rs_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE)  + scale(ELEV) +
    scale(ph)  + scale(totaln) +  scale(bulk) + scale(clay) +
    scale(pre) + scale(tem), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data3000
)
# Calculate VIF
vif(model_nlme_mode30)
# Display the summary of the model
summary(model_nlme_mode30)

# Build a linear mixed-effects model for "pdown" (Drought Alone) data
model_nlme_mode40 <- lme(
  log2(Rs_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE)  + scale(ELEV) + 
    scale(ph)  + scale(totaln) +  scale(bulk) + scale(clay) +
    scale(pre) + scale(tem), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data4000
)
# Calculate VIF
vif(model_nlme_mode40)
# Display the summary of the model
summary(model_nlme_mode40) 

# Build a combined linear mixed-effects model using all "sin" data
model_nlme_mode_sin_total <- lme(
  log2(Rs_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE) + scale(ELEV) + 
    scale(ph)  + scale(totaln) +  scale(bulk) + scale(clay) +
    scale(pre) + scale(tem), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_sin_all
)
# Calculate VIF
vif(model_nlme_mode_sin_total)
# Display the summary of the model
summary(model_nlme_mode_sin_total)

# ---------------------------------------------
# 8. Loading Libraries for Plotting Models
# ---------------------------------------------

# Load the 'sjPlot' package for plotting model summaries
library(sjPlot)

# ---------------------------------------------
# 9. Plotting the Models Using 'sjPlot'
# ---------------------------------------------

# Plot the model for "pup_tup" (com_model_1)
com_model_1 = plot_model(model_nlme_mode1)

# Plot the model for "pdown_tup" (com_model_3)
com_model_3 = plot_model(model_nlme_mode3)

# Plot the combined model for all "com" data (com_model_total)
com_model_total = plot_model(model_nlme_mode_com_total)

# Plot the model for "pup" (sin_model_1)
sin_model_1 = plot_model(model_nlme_mode10)

# Plot the model for "tup" (sin_model_3)
sin_model_3 = plot_model(model_nlme_mode30)

# Plot the model for "pdown" (sin_model_4)
sin_model_4 = plot_model(model_nlme_mode40)

# Plot the combined model for all "sin" data (sin_model_total)
sin_model_total = plot_model(model_nlme_mode_sin_total)

# ---------------------------------------------
# 10. Inspecting the Data Used in the Plots
# ---------------------------------------------

# View the data used in the "pup_tup" model plot
com_model_1$data

# View the data used in the "pup" model plot
sin_model_1$data

# View the data used in the "pdown_tup" model plot
com_model_3$data

# View the data used in the "tup" model plot
sin_model_3$data

# View the data used in the "pdown" model plot
sin_model_4$data

# View the data used in the combined "com" model plot
com_model_total$data

# View the data used in the combined "sin" model plot
sin_model_total$data

# ---------------------------------------------
# 11. Extracting Estimates and Confidence Intervals from Models
# ---------------------------------------------

# Combine estimates, lower and upper confidence intervals from all models
newtemn = rbind(
  com_model_total$data$estimate[1],
  com_model_1$data$estimate[1],
  com_model_3$data$estimate[1],
  
  sin_model_total$data$estimate[1],
  sin_model_1$data$estimate[1],
  sin_model_3$data$estimate[1],
  sin_model_4$data$estimate[1]
)

# Combine lower confidence intervals
lower = rbind(
  com_model_total$data$conf.low[1],
  com_model_1$data$conf.low[1],
  com_model_3$data$conf.low[1],
  
  sin_model_total$data$conf.low[1],
  sin_model_1$data$conf.low[1],
  sin_model_3$data$conf.low[1],
  sin_model_4$data$conf.low[1]
)

# Combine upper confidence intervals
uper = rbind(
  com_model_total$data$conf.high[1],
  com_model_1$data$conf.high[1],
  com_model_3$data$conf.high[1],
  
  sin_model_total$data$conf.high[1],
  sin_model_1$data$conf.high[1],
  sin_model_3$data$conf.high[1],
  sin_model_4$data$conf.high[1]
)

# Define the extreme types corresponding to each model
extrem_type = c(
  "com",
  "pre_up&tem_up",
  "pre_down&tem_up",
  
  "sin",
  "pre_up",
  "tem_up",
  "pre_down"
)

# Define the type of extreme scenario
com_type = c(
  "Compound extremes",
  "Compound extremes",
  "Compound extremes",
  
  "Individual extremes",
  "Individual extremes",
  "Individual extremes",
  "Individual extremes"
)

# Define a factor variable (currently all set to "a")
factor = c("a", "a", "a", "a", "a", "a", "a")

# Combine all extracted information into a single data frame
newtemn1 = cbind(com_type, extrem_type, newtemn, lower, uper, factor) 
# Check dimensions to ensure correctness
dim(extrem_type); dim(newtemn); dim(lower); dim(uper)

# Assign column names to the combined data frame
colnames(newtemn1) = c("com_type", "extrem_type", "Estimate", "lower", "uper", "factor")
# Convert to a data frame
newtemn2 = as.data.frame(newtemn1)
# Convert relevant columns to numeric
newtemn2$Estimate = as.numeric(newtemn2$Estimate)
newtemn2$lower = as.numeric(newtemn2$lower)
newtemn2$uper = as.numeric(newtemn2$uper)

# Display the structure of the new data frame
str(newtemn2)

# Define colors for the bars and points in the plot
barCOLS = c("#efa112","#219a21")
dotCOLS = c("#f5c266","#66b966")

# Reorder the 'extrem_type' factor for consistent plotting
newtemn2$extrem_type <- factor(newtemn2$extrem_type, 
                               levels = c("com","pre_up&tem_up","pre_down&tem_up",
                                          "sin","tem_up","pre_up","pre_down"))

# Load the 'forcats' package for factor manipulation
library(forcats)

# Recode the 'extrem_type' for better readability in plots
newtemn3 <- newtemn2 %>%
  mutate(extrem_type = fct_recode(extrem_type,
                                  "Compound\nextremes"  = "com",
                                  "Compound\nheat and moisture\nexcess"  = "pre_up&tem_up",
                                  "Compound\nheat and drought"  = "pre_down&tem_up",
                                  
                                  "Individual\nextremes"  = "sin",
                                  "Heat\nalone"    = "tem_up",
                                  "Moisture excess\nalone"    = "pre_up",
                                  "Drought\nalone"    = "pre_down"))

# View the first few rows of the recoded data
head(newtemn3)

# ---------------------------------------------
# 12. Creating a Data Frame with Plot Counts
# ---------------------------------------------

# Create a data frame 'plot_num' that contains the number of plots for each extreme type
plot_num <- data.frame(
  extreme_type = c(
    "compound extremes",
    "compound heat and moisture excess extremes",
    "compound heat and drought extremes",
    
    "individual extremes",
    "individual moisture excess extremes",
    "individual heat extremes",
    "individual drought extremes"
  ), 
  lbl = c(
    dim(data_com_all)[1],    # Total compound extremes
    dim(data100)[1],         # Compound heat and moisture excess extremes
    dim(data300)[1],         # Compound heat and drought extremes
    
    dim(data_sin_all)[1],    # Total individual extremes
    dim(data1000)[1],        # Individual moisture excess extremes
    dim(data3000)[1],        # Individual heat extremes
    dim(data4000)[1]         # Individual drought extremes
  )
) %>%
  mutate(extrem_type = fct_recode(extrem_type,
                                  "Compound\nextremes"  = "compound extremes",
                                  "Compound\nheat and moisture\nexcess"  = "compound heat and moisture excess extremes",
                                  "Compound\nheat and drought"  = "compound heat and drought extremes",
                                  
                                  "Individual\nextremes"  = "individual extremes", 
                                  "Heat\nalone"  = "individual heat extremes", 
                                  "Moisture excess\nalone"  = "individual moisture excess extremes",
                                  "Drought\nalone"  = "individual drought extremes"))

# ---------------------------------------------
# 13. Creating the Bar Plot for Resistance (Rs) Effects
# ---------------------------------------------

# Create a bar plot 'pb' showing the estimates and confidence intervals for resistance effects
pb <- ggplot(newtemn3, aes(x = extrem_type, y = Estimate, ymin = lower, ymax = uper, col = com_type, fill = com_type)) +
  
  # Add lineranges to represent confidence intervals
  geom_linerange(size = 6, position = position_dodge(width = 0.5), show.legend = FALSE) +
  
  # Define custom fill and color scales
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  
  # Add points for the estimates
  geom_point(size = 4.5, shape = 19, colour = "white", 
             stroke = 1, position = position_dodge(width = 0.5)) +
  
  # Customize the y-axis
  scale_y_continuous(
    breaks = c(-0.1, 0, 0.1, 0.2),
    limits = c(-0.12, 0.22)
  ) +
  
  # Add a horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  
  # Apply a clean theme
  theme_bw() +
  
  # Define labels for the axes
  labs(
    x = NULL,
    y = expression(atop(Species~diversity~effect~(italic(theta))~on, 
                        paste(resistance~(ln(italic(R)[s])))))
  ) +
  
  # Highlight different regions with semi-transparent rectangles
  annotate("rect", xmin = 0.4, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#e9e2c8") +
  annotate("rect", xmin = 3.5, xmax = 7.6, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#bbdcc5") +
  
  # Add text labels indicating the number of plots
  geom_text(
    data = plot_num, 
    aes(x = extrem_type, y = -0.1, label = paste0("(", lbl, ")")), 
    size = 6, 
    color = "grey50"
  )

# Display the bar plot
pb

# ---------------------------------------------
# 14. Building Linear Mixed-Effects Models for Legacy Effects (Le)
# ---------------------------------------------

# Build a linear mixed-effects model for "pup_tup" data focusing on Legacy Effects
model_Le_mode1 <- lme(
  log2(Le_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE) +  scale(ELEV) + 
    scale(ph)  + scale(totaln) +  scale(bulk) + scale(clay) +
    scale(pre) + scale(tem) + scale(Rs_com), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data100
)
# Calculate VIF
vif(model_Le_mode1)
# Display the summary of the model
summary(model_Le_mode1) 

# Build another linear mixed-effects model for "pdown_tup" data
model_Le_mode3 <- lme(
  log2(Le_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE) +  scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem)  + scale(Rs_com),  
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data300
)
# Calculate VIF
vif(model_Le_mode3)
# Display the summary of the model
summary(model_Le_mode3) 

# Build a combined linear mixed-effects model using all "com" data for Legacy Effects
model_Le_mode_com_total <- lme(
  log2(Le_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE) +  scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem) + scale(Rs_com),
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_com_all
)
# Calculate VIF
vif(model_Le_mode_com_total)
# Display the summary of the model
summary(model_Le_mode_com_total) 

# ---------------------------------------------
# 15. Building Linear Mixed-Effects Models for Legacy Effects of Individual Extremes
# ---------------------------------------------

# Check the range of 'Le_sin' in the "pup" (Moisture Excess Alone) dataset
range(data1000$Le_sin)

# Build a linear mixed-effects model for "pup" (Moisture Excess Alone) Legacy Effects
model_Le_mode10 <- lme(
  log2(Le_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE) +  scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem) + scale(Rs_sin), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data1000
)
# Calculate VIF
vif(model_Le_mode10)
# Display the summary of the model
summary(model_Le_mode10) 

# Build a linear mixed-effects model for "tup" (Heat Alone) Legacy Effects
model_Le_mode30 <- lme(
  log2(Le_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE) +  scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem) + scale(Rs_sin), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data3000
)
# Calculate VIF
vif(model_Le_mode30)
# Display the summary of the model
summary(model_Le_mode30) 

# Build a linear mixed-effects model for "pdown" (Drought Alone) Legacy Effects
model_Le_mode40 <- lme(
  log2(Le_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE) +  scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem) + scale(Rs_sin), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data4000
)
# Calculate VIF
vif(model_Le_mode40)
# Display the summary of the model
summary(model_Le_mode40) 

# Build a combined linear mixed-effects model using all "sin" data for Legacy Effects
model_Le_mode_sin_total <- lme(
  log2(Le_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE) +  scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem) + scale(Rs_sin), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_sin_all
)
# Calculate VIF
vif(model_Le_mode_sin_total)
# Display the summary of the model
summary(model_Le_mode_sin_total) 

# ---------------------------------------------
# 16. Plotting Legacy Effects Models Using 'sjPlot'
# ---------------------------------------------

# Plot the combined Legacy Effects model for "com" data
com_model_total = plot_model(model_Le_mode_com_total)

# Plot the Legacy Effects model for "pup" data
com_model_5 = plot_model(model_Le_mode1)

# Plot the Legacy Effects model for "pdown_tup" data
com_model_7 = plot_model(model_Le_mode3)

# Plot the combined Legacy Effects model for "sin" data
sin_model_total = plot_model(model_Le_mode_sin_total)

# Plot the Legacy Effects model for "pup" data
sin_model_5 = plot_model(model_Le_mode10)

# Plot the Legacy Effects model for "tup" data
sin_model_7 = plot_model(model_Le_mode30)

# Plot the Legacy Effects model for "pdown" data
sin_model_8 = plot_model(model_Le_mode40)

# ---------------------------------------------
# 17. Inspecting the Data Used in Legacy Effects Models
# ---------------------------------------------

# View the data used in the "pup" Legacy Effects model plot
com_model_5$data

# View the data used in the "pup_tup" Legacy Effects model plot
sin_model_5$data

# View the data used in the "pdown_tup" Legacy Effects model plot
com_model_7$data

# View the data used in the "tup" Legacy Effects model plot
sin_model_7$data

# View the data used in the "pdown" Legacy Effects model plot
sin_model_8$data

# View the data used in the combined "com" Legacy Effects model plot
com_model_total$data

# View the data used in the combined "sin" Legacy Effects model plot
sin_model_total$data

# ---------------------------------------------
# 18. Extracting Estimates and Confidence Intervals for Legacy Effects Models
# ---------------------------------------------

# Combine estimates from all Legacy Effects models
newtemn_Le = rbind(
  com_model_total$data$estimate[1],
  com_model_5$data$estimate[1],
  com_model_7$data$estimate[1],
  
  sin_model_total$data$estimate[1],
  sin_model_5$data$estimate[1],
  sin_model_7$data$estimate[1],
  sin_model_8$data$estimate[1]
)

# Combine lower confidence intervals
lower_Le = rbind(
  com_model_total$data$conf.low[1],
  com_model_5$data$conf.low[1],
  com_model_7$data$conf.low[1],
  
  sin_model_total$data$conf.low[1],
  sin_model_5$data$conf.low[1],
  sin_model_7$data$conf.low[1],
  sin_model_8$data$conf.low[1]
)

# Combine upper confidence intervals
uper_Le = rbind(
  com_model_total$data$conf.high[1],
  com_model_5$data$conf.high[1],
  com_model_7$data$conf.high[1],
  
  sin_model_total$data$conf.high[1],
  sin_model_5$data$conf.high[1],
  sin_model_7$data$conf.high[1],
  sin_model_8$data$conf.high[1]
)

# Define the extreme types corresponding to each model
extrem_type_Le = c(
  "com",
  "pre_up&tem_up",
  "pre_down&tem_up",
  
  "sin",
  "pre_up",
  "tem_up",
  "pre_down"
)

# Define the type of extreme scenario
com_type_Le = c(
  "Compound extremes",
  "Compound extremes",
  "Compound extremes",
  
  "Individual extremes",
  "Individual extremes",
  "Individual extremes",
  "Individual extremes"
)

# Combine all extracted information into a single data frame for Legacy Effects
newtemn_Le1 = cbind(com_type_Le, extrem_type_Le, newtemn_Le, lower_Le, uper_Le) 
# Check dimensions to ensure correctness
dim(extrem_type_Le); dim(newtemn_Le); dim(lower_Le); dim(uper_Le)

# Assign column names to the combined data frame
colnames(newtemn_Le1) = c("com_type", "extrem_type", "Estimate", "lower", "uper")
# Convert to a data frame
newtemn_Le2 = as.data.frame(newtemn_Le1)
# Convert relevant columns to numeric
newtemn_Le2$Estimate = as.numeric(newtemn_Le2$Estimate)
newtemn_Le2$lower = as.numeric(newtemn_Le2$lower)
newtemn_Le2$uper = as.numeric(newtemn_Le2$uper)

# Display the structure of the new Legacy Effects data frame
str(newtemn_Le2)

# Define colors for the bars and points in the Legacy Effects plot
barCOLS = c("#efa112","#219a21")
dotCOLS = c("#f5c266","#66b966")

# Reorder the 'extrem_type' factor for consistent plotting
newtemn_Le2$extrem_type <- factor(newtemn_Le2$extrem_type, 
                                  levels = c("com","pre_up&tem_up","pre_down&tem_up",
                                             "sin","tem_up","pre_up","pre_down"))

# Load the 'forcats' package for factor manipulation
library(forcats)

# Recode the 'extrem_type' for better readability in plots
newtemn_Le3 <- newtemn_Le2 %>%
  mutate(extrem_type = fct_recode(extrem_type,
                                  "Compound\nextremes"  = "com",
                                  "Compound\nheat and moisture\nexcess"  = "pre_up&tem_up",
                                  "Compound\nheat and drought"  = "pre_down&tem_up",
                                  
                                  "Individual\nextremes"    = "sin",
                                  "Heat\nalone"    = "tem_up",
                                  "Moisture excess\nalone"    = "pre_up",
                                  "Drought\nalone"    = "pre_down"))

# View the first few rows of the recoded Legacy Effects data
head(newtemn_Le3)

# ---------------------------------------------
# 19. Creating a Data Frame with Plot Counts for Legacy Effects
# ---------------------------------------------

# Create a data frame 'plot_num' that contains the number of plots for each extreme type
plot_num <- data.frame(
  extreme_type = c(
    "compound extremes",
    "compound heat and moisture excess extremes",
    "compound heat and drought extremes",
    
    "individual extremes",
    "individual moisture excess extremes",
    "individual heat extremes",
    "individual drought extremes"
  ), 
  lbl = c(
    dim(data_com_all)[1],    # Total compound extremes
    dim(data100)[1],         # Compound heat and moisture excess extremes
    dim(data300)[1],         # Compound heat and drought extremes
    
    dim(data_sin_all)[1],    # Total individual extremes
    dim(data1000)[1],        # Individual moisture excess extremes
    dim(data3000)[1],        # Individual heat extremes
    dim(data4000)[1]         # Individual drought extremes
  )
) %>%
  mutate(extrem_type = fct_recode(extrem_type,
                                  "Compound\nextremes"  = "compound extremes",
                                  "Compound\nheat and moisture\nexcess"  = "compound heat and moisture excess extremes",
                                  "Compound\nheat and drought"  = "compound heat and drought extremes",
                                  
                                  "Individual\nextremes"  = "individual extremes", 
                                  "Heat\nalone"  = "individual heat extremes", 
                                  "Moisture excess\nalone"  = "individual moisture excess extremes",
                                  "Drought\nalone"  = "individual drought extremes"))

# ---------------------------------------------
# 20. Creating the Bar Plot for Legacy Effects (Le)
# ---------------------------------------------

# Create a bar plot 'pd' showing the estimates and confidence intervals for legacy effects
pd <- ggplot(newtemn_Le3, aes(x = extrem_type, y = Estimate, ymin = lower, ymax = uper, col = com_type, fill = com_type)) +
  
  # Add lineranges to represent confidence intervals
  geom_linerange(size = 6, position = position_dodge(width = 0.5), show.legend = FALSE) +
  
  # Define custom fill and color scales
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  
  # Add points for the estimates
  geom_point(size = 4.5, shape = 19, colour = "white", 
             stroke = 1, position = position_dodge(width = 0.5)) +
  
  # Customize the y-axis
  scale_y_continuous(
    breaks = c(-0.3, -0.2, -0.1, 0, 0.1),
    limits = c(-0.32, 0.12)
  ) +
  
  # Add a horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  
  # Apply a clean theme
  theme_bw() +
  
  # Define labels for the axes
  labs(
    x = NULL,
    y = expression(atop(Species~diversity~effect~(italic(theta))~on, 
                        paste(legacy~effect~(ln(italic(L)[e])))))
  ) +
  
  # Highlight different regions with semi-transparent rectangles
  annotate("rect", xmin = 0.4, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#e9e2c8") +
  annotate("rect", xmin = 3.5, xmax = 7.6, ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#bbdcc5") +
  
  # Add text labels indicating the number of plots
  geom_text(
    data = plot_num, 
    aes(x = extrem_type, y = -0.3, label = paste0("(", lbl, ")")), 
    size = 6, 
    color = "grey50"
  )

# Display the Legacy Effects bar plot
pd

# ---------------------------------------------
# 21. Preparing Combined Data for Regression Analysis
# ---------------------------------------------

# Select relevant columns and rename them for consistency
data_sin_7 = data_sin_all %>% 
  dplyr::select(species_richness, Rs_sin, Le_sin) %>% 
  rename(
    "Rs" = "Rs_sin",
    "Le" = "Le_sin"
  ) %>% 
  mutate(factor = "Individual extremes")  # Add a factor column

data_com_7 = data_com_all %>% 
  dplyr::select(species_richness, Rs_com, Le_com) %>% 
  rename(
    "Rs" = "Rs_com",
    "Le" = "Le_com"
  ) %>% 
  mutate(factor = "Compound extremes")  # Add a factor column

# Check the dimensions of the individual and compound extremes data
dim(data_sin_7); dim(data_com_7)

# Combine individual and compound extremes data into one data frame
data_combined = rbind(data_sin_7, data_com_7) 

# ---------------------------------------------
# 22. Extracting Coefficients and P-Values from Legacy Effects Models
# ---------------------------------------------

# Extract the summary of the combined "sin" Legacy Effects model
model_Le_mode_sin_total_model <- summary(model_Le_mode_sin_total)
# Extract the coefficient for 'species_richness' from the "sin" model
species_richness_coef_Le_sin <- model_Le_mode_sin_total_model$tTable["scale(log2(species_richness))", "Value"]

# Extract the summary of the combined "com" Legacy Effects model
model_Le_mode_com_total_model <- summary(model_Le_mode_com_total)
# Extract the coefficient for 'species_richness' from the "com" model
species_richness_coef_Le_com <- model_Le_mode_com_total_model$tTable["scale(log2(species_richness))", "Value"]

# Extract the p-value for 'species_richness' from the "sin" model
species_richness_p_Le_sin <- model_Le_mode_sin_total_model$tTable["scale(log2(species_richness))", "p-value"]
# Create a label for the p-value with appropriate formatting
p_value_label_Le_sin <- ifelse(
  species_richness_p_Le_sin < 0.001, "'0.001'",
  ifelse(
    species_richness_p_Le_sin < 0.01, "'0.01'",
    ifelse(species_richness_p_Le_sin < 0.05, "'0.05'", "")
  )
)

# Extract the p-value for 'species_richness' from the "com" model
species_richness_p_Le_com <- model_Le_mode_com_total_model$tTable["scale(log2(species_richness))", "p-value"]
# Create a label for the p-value with appropriate formatting
p_value_label_Le_com <- ifelse(
  species_richness_p_Le_com < 0.001, "'0.001'",
  ifelse(
    species_richness_p_Le_com < 0.01, "'0.01'",
    ifelse(species_richness_p_Le_com < 0.05, "'0.05'", "")
  )
)

# ---------------------------------------------
# 23. Creating Statistics Labels for Regression Plots
# ---------------------------------------------

# Create a data frame 'data_stats_p1' containing regression statistics for 'Rs'
data_stats_p1 <- data_combined %>%
  group_by(factor) %>%
  summarise(
    model = list(lm(log2(Rs) ~ log2(species_richness), data = cur_data())),
    .groups = 'drop'
  ) %>%
  mutate(
    summary = map(model, broom::glance),          # Extract summary statistics
    coefficients = map(model, broom::tidy),      # Extract coefficients
    df_num = map_dbl(model, ~length(coef(.x)) - 1),  # Degrees of freedom numerator
    df_den = map_dbl(model, ~df.residual(.x))        # Degrees of freedom denominator
  ) %>%
  tidyr::unnest(cols = c(summary, coefficients), names_sep = "_") %>%
  filter(coefficients_term == "log2(species_richness)") %>%  # Focus on 'species_richness' term
  select(factor, summary_r.squared, summary_statistic, summary_p.value, coefficients_estimate, df_num, df_den) %>%
  rename(
    F_value = summary_statistic, 
    slope = coefficients_estimate, 
    p_value = summary_p.value, 
    r_squared = summary_r.squared
  ) %>%
  mutate(
    # Create labels for the plots with formatted statistical values
    label = ifelse(
      p_value < 0.001, 
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) < 0.001"),
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) == ", formatC(p_value, format = "e", digits = 2))
    )
  )

# ---------------------------------------------
# 24. Defining Positions for Statistical Labels in Plot
# ---------------------------------------------

# Define the positions where the statistical labels will be placed on the plot
label_positions_p1 <- data_stats_p1 %>%
  mutate(
    x = 4.585,  # X-coordinate for the label
    y = 20      # Y-coordinate for the label
  )

# ---------------------------------------------
# 25. Creating the Regression Plot for Resistance (Rs)
# ---------------------------------------------

# Create a regression plot 'pa' showing the relationship between species richness and resistance (Rs)
pa = ggplot(data_combined, aes(x = log2(species_richness), y = log2(Rs), color = factor, fill = factor)) +
  
  # Add scatter points
  geom_point(alpha = 0.95, size = 2, shape = 19, show.legend = FALSE) + 
  
  # Add linear regression line for "Compound extremes"
  geom_smooth(
    data = filter(data_combined, factor == "Compound extremes"),
    color = "#efa112", 
    fill = "#efa112", 
    method = 'lm', 
    formula = y ~ x, 
    se = TRUE, 
    level = 0.95, 
    size = 0.8, 
    alpha = 0.6
  ) + 
  
  # Add linear regression line for "Individual extremes"
  geom_smooth(
    data = filter(data_combined, factor == "Individual extremes"),
    color = "#219a21", 
    fill = "#219a21", 
    method = 'lm', 
    formula = y ~ x, 
    se = TRUE, 
    level = 0.95, 
    size = 0.8, 
    alpha = 0.6
  ) + 
  
  # Customize the x-axis with mathematical expressions and specific breaks and labels
  scale_x_continuous(
    name = expression(Species~richness~(italic(S))), 
    limits = c(0, 4.585),
    breaks = c(0, 1, 2, 3, 4, 4.585),
    labels = c("1", "2", "4", "8", "16", "24"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # Create separate panels for "Compound extremes" and "Individual extremes"
  facet_wrap(~ fct_relevel(factor, "Compound extremes","Individual extremes"), 
             ncol = 1, strip.position = "right") +
  
  # Customize the y-axis with mathematical expressions and specific breaks
  scale_y_continuous(
    name = expression(Resistance ~ (ln(italic(R)[s]))),
    breaks = c(0, 5, 10, 15, 20),
    limits = c(-1, 21)
  ) +
  
  # Add statistical labels to the plot
  geom_text(
    data = label_positions_p1, 
    aes(x = x, y = y, label = label), 
    inherit.aes = FALSE, 
    hjust = 1, 
    size = 5.5, 
    color = "black", 
    parse = TRUE
  ) +
  
  # Define custom color and fill scales
  scale_colour_manual(values = c("#e9e2c8","#bbdcc5"), labels = c("Compound extremes","Individual extremes")) +
  scale_fill_manual(values = c("#e9e2c8","#bbdcc5"), labels = c("Compound extremes","Individual extremes")) +
  
  # Define custom shape scales
  scale_shape_discrete(labels = c("Compound extremes","Individual extremes")) +
  
  # Apply a clean theme
  theme_bw()

# Display the regression plot for Resistance (Rs)
pa

# ---------------------------------------------
# 26. Displaying Model Summaries for Legacy Effects
# ---------------------------------------------

# Display summaries of the Legacy Effects models
model_Le_mode_sin_total
model_Le_mode_com_total

# ---------------------------------------------
# 27. Extracting Coefficients and P-Values from Legacy Effects Models
# ---------------------------------------------

# Extract the summary of the combined "sin" Legacy Effects model
model_Le_mode_sin_total_model <- summary(model_Le_mode_sin_total)
# Extract the coefficient for 'species_richness' from the "sin" model
species_richness_coef_Le_sin <- model_Le_mode_sin_total_model$tTable["scale(log2(species_richness))", "Value"]

# Extract the summary of the combined "com" Legacy Effects model
model_Le_mode_com_total_model <- summary(model_Le_mode_com_total)
# Extract the coefficient for 'species_richness' from the "com" model
species_richness_coef_Le_com <- model_Le_mode_com_total_model$tTable["scale(log2(species_richness))", "Value"]

# Extract the p-value for 'species_richness' from the "sin" model
species_richness_p_Le_sin <- model_Le_mode_sin_total_model$tTable["scale(log2(species_richness))", "p-value"]
# Create a label for the p-value with appropriate formatting
p_value_label_Le_sin <- ifelse(
  species_richness_p_Le_sin < 0.001, "'0.001'",
  ifelse(
    species_richness_p_Le_sin < 0.01, "'0.01'",
    ifelse(species_richness_p_Le_sin < 0.05, "'0.05'", "")
  )
)

# Extract the p-value for 'species_richness' from the "com" model
species_richness_p_Le_com <- model_Le_mode_com_total_model$tTable["scale(log2(species_richness))", "p-value"]
# Create a label for the p-value with appropriate formatting
p_value_label_Le_com <- ifelse(
  species_richness_p_Le_com < 0.001, "'0.001'",
  ifelse(
    species_richness_p_Le_com < 0.01, "'0.01'",
    ifelse(species_richness_p_Le_com < 0.05, "'0.05'", "")
  )
)

# ---------------------------------------------
# 28. Creating Statistics Labels for Legacy Effects Regression Plots
# ---------------------------------------------

# Create a data frame 'data_stats_p2' containing regression statistics for 'Le'
data_stats_p2 <- data_combined %>%
  group_by(factor) %>%
  summarise(
    model = list(lm(log2(Le) ~ log2(species_richness), data = cur_data())),
    .groups = 'drop'
  ) %>%
  mutate(
    summary = map(model, broom::glance),          # Extract summary statistics
    coefficients = map(model, broom::tidy),      # Extract coefficients
    df_num = map_dbl(model, ~length(coef(.x)) - 1),  # Degrees of freedom numerator
    df_den = map_dbl(model, ~df.residual(.x))        # Degrees of freedom denominator
  ) %>%
  tidyr::unnest(cols = c(summary, coefficients), names_sep = "_") %>%
  filter(coefficients_term == "log2(species_richness)") %>%  # Focus on 'species_richness' term
  select(factor, summary_r.squared, summary_statistic, summary_p.value, coefficients_estimate, df_num, df_den) %>%
  rename(
    F_value = summary_statistic, 
    slope = coefficients_estimate, 
    p_value = summary_p.value, 
    r_squared = summary_r.squared
  ) %>%
  mutate(
    # Create labels for the plots with formatted statistical values
    label = ifelse(
      p_value < 0.001, 
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) < 0.001"),
      paste0("italic(F)[", df_num, "*','*", df_den, "] == ", round(F_value, 0),
             "~~italic(p) == ", formatC(p_value, format = "e", digits = 2))
    )
  )

# ---------------------------------------------
# 29. Defining Positions for Statistical Labels in Legacy Effects Plot
# ---------------------------------------------

# Define the positions where the statistical labels will be placed on the plot
label_positions_p2 <- data_stats_p2 %>%
  mutate(
    x = 4.585,  # X-coordinate for the label
    y = -20      # Y-coordinate for the label
  )

# ---------------------------------------------
# 30. Creating the Regression Plot for Legacy Effects (Le)
# ---------------------------------------------

# Create a regression plot 'pc' showing the relationship between species richness and legacy effects (Le)
pc = ggplot(data_combined, aes(x = log2(species_richness), y = log2(Le), color = factor, fill = factor)) +
  
  # Add scatter points
  geom_point(alpha = 0.95, size = 2, shape = 19, show.legend = FALSE) + 
  
  # Add linear regression line for "Compound extremes"
  geom_smooth(
    data = filter(data_combined, factor == "Compound extremes"),
    color = "#efa112", 
    fill = "#efa112", 
    method = 'lm', 
    formula = y ~ x, 
    se = TRUE, 
    level = 0.95, 
    size = 0.8, 
    alpha = 0.6
  ) + 
  
  # Add linear regression line for "Individual extremes"
  geom_smooth(
    data = filter(data_combined, factor == "Individual extremes"),
    color = "#219a21", 
    fill = "#219a21", 
    method = 'lm', 
    formula = y ~ x, 
    se = TRUE, 
    level = 0.95, 
    size = 0.8, 
    alpha = 0.6
  ) + 
  
  # Customize the x-axis with mathematical expressions and specific breaks and labels
  scale_x_continuous(
    name = expression(Species~richness~(italic(S))), 
    limits = c(0, 4.585),
    breaks = c(0, 1, 2, 3, 4, 4.585),
    labels = c("1", "2", "4", "8", "16", "24"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # Create separate panels for "Compound extremes" and "Individual extremes"
  facet_wrap(~ fct_relevel(factor, "Compound extremes","Individual extremes"), 
             ncol = 1, strip.position = "right") +
  
  # Customize the y-axis with mathematical expressions and specific breaks
  scale_y_continuous(
    name = expression(Legacy~effect~(ln(italic(L)[e]))),
    breaks = c(-20, -15, -10, -5, 0),
    limits = c(-21, 1)
  ) +
  
  # Add statistical labels to the plot
  geom_text(
    data = label_positions_p2, 
    aes(x = x, y = y, label = label), 
    inherit.aes = FALSE, 
    hjust = 1, 
    size = 5.5, 
    color = "black", 
    parse = TRUE
  ) +
  
  # Define custom color and fill scales
  scale_colour_manual(values = c("#e9e2c8","#bbdcc5"), labels = c("Compound extremes","Individual extremes")) +
  scale_fill_manual(values = c("#e9e2c8","#bbdcc5"), labels = c("Compound extremes","Individual extremes")) +
  
  # Define custom shape scales
  scale_shape_discrete(labels = c("Compound extremes","Individual extremes")) +
  
  # Apply a clean theme
  theme_bw()

# Display the regression plot for Legacy Effects (Le)