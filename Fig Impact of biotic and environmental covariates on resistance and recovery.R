########
# ---------------------------------------------
# 1. Building Linear Mixed-Effects Model for Legacy Effects (Compound Extremes)
# ---------------------------------------------

# Fit a linear mixed-effects model predicting log-transformed Legacy Effects (Le_com)
# based on scaled predictors: species richness, stand age (STDAGE), elevation (ELEV),
# pH, bulk density, total nitrogen (totaln), clay content, precipitation (pre),
# temperature (tem), and resistance (Rs_com).
# Random effects are specified for ECOSUBCD nested within plot_ID.
# An exponential correlation structure is used based on longitude (LON) and latitude (LAT).
model_Le_mode_com_total <- lme(
  log2(Le_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE)  + scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem) + 
    scale(log2(Rs_com)),
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_com_all
)

# Calculate Variance Inflation Factors (VIF) to check for multicollinearity among predictors
vif(model_Le_mode_com_total)

# Display a summary of the fitted model, including coefficients, standard errors, and p-values
summary(model_Le_mode_com_total) 

# ---------------------------------------------
# 2. Extracting and Cleaning Model Coefficients for Compound Extremes
# ---------------------------------------------

# Extract the table of coefficients from the model summary
# Convert it to a data frame and retain row names as a separate column named "factor"
# Filter out the intercept term
# Clean the factor names by removing scaling and log transformations
# Add a new column to indicate the extreme type as "Compound extremes"
# Calculate lower and upper confidence intervals using 1.96 * standard error
model_com = summary(model_Le_mode_com_total)$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "factor") %>%
  filter(factor != "(Intercept)") %>%
  mutate(
    factor = gsub("scale\\(|\\)", "", factor),  # Remove "scale(" and ")" from factor names
    factor = gsub("log2\\(|", "", factor)       # Remove "log2(" from factor names
  ) %>%
  mutate(extreme.type = "Compound extremes") %>%  # Assign extreme type
  mutate(
    conf.low = Value - 1.96 * Std.Error,          # Calculate lower 95% confidence interval
    conf.high = Value + 1.96 * Std.Error          # Calculate upper 95% confidence interval
  )  

########
# ---------------------------------------------
# 3. Building Linear Mixed-Effects Model for Legacy Effects (Individual Extremes)
# ---------------------------------------------

# Fit a linear mixed-effects model predicting log-transformed Legacy Effects (Le_sin)
# based on scaled predictors: species richness, stand age (STDAGE), elevation (ELEV),
# pH, bulk density, total nitrogen (totaln), clay content, precipitation (pre),
# temperature (tem), and resistance (Rs_sin).
# Random effects are specified for ECOSUBCD nested within plot_ID.
# An exponential correlation structure is used based on longitude (LON) and latitude (LAT).
model_Le_mode_sin_total <- lme(
  log2(Le_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE) + scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem) + 
    scale(log2(Rs_sin)), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_sin_all
)

# Calculate Variance Inflation Factors (VIF) to check for multicollinearity among predictors
vif(model_Le_mode_sin_total)

# Display a summary of the fitted model, including coefficients, standard errors, and p-values
summary(model_Le_mode_sin_total) 

# ---------------------------------------------
# 4. Extracting and Cleaning Model Coefficients for Individual Extremes
# ---------------------------------------------

# Extract the table of coefficients from the model summary
# Convert it to a data frame and retain row names as a separate column named "factor"
# Filter out the intercept term
# Clean the factor names by removing scaling and log transformations
# Add a new column to indicate the extreme type as "Individual extremes"
# Calculate lower and upper confidence intervals using 1.96 * standard error
model_sin = summary(model_Le_mode_sin_total)$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "factor") %>%
  filter(factor != "(Intercept)") %>%
  mutate(
    factor = gsub("scale\\(|\\)", "", factor),  # Remove "scale(" and ")" from factor names
    factor = gsub("log2\\(|", "", factor)       # Remove "log2(" from factor names
  ) %>%
  mutate(extreme.type = "Individual extremes") %>%  # Assign extreme type
  mutate(
    conf.low = Value - 1.96 * Std.Error,          # Calculate lower 95% confidence interval
    conf.high = Value + 1.96 * Std.Error          # Calculate upper 95% confidence interval
  )  

########
# ---------------------------------------------
# 5. Combining Model Coefficients for All Extremes
# ---------------------------------------------

# Combine the compound and individual extremes coefficients into a single data frame
model_total_Le = rbind(model_com, model_sin) %>%
  mutate(
    # Categorize predictors into groups based on their nature
    group = case_when(
      factor %in% c("ELEV") ~ "Geography",
      factor %in% c("ph", "totaln", "bulk", "clay") ~ "Soil properties",
      factor %in% c("pre", "tem") ~ "Climatic conditions",
      factor %in% c("STDAGE", "tree_count", "species_richness") ~ "Plot attributes",
      factor %in% c("Rs_com","Rs_sin") ~ "Buffer capacity",
      TRUE ~ "Other"
    ),
    # Provide more descriptive names for each factor
    factor = case_when(
      factor == "ELEV" ~ "Elevation",
      
      factor == "ph" ~ "pH",
      factor == "totaln" ~ "Total nitrogen content",
      factor == "bulk" ~ "Bulk density",
      factor == "clay" ~ "Clay content",
      
      factor == "species_richness" ~ "Species richness",
      factor == "STDAGE" ~ "Stand age",
      
      factor == "pre" ~ "Mean annual precipitation",
      factor == "tem" ~ "Mean annual temperature",
      factor == "Rs_com" ~ "Resistance",
      factor == "Rs_sin" ~ "Resistance",
      TRUE ~ factor
    )
  )

# ---------------------------------------------
# 6. Setting Factor Levels for Plotting
# ---------------------------------------------

# Define the order of groups for consistent plotting
model_total_Le$group <- factor(
  model_total_Le$group, 
  levels = c("Climatic conditions", "Soil properties", "Geography", "Plot attributes", "Buffer capacity")
)

# Define the order of factors within each group for consistent plotting
model_total_Le$factor <- factor(
  model_total_Le$factor, 
  levels = c(
    "Species richness", "Stand age", 
    "Mean annual precipitation", "Mean annual temperature",
    "pH", "Bulk density", "Total nitrogen content", "Clay content",
    "Elevation", "Resistance"
  )
)

# Define the order of extreme types for consistent plotting
model_total_Le$extreme.type <- factor(
  model_total_Le$extreme.type, 
  levels = c("Compound extremes", "Individual extremes")
)

# ---------------------------------------------
# 7. Filtering Non-Significant Effects
# ---------------------------------------------

# Filter out predictors with p-values greater than or equal to 0.05 (non-significant)
model_total_Le_result = model_total_Le %>%
  filter(`p-value` >= 0.05)

# Display the filtered results
model_total_Le_result

# Extract specific subsets for potential annotation or further analysis
# Example: Extract the "Bulk density" predictor for "Compound extremes"
c1 <- subset(
  model_total_Le, 
  factor == "Bulk density" & extreme.type == "Compound extremes" & group == "Soil properties"
)

# Example: Extract the "Clay content" predictor for "Compound extremes"
c2 <- subset(
  model_total_Le, 
  factor == "Clay content" & extreme.type == "Compound extremes" & group == "Soil properties"
)

# View the first few rows of the combined Legacy Effects data
head(model_total_Le)

# ---------------------------------------------
# 8. Creating the Bar Plot for Legacy Effects (Le)
# ---------------------------------------------

# Define custom colors for the bars and points in the plot
barCOLS = c("#efa112", "#219a21")
dotCOLS = c("#f5c266", "#66b966")

# Create a bar plot 'p_b' showing the estimates and confidence intervals for legacy effects
p_b <- ggplot(model_total_Le, aes(
  x = factor, 
  y = Value, 
  ymin = conf.low, 
  ymax = conf.high,
  color = extreme.type, 
  fill = extreme.type
)) + 
  # Add lineranges to represent confidence intervals
  geom_linerange(
    size = 5.2,
    position = position_dodge(width = 0.5), 
    show.legend = FALSE
  ) +
  # Define custom fill and color scales
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  # Add points for the estimates
  geom_point(
    size = 3.2, 
    shape = 19, 
    colour = "white", 
    stroke = 0.75, 
    position = position_dodge(width = 0.5)
  ) +
  # Add a horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  # Remove x-axis label
  xlab(NULL) +
  # Apply a clean theme
  theme_bw() +
  # Add a title to the plot using mathematical expressions
  labs(title = expression(Legacy~effect~(ln(italic(L)[e])))) +
  # Highlight different regions with semi-transparent rectangles
  annotate(
    "rect", 
    xmin = 1.5, xmax = 2.5, 
    ymin = -0.6, ymax = 0.3, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) + 
  annotate(
    "rect", 
    xmin = 3.5, xmax = 4.5, 
    ymin = -0.6, ymax = 0.3, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) +  
  annotate(
    "rect", 
    xmin = 5.5, xmax = 6.5, 
    ymin = -0.6, ymax = 0.3, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) +  
  annotate(
    "rect", 
    xmin = 7.5, xmax = 8.5, 
    ymin = -0.6, ymax = 0.3, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) 

# Display the Legacy Effects bar plot
p_b

# ---------------------------------------------
# 9. Building Linear Mixed-Effects Model for Resistance (Rs) (Compound Extremes)
# ---------------------------------------------

# Fit a linear mixed-effects model predicting log-transformed Resistance (Rs_com)
# based on scaled predictors: species richness, stand age (STDAGE), elevation (ELEV),
# pH, bulk density, total nitrogen (totaln), clay content, precipitation (pre),
# and temperature (tem).
# Random effects are specified for ECOSUBCD nested within plot_ID.
# An exponential correlation structure is used based on longitude (LON) and latitude (LAT).
model_Rs_mode_com_total <- lme(
  log2(Rs_com) ~ scale(log2(species_richness)) + 
    scale(STDAGE) + scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem),
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_com_all
)

# Calculate Variance Inflation Factors (VIF) to check for multicollinearity among predictors
vif(model_Rs_mode_com_total)

# Display a summary of the fitted model, including coefficients, standard errors, and p-values
summary(model_Rs_mode_com_total) 

# ---------------------------------------------
# 10. Extracting and Cleaning Model Coefficients for Resistance (Compound Extremes)
# ---------------------------------------------

# Extract the table of coefficients from the model summary
# Convert it to a data frame and retain row names as a separate column named "factor"
# Filter out the intercept term
# Clean the factor names by removing scaling and log transformations
# Add a new column to indicate the extreme type as "Compound extremes"
# Calculate lower and upper confidence intervals using 1.96 * standard error
model_com = summary(model_Rs_mode_com_total)$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "factor") %>%
  filter(factor != "(Intercept)") %>%
  mutate(
    factor = gsub("scale\\(|\\)", "", factor),  # Remove "scale(" and ")" from factor names
    factor = gsub("log2\\(|", "", factor)       # Remove "log2(" from factor names
  ) %>%
  mutate(extreme.type = "Compound extremes") %>%  # Assign extreme type
  mutate(
    conf.low = Value - 1.96 * Std.Error,          # Calculate lower 95% confidence interval
    conf.high = Value + 1.96 * Std.Error          # Calculate upper 95% confidence interval
  )  

#
# ---------------------------------------------
# 11. Building Linear Mixed-Effects Model for Resistance (Rs) (Individual Extremes)
# ---------------------------------------------

# Fit a linear mixed-effects model predicting log-transformed Resistance (Rs_sin)
# based on scaled predictors: species richness, stand age (STDAGE), elevation (ELEV),
# pH, bulk density, total nitrogen (totaln), clay content, precipitation (pre),
# and temperature (tem).
# Random effects are specified for ECOSUBCD nested within plot_ID.
# An exponential correlation structure is used based on longitude (LON) and latitude (LAT).
model_Rs_mode_sin_total <- lme(
  log2(Rs_sin) ~ scale(log2(species_richness)) + 
    scale(STDAGE) + scale(ELEV) + 
    scale(ph) +  scale(bulk) + scale(totaln) + scale(clay) +
    scale(pre) + scale(tem), 
  random = ~ 1 | ECOSUBCD/plot_ID, 
  correlation = corExp(form = ~ LON + LAT), 
  data = data_sin_all
)

# Calculate Variance Inflation Factors (VIF) to check for multicollinearity among predictors
vif(model_Rs_mode_sin_total)

# Display a summary of the fitted model, including coefficients, standard errors, and p-values
summary(model_Rs_mode_sin_total) 

# ---------------------------------------------
# 12. Extracting and Cleaning Model Coefficients for Resistance (Individual Extremes)
# ---------------------------------------------

# Extract the table of coefficients from the model summary
# Convert it to a data frame and retain row names as a separate column named "factor"
# Filter out the intercept term
# Clean the factor names by removing scaling and log transformations
# Add a new column to indicate the extreme type as "Individual extremes"
# Calculate lower and upper confidence intervals using 1.96 * standard error
model_sin = summary(model_Rs_mode_sin_total)$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "factor") %>%
  filter(factor != "(Intercept)") %>%
  mutate(
    factor = gsub("scale\\(|\\)", "", factor),  # Remove "scale(" and ")" from factor names
    factor = gsub("log2\\(|", "", factor)       # Remove "log2(" from factor names
  ) %>%
  mutate(extreme.type = "Individual extremes") %>%  # Assign extreme type
  mutate(
    conf.low = Value - 1.96 * Std.Error,          # Calculate lower 95% confidence interval
    conf.high = Value + 1.96 * Std.Error          # Calculate upper 95% confidence interval
  )  

# ---------------------------------------------
# 13. Combining Model Coefficients for All Extremes (Resistance)
# ---------------------------------------------

# Combine the compound and individual extremes coefficients into a single data frame
model_total_Rs = rbind(model_com, model_sin) %>%
  mutate(
    # Categorize predictors into groups based on their nature
    group = case_when(
      factor %in% c("ELEV") ~ "Geography", 
      factor %in% c("ph", "totaln", "bulk", "clay") ~ "Soil properties",
      factor %in% c("pre", "tem") ~ "Climatic conditions",
      factor %in% c("STDAGE", "tree_count", "species_richness") ~ "Plot attributes", 
      TRUE ~ "Other"
    ),
    # Provide more descriptive names for each factor
    factor = case_when(
      factor == "ELEV" ~ "Elevation",
      
      factor == "ph" ~ "pH",
      factor == "totaln" ~ "Total nitrogen content",
      factor == "bulk" ~ "Bulk density",
      factor == "clay" ~ "Clay content",
      
      factor == "species_richness" ~ "Species richness",
      factor == "STDAGE" ~ "Stand age",
      
      factor == "pre" ~ "Mean annual precipitation",
      factor == "tem" ~ "Mean annual temperature",
      factor == "Rs_com" ~ "Resistance",
      factor == "Rs_sin" ~ "Resistance",
      TRUE ~ factor
    )
  )

# ---------------------------------------------
# 14. Setting Factor Levels for Plotting (Resistance)
# ---------------------------------------------

# Define the order of groups for consistent plotting
model_total_Rs$group <- factor(
  model_total_Rs$group, 
  levels = c("Climatic conditions", "Soil properties", "Geography", "Plot attributes", "Buffer capacity")
)

# Define the order of factors within each group for consistent plotting
model_total_Rs$factor <- factor(
  model_total_Rs$factor, 
  levels = c(
    "Species richness", "Stand age",
    "Mean annual precipitation", "Mean annual temperature",
    "pH", "Bulk density", "Total nitrogen content", "Clay content",
    "Elevation"
  )
)

# Define the order of extreme types for consistent plotting
model_total_Rs$extreme.type <- factor(
  model_total_Rs$extreme.type, 
  levels = c("Compound extremes", "Individual extremes")
)

# ---------------------------------------------
# 15. Filtering Non-Significant Effects (Resistance)
# ---------------------------------------------

# Filter out predictors with p-values greater than or equal to 0.05 (non-significant)
model_total_Rs_result = model_total_Rs %>%
  filter(`p-value` >= 0.05)

# Display the filtered results
model_total_Rs_result

# Extract specific subsets for potential annotation or further analysis
# Example: Extract the "Clay content" predictor for "Individual extremes" in "Soil properties"
R1 <- subset(
  model_total_Rs, 
  factor == "Clay content" & extreme.type == "Individual extremes" & group == "Soil properties"
)

# View the first few rows of the combined Resistance data
head(model_total_Rs)

# ---------------------------------------------
# 16. Creating the Bar Plot for Resistance (Rs)
# ---------------------------------------------

# Define custom colors for the bars and points in the plot
barCOLS = c("#efa112", "#219a21")
dotCOLS = c("#f5c266", "#66b966")

# Create a bar plot 'p_a' showing the estimates and confidence intervals for resistance effects
p_a <- ggplot(model_total_Rs, aes(
  x = factor, 
  y = Value, 
  ymin = conf.low, 
  ymax = conf.high,
  color = extreme.type, 
  fill = extreme.type
)) + 
  # Add lineranges to represent confidence intervals
  geom_linerange(
    size = 5.2,
    position = position_dodge(width = 0.5), 
    show.legend = FALSE
  ) +
  # Define custom fill and color scales
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  # Add points for the estimates
  geom_point(
    size = 3.2, 
    shape = 19, 
    colour = "white", 
    stroke = 0.75, 
    position = position_dodge(width = 0.5)
  ) +
  # Add a horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  # Remove x-axis label
  xlab(NULL) +
  # Apply a clean theme
  theme_bw() +
  # Add a title to the plot using mathematical expressions
  labs(title = expression(Resistance~(ln(italic(R)[s])))) +
  # Customize the y-axis with specific breaks and limits
  scale_y_continuous(
    name = "Parameter estimates",
    limits = c(-0.62, 0.62),
    breaks = c(-0.6, -0.3, 0, 0.3, 0.6),
    expand = c(0, 0)
  ) +
  # Highlight different regions with semi-transparent rectangles
  annotate(
    "rect", 
    xmin = 1.5, xmax = 2.5, 
    ymin = -0.60, ymax = 0.60, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) + 
  annotate(
    "rect", 
    xmin = 3.5, xmax = 4.5, 
    ymin = -0.60, ymax = 0.60, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) +  
  annotate(
    "rect", 
    xmin = 5.5, xmax = 6.5, 
    ymin = -0.60, ymax = 0.60, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) +  
  annotate(
    "rect", 
    xmin = 7.5, xmax = 8.5, 
    ymin = -0.60, ymax = 0.60, 
    alpha = 0.2, 
    fill = "lightgrey"
  ) 

# Display the Resistance bar plot
p_a
