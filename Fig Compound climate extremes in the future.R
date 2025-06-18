# Load necessary libraries
library(dplyr)       # Data manipulation
library(nlme)        # Linear and nonlinear mixed effects models
library(ggplot2)     # Data visualization
library(ggpubr)      # 'ggplot2' based publication ready plots
library(cowplot)     # Plot layout and annotation
library(ggnewscale)  # Add new color scales to ggplot
library(sp)          # Spatial data classes and methods
library(raster)      # Raster data manipulation
library(ncdf4)       # Interface to 'NetCDF' format
library(tidyverse)   # Collection of R packages for data science
library(stringr)     # String manipulation

# Load libraries again (redundant, can be removed)
library(dplyr)
library(ggpubr)
library(cowplot)

# Convert the 'year' column in com_126 to numeric
com_126$year = as.numeric(com_126$year)

# Summarize com_126 data by year
com_126_num = com_126 %>%
  group_by(year) %>% 
  summarise(com_num = as.numeric(n() / total_plotnum) * 100) %>%  # Calculate percentage
  dplyr::mutate_if(is.numeric, round, 0) %>%  # Round numeric columns to 0 decimals
  mutate(spp_type = "SSP126") %>%  # Add species type column
  filter(year >= 2021)  # Filter years from 2021 onwards

# Display dimensions and first few rows of com_126_num
dim(com_126); dim(com_126_num); head(com_126_num)

# Plot com_num against year for com_126_num
plot(com_126_num$year, com_126_num$com_num)

# Repeat the same process for com_245 dataset
com_245$year = as.numeric(com_245$year)
com_245_num = com_245 %>%
  group_by(year) %>% 
  summarise(com_num = as.numeric(n() / total_plotnum) * 100) %>% 
  dplyr::mutate_if(is.numeric, round, 0) %>%  
  mutate(spp_type = "SSP245") %>%
  filter(year >= 2021)
dim(com_245); dim(com_245_num); head(com_245_num)

# Repeat the same process for com_585 dataset
com_585$year = as.numeric(com_585$year)
com_585_num = com_585 %>%
  group_by(year) %>% 
  summarise(com_num = as.numeric(n() / total_plotnum) * 100) %>% 
  dplyr::mutate_if(is.numeric, round, 0) %>%  
  mutate(spp_type = "SSP585") %>%
  filter(year >= 2021)
dim(com_585); dim(com_585_num); head(com_585_num)

# Summarize com_126 data for years up to 2020
based_point = com_126 %>%
  group_by(year) %>% 
  summarise(com_num = as.numeric(n() / total_plotnum) * 100) %>% 
  dplyr::mutate_if(is.numeric, round, 0) %>%  
  mutate(spp_type = "SSP126") %>%
  filter(year <= 2020)

# Combine all summarized data into one dataframe
com_num_all = bind_rows(com_126_num, com_245_num, com_585_num) 

# Display dimensions of individual and combined dataframes
dim(com_126_num); dim(com_245_num); dim(com_585_num); dim(com_num_all)

# Display first few rows of the combined dataframe
head(com_num_all)

# Create a dataframe for rectangular annotations in the plot
trs_c <- data.frame(x = c(1905, 1905, 2095, 2095), y = c(52, 61, 61, 52))

# Calculate mean composite numbers for each scenario
data_b_126 = mean(com_126_num$com_num)
data_b_245 = mean(com_245_num$com_num)
data_b_585 = mean(com_585_num$com_num)

# Filter based on_point data for years from 2000 onwards
based_point2 = based_point %>%
  filter(year >= 2000)

# Calculate mean composite number for based_point2
data_b_based_point2 = mean(based_point2$com_num)

# Calculate the mean and standard deviation of com_num every 10 years for based_point
based_point_summary <- based_point %>%
  mutate(year_group = (year - 1901) %/% 10 ) %>%  # Create 10-year groups
  group_by(year_group) %>%
  summarise(
    start_year = min(year),
    end_year = max(year),
    year = (start_year + end_year - 1) / 2,  # Calculate midpoint year
    mean_com_num = mean(com_num),
    sd_com_num = sd(com_num)
  )

# Calculate the mean and standard deviation of com_num every 10 years for all scenarios
com_num_all_summary <- com_num_all %>%
  mutate(year_group = (year - 1901) %/% 10 ) %>%  # Create 10-year groups
  group_by(year_group, spp_type) %>%
  summarise(
    start_year = min(year),
    end_year = max(year),
    year = (start_year + end_year - 1) / 2,  # Calculate midpoint year
    mean_com_num = mean(com_num),
    sd_com_num = sd(com_num)
  )

# Create datasets for dashed lines in the plot for SSP126 scenario
data_c_obs = based_point_summary %>%
  filter(year == 2015) %>%
  pull(mean_com_num)

data_c_126 = com_num_all_summary %>%
  filter(year == 2025, spp_type == "SSP126") %>%
  pull(mean_com_num)

c_data_126 <- data.frame(
  x = c(2015, 2025),
  y = c(data_c_obs, data_c_126)
)

# Create datasets for dashed lines in the plot for SSP245 scenario
data_c_245 = com_num_all_summary %>%
  filter(year == 2025, spp_type == "SSP245") %>%
  pull(mean_com_num)

c_data_245 <- data.frame(
  x = c(2015, 2025),
  y = c(data_c_obs, data_c_245)
)

# Create datasets for dashed lines in the plot for SSP585 scenario
data_c_585 = com_num_all_summary %>%
  filter(year == 2025, spp_type == "SSP585") %>%
  pull(mean_com_num)

c_data_585 <- data.frame(
  x = c(2015, 2025),
  y = c(data_c_obs, data_c_585)
)

# Rename species types for better readability
com_num_all_summary <- com_num_all_summary %>%
  mutate(spp_type = case_when(
    spp_type == "SSP126" ~ "SSP1-2.6",
    spp_type == "SSP245" ~ "SSP2-4.5",
    spp_type == "SSP585" ~ "SSP5-8.5",
    TRUE ~ spp_type
  ))

# Set the order of species types
com_num_all_summary$spp_type <- factor(com_num_all_summary$spp_type, 
                                       levels = c("SSP1-2.6",
                                                  "SSP2-4.5",
                                                  "SSP5-8.5"))

# Create the first plot (p_c)
p_c = ggplot(com_num_all_summary, aes(x = year)) + 
  # Add shaded rectangles for different historical periods
  annotate("rect", xmin = 1900, xmax = 1920, ymin = -2, ymax = 50, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 1940, xmax = 1960, ymin = -2, ymax = 50, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 1980, xmax = 2000, ymin = -2, ymax = 50, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 2020, xmax = 2040, ymin = -2, ymax = 50, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 2060, xmax = 2080, ymin = -2, ymax = 50, alpha = 0.20, fill = "lightgrey") + 
  # Add dashed lines for different SSP scenarios
  geom_line(data = c_data_126, aes(x = x, y = y), size = 0.6, color = "#8DB3D2") +  
  geom_line(data = c_data_245, aes(x = x, y = y), size = 0.6, color = "#3D5B92") +  
  geom_line(data = c_data_585, aes(x = x, y = y), size = 0.6, color = "#C74A45") +  
  # Plot mean composite numbers for each scenario
  geom_line(aes(y = mean_com_num, color = spp_type), size = 0.6) + 
  # Plot historical mean composite numbers
  geom_line(data = based_point_summary, aes(y = mean_com_num), color = "grey60", size = 0.6, show.legend = FALSE) + 
  # Label for x-axis
  xlab("Years") + 
  # Define y-axis scale
  scale_y_continuous(
    name = "Proportion of\ncompound extremes extent (% yr⁻¹)",
    limits = c(-4, 62),
    breaks = c(0, 20, 40, 60), 
    expand = c(0, 0)
  ) + 
  # Define x-axis scale with custom labels
  scale_x_continuous(
    name = "Years",
    limits = c(1895, 2100),
    breaks = c(
      1910, 1930, 1950, 1970, 1990,
      2010, 2030, 2050, 2070, 2090
    ),
    labels = c(
      "1901-1920", "1921-1940", "1941-1960", "1961-1980", "1981-2000",
      "2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"
    ),
    expand = c(0, 0)
  ) +
  # Define color and fill scales manually
  scale_colour_manual(values = c("#8DB3D2", "#3D5B92", "#C74A45")) +
  scale_fill_manual(values = c("#8DB3D2", "#3D5B92", "#C74A45")) +
  # Apply a theme
  theme_bw() +
  # Add annotations for different periods
  annotate(geom = "text", x = 1950, y = 57, label = "Historical records\n1901-2000", color = "black", size = 4, alpha = 0.6) + 
  annotate(geom = "text", x = 2011, y = 57, label = "Current observations\n2001-2020", color = "black", size = 4, alpha = 0.6) + 
  annotate(geom = "text", x = 2065, y = 57, label = "Future projections\n2021-2100", color = "black", size = 4, alpha = 0.6)

# Display the first plot
p_c

### Plot d ###

# Process for SSP126 scenario
# Convert 'year' to numeric
com_126$year = as.numeric(com_126$year)
com_126_fre = com_126

# Calculate frequency counts for different future periods
k_126_1 = com_126_fre %>%  
  filter(year < 2040 & year >= 2021) %>%  
  group_by(pltID) %>%
  summarize("2030" = n()) 

k_126_2 = com_126_fre %>%  
  filter(year < 2060 & year >= 2041) %>%  
  group_by(pltID) %>%
  summarize("2050" = n()) 

k_126_3 = com_126_fre %>%  
  filter(year < 2080 & year >= 2061) %>%  
  group_by(pltID) %>%
  summarize("2070" = n()) 

k_126_4 = com_126_fre %>%  
  filter(year < 2100 & year >= 2081) %>%  
  group_by(pltID) %>%
  summarize("2090" = n()) 

# Calculate frequency counts for historical period
k_126_mean = com_126_fre %>%  
  filter(year < 2020 & year >= 2001) %>%  
  group_by(pltID) %>%
  summarize("fre" = n())
k_126_mean$fre

# Calculate frequency counts for all future periods
k_126_future = com_126_fre %>%  
  filter(year < 2100 & year >= 2021) %>%  
  group_by(pltID) %>%
  summarize("fre" = n())
k_126_future$fre

# Merge frequency data with main dataset and replace NA with 0
data_126 = data1 %>% 
  dplyr::select(contains("pltID")) %>% 
  left_join(k_126_1, by = "pltID") %>% 
  left_join(k_126_2, by = "pltID") %>% 
  left_join(k_126_3, by = "pltID") %>% 
  left_join(k_126_4, by = "pltID") %>% 
  replace(is.na(.), 0)

# Display head and dimensions of data_126
head(data_126); dim(data_126) 

# Calculate mean frequencies for each period
data_126_mean <- data_126 %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_mean" = "V1")

# Calculate standard deviation of frequencies and merge with means
data_126_all <- data_126 %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), sd)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_sd" = "V1") %>%
  left_join(data_126_mean, by = "year") %>%  
  mutate(spp_type = "SSP126") 

# Repeat the above process for SSP245 scenario
com_245$year = as.numeric(com_245$year)
com_245_fre = com_245

k_245_1 = com_245_fre %>%  
  filter(year < 2040 & year >= 2021) %>%  
  group_by(pltID) %>%
  summarize("2030" = n()) 

k_245_2 = com_245_fre %>%  
  filter(year < 2060 & year >= 2041) %>%  
  group_by(pltID) %>%
  summarize("2050" = n()) 

k_245_3 = com_245_fre %>%  
  filter(year < 2080 & year >= 2061) %>%  
  group_by(pltID) %>%
  summarize("2070" = n())

k_245_4 = com_245_fre %>%  
  filter(year < 2100 & year >= 2081) %>%  
  group_by(pltID) %>%
  summarize("2090" = n())

k_245_mean = com_245_fre %>%  
  filter(year < 2020 & year >= 2001) %>%  
  group_by(pltID) %>%
  summarize("fre" = n())
k_245_mean$fre

k_245_future = com_245_fre %>%  
  filter(year < 2100 & year >= 2021) %>%  
  group_by(pltID) %>%
  summarize("fre" = n())
k_245_future$fre

# Merge frequency data with main dataset for SSP245 and replace NA with 0
data_245 = data1 %>% 
  dplyr::select(contains("pltID")) %>% 
  left_join(k_245_1, by = "pltID") %>% 
  left_join(k_245_2, by = "pltID") %>% 
  left_join(k_245_3, by = "pltID") %>% 
  left_join(k_245_4, by = "pltID") %>% 
  replace(is.na(.), 0)

# Display head and dimensions of data_245
head(data_245); dim(data_245) 

# Calculate mean frequencies for SSP245
data_245_mean <- data_245 %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_mean" = "V1")

# Calculate standard deviation of frequencies and merge with means for SSP245
data_245_all <- data_245 %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), sd)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_sd" = "V1") %>%
  left_join(data_245_mean, by = "year") %>%  
  mutate(spp_type = "SSP245") 

# Repeat the process for SSP585 scenario
com_585$year = as.numeric(com_585$year)
com_585_fre = com_585

k_585_1 = com_585_fre %>%  
  filter(year < 2040 & year >= 2021) %>%  
  group_by(pltID) %>%
  summarize("2030" = n()) 

k_585_2 = com_585_fre %>%  
  filter(year < 2060 & year >= 2041) %>%  
  group_by(pltID) %>%
  summarize("2050" = n()) 

k_585_3 = com_585_fre %>%  
  filter(year < 2080 & year >= 2061) %>%  
  group_by(pltID) %>%
  summarize("2070" = n()) 

k_585_4 = com_585_fre %>%  
  filter(year < 2100 & year >= 2081) %>%  
  group_by(pltID) %>%
  summarize("2090" = n()) 

k_585_mean = com_585_fre %>%  
  filter(year < 2020 & year >= 2001) %>%  
  group_by(pltID) %>%
  summarize("fre" = n())
k_585_mean$fre

k_585_future = com_585_fre %>%  
  filter(year < 2100 & year >= 2021) %>%  
  group_by(pltID) %>%
  summarize("fre" = n())
k_585_future$fre

# Merge frequency data with main dataset for SSP585 and replace NA with 0
data_585 = data1 %>% 
  dplyr::select(contains("pltID")) %>% 
  left_join(k_585_1, by = "pltID") %>% 
  left_join(k_585_2, by = "pltID") %>% 
  left_join(k_585_3, by = "pltID") %>% 
  left_join(k_585_4, by = "pltID") %>% 
  replace(is.na(.), 0)

# Display head and dimensions of data_585
head(data_585); dim(data_585)

# Calculate mean frequencies for SSP585
data_585_mean <- data_585 %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_mean" = "V1")

# Calculate standard deviation of frequencies and merge with means for SSP585
data_585_all <- data_585 %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), sd)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_sd" = "V1") %>%
  left_join(data_585_mean, by = "year") %>%  
  mutate(spp_type = "SSP585") 

# Process historical data
# Calculate frequency counts for different historical periods
k_his_1 = com_126_fre %>%  
  filter(year < 1920 & year >= 1901) %>%  
  group_by(pltID) %>%
  summarize("1910" = n()) 

k_his_2 = com_126_fre %>%  
  filter(year < 1940 & year >= 1921) %>%  
  group_by(pltID) %>%
  summarize("1930" = n()) 

k_his_3 = com_126_fre %>%  
  filter(year < 1960 & year >= 1941) %>%  
  group_by(pltID) %>%
  summarize("1950" = n()) 

k_his_4 = com_126_fre %>%  
  filter(year < 1980 & year >= 1961) %>%  
  group_by(pltID) %>%
  summarize("1970" = n()) 

k_his_5 = com_126_fre %>%  
  filter(year < 2000 & year >= 1981) %>%  
  group_by(pltID) %>%
  summarize("1990" = n()) 

k_his_6 = com_126_fre %>%  
  filter(year < 2020 & year >= 2001) %>%  
  group_by(pltID) %>%
  summarize("2010" = n())

# Merge historical frequency data with main dataset and replace NA with 0
data_his = data1 %>% 
  dplyr::select(contains("pltID")) %>% 
  left_join(k_his_1, by = "pltID") %>% 
  left_join(k_his_2, by = "pltID") %>% 
  left_join(k_his_3, by = "pltID") %>% 
  left_join(k_his_4, by = "pltID") %>%  
  left_join(k_his_5, by = "pltID") %>%  
  left_join(k_his_6, by = "pltID") %>% 
  replace(is.na(.), 0)

# Display head and dimensions of historical data
head(data_his); dim(data_his)

# Calculate mean frequencies for historical data
data_his_mean <- data_his %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_mean" = "V1")

# Calculate standard deviation of frequencies and merge with means for historical data
data_his_all <- data_his %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), sd)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") %>%
  rename("frequency_sd" = "V1") %>%
  left_join(data_his_mean, by = "year") %>%  
  mutate(spp_type = "his") 

# Combine all frequency data
data_his_all$year = as.numeric(data_his_all$year)
com_frequency_all = bind_rows(data_126_all, data_245_all, data_585_all) 

# Display dimensions of all frequency data
dim(data_126_all); dim(data_245_all); dim(data_585_all); dim(com_frequency_all)

# Display first few rows of the combined frequency data
head(com_frequency_all)

# Create a dataframe for rectangular annotations in the second plot
trs_d <- data.frame(x = c(1905, 1905, 1995, 1995), y = c(-0.6, 12.6, 12.6, -0.6))

# Create datasets for dashed lines in the second plot for SSP126 scenario
data_line_obs = data_his_all[data_his_all$year == 2010, ][, 3]
data_line_126 = data_126_all[data_126_all$year == 2030, ][, 3]
line_data_126 <- data.frame(
  x = c(2010, 2026),
  y = c(data_line_obs, data_line_126)
)

# Create datasets for dashed lines in the second plot for SSP245 scenario
data_line_obs = data_his_all[data_his_all$year == 2010, ][, 3]
data_line_245 = data_245_all[data_245_all$year == 2030, ][, 3]
line_data_245 <- data.frame(
  x = c(2010, 2030),
  y = c(data_line_obs, data_line_245)
)

# Create datasets for dashed lines in the second plot for SSP585 scenario
data_line_obs = data_his_all[data_his_all$year == 2010, ][, 3]
data_line_585 = data_585_all[data_585_all$year == 2030, ][, 3]
line_data_585 <- data.frame(
  x = c(2010, 2034),
  y = c(data_line_obs, data_line_585)
)

# Ensure 'x' columns are numeric
line_data_126$x = as.numeric(line_data_126$x)
line_data_245$x = as.numeric(line_data_245$x)
line_data_585$x = as.numeric(line_data_585$x)

# Convert 'year' in com_frequency_all to numeric
com_frequency_all$year = as.numeric(com_frequency_all$year)

# Display first few rows of com_frequency_all
head(com_frequency_all)

# Rename species types for better readability
com_frequency_all <- com_frequency_all %>%
  mutate(spp_type = case_when(
    spp_type == "SSP126" ~ "SSP1-2.6",
    spp_type == "SSP245" ~ "SSP2-4.5",
    spp_type == "SSP585" ~ "SSP5-8.5",
    TRUE ~ spp_type
  ))

# Set the order of species types
com_frequency_all$spp_type <- factor(com_frequency_all$spp_type, 
                                     levels = c("SSP1-2.6",
                                                "SSP2-4.5",
                                                "SSP5-8.5"))

# Create the second plot (p_d)
p_d = ggplot(com_frequency_all, aes(x = year)) + 
  # Add shaded rectangles for different historical periods
  annotate("rect", xmin = 1900, xmax = 1920, ymin = -0.5, ymax = 10, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 1940, xmax = 1960, ymin = -0.5, ymax = 10, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 1980, xmax = 2000, ymin = -0.5, ymax = 10, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 2020, xmax = 2040, ymin = -0.5, ymax = 10, alpha = 0.20, fill = "lightgrey") + 
  annotate("rect", xmin = 2060, xmax = 2080, ymin = -0.5, ymax = 10, alpha = 0.20, fill = "lightgrey") + 
  # Add dashed lines for different SSP scenarios
  geom_line(data = line_data_126, aes(x = x, y = y), linetype = "dashed", alpha = 0.75, color = "#8DB3D2") +  
  geom_line(data = line_data_245, aes(x = x, y = y), linetype = "dashed", alpha = 0.75, color = "#3D5B92") +  
  geom_line(data = line_data_585, aes(x = x, y = y), linetype = "dashed", alpha = 0.75, color = "#C74A45") +  
  # Plot mean frequencies with color by species type
  geom_line(aes(y = frequency_mean, colour = spp_type, group = spp_type), 
            position = position_dodge(width = 12), 
            alpha = 0.75, linetype = "dashed") +
  # Add error bars for standard deviation
  geom_errorbar(aes(ymin = frequency_mean - frequency_sd, 
                    ymax = frequency_mean + frequency_sd, fill = spp_type),
                colour = "black", 
                position = position_dodge(width = 12), width = 8, alpha = 0.4, show.legend = FALSE) +
  # Add points for mean frequencies
  geom_point(aes(y = frequency_mean, fill = spp_type), colour = "black", 
             stat = 'identity', position = position_dodge(width = 12), size = 3, shape = 21, alpha = 0.9) +
  # Plot historical mean frequencies
  geom_line(data = data_his_all, aes(y = frequency_mean), colour = "gray60", 
            position = position_dodge(width = 18), 
            alpha = 0.75, linetype = "dashed") +
  # Add error bars for historical standard deviation
  geom_errorbar(data = data_his_all, aes(ymin = frequency_mean - frequency_sd, 
                                         ymax = frequency_mean + frequency_sd), colour = "black",
                position = position_dodge(width = 18), width = 3, alpha = 0.4, show.legend = FALSE) +
  # Add points for historical mean frequencies
  geom_point(data = data_his_all, aes(y = frequency_mean), fill = "gray60", colour = "black", 
             stat = 'identity', size = 3, shape = 21, alpha = 0.9, show.legend = FALSE) +
  # Define y-axis scale with mathematical expression
  scale_y_continuous(
    name = expression(atop(Frequency~of, paste(compound~extremes~(20~italic(yr)^-1)))),
    limits = c(-1, 12.6),
    breaks = c(0, 2, 4, 6, 8, 10, 12), 
    expand = c(0, 0)
  ) + 
  # Define x-axis scale with custom labels
  scale_x_continuous(
    name = "Years",
    limits = c(1895, 2100),
    breaks = c(
      1910, 1930, 1950, 1970, 1990,
      2010, 2030, 2050, 2070, 2090
    ),
    labels = c(
      "1901-1920", "1921-1940", "1941-1960", "1961-1980", "1981-2000",
      "2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"
    ),
    expand = c(0, 0)
  ) +
  # Define color and fill scales manually
  scale_colour_manual(values = c("#8DB3D2", "#3D5B92", "#C74A45")) +
  scale_fill_manual(values = c("#8DB3D2", "#3D5B92", "#C74A45")) +
  # Apply a theme
  theme_bw() +
  # Add annotations for different periods
  annotate(geom = "text", x = 1950, y = 11.5, label = "Historical records\n1901-2000", color = "black", size = 4, alpha = 0.6) +  
  annotate(geom = "text", x = 2011, y = 11.5, label = "Current observations\n2001-2020", color = "black", size = 4, alpha = 0.6) + 
  annotate(geom = "text", x = 2065, y = 11.5, label = "Future projections\n2021-2100", color = "black", size = 4, alpha = 0.6)

# Display the second plot
p_d
