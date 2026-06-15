#############################################
# Figure 3: Inset maps for forest resistance and recovery under compound versus individual climate extremes
#
# Figure title:
#   Distributions of forest resistance (a,b) and recovery (c,d) under
#   compound versus individual climate extremes.
#
# Purpose:
#   This script produces the inset map portion of Figure 3.
#   For each ecoregion containing eligible forest plots, the map shows the
#   extreme type associated with the highest mean forest resistance or recovery.
#
# Panels:
#   Panel a: Forest resistance under compound heat and moisture excess compared
#            with heat alone and moisture excess alone.
#   Panel b: Forest resistance under compound heat and drought compared with
#            heat alone and drought alone.
#   Panel c: Forest recovery under compound heat and moisture excess compared
#            with heat alone and moisture excess alone.
#   Panel d: Forest recovery under compound heat and drought compared with
#            heat alone and drought alone.
#
# Input terminology:
#   Rs_com / Rs_sin: forest resistance under compound / individual extremes.
#   Le_com / Le_sin: forest recovery under compound / individual extremes.
#
# Extreme-type codes used in the input data:
#   pup_tup   = compound heat and moisture excess
#   pdown_tup = compound heat and drought
#   pup       = moisture excess alone
#   pdown     = drought alone
#   tup       = heat alone
#
# Eligibility rule:
#   Each panel is restricted to forest plots that experienced the corresponding
#   compound extreme and both corresponding individual extremes.
#   This restriction is implemented using inner joins among the three
#   plot-level datasets.
#
#
#
# Ecological subregion boundary shapefile.
# Source: Cleland et al. (2007), downloaded from Data Basin:
# https://databasin.org/datasets/662c543156c14313b87d9b99b7a78221/
#
# Reference:
# Cleland, D. T. et al. (2007). Ecological Subregions: Sections and Subsections
# for the conterminous United States. U.S. Department of Agriculture, Forest Service.
#############################################


#==================================================
# 0. Clean workspace and load packages
#==================================================

# Remove all objects from the current R environment.
# This avoids accidentally using objects created in previous R sessions.
rm(list = ls())

# Data manipulation.
library(dplyr)

# Core plotting package.
library(ggplot2)

# Spatial vector data handling.
library(sf)


#==================================================
# 1. Define input and output paths
#==================================================

# Replace these example paths with the corresponding paths on your own computer
# before running the script.

compound_extreme_data_path   <- "/path/to/data_com.csv"
individual_extreme_data_path <- "/path/to/data_sin.csv"
ecoregion_shapefile_path     <- "/path/to/province2007.shp"
output_directory             <- "/path/to/figure_outputs"

# Create the output directory if it does not already exist.
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)


#==================================================
# 2. Import tabular and spatial data
#==================================================

# Read compound-extreme data.
# Each row should represent one plot-event record under a compound climate extreme.
compound_extreme_data <- read.csv(compound_extreme_data_path, header = TRUE)

# Read individual-extreme data.
# Each row should represent one plot-event record under an individual climate extreme.
individual_extreme_data <- read.csv(individual_extreme_data_path, header = TRUE)

# Read the USDA Forest Service ecoregion shapefile.
# The shapefile contains polygon geometries for ecoregions.
ecoregion_map <- st_read(ecoregion_shapefile_path, quiet = TRUE)

# Standardize the ecoregion code used for joining map polygons with the response data.
# The original PROVINCE field may contain non-numeric characters, so all non-digits
# are removed first. The result is stored as a character variable to match ECOSUBCD.
ecoregion_map <- ecoregion_map %>%
  mutate(
    ecoregion_code = as.character(as.numeric(gsub("\\D", "", PROVINCE)))
  )


#==================================================
# 3. Define map colors and projection
#==================================================

# Fill colors used in the inset maps.
# Yellow: compound extreme has the highest mean value.
# Green : moisture excess alone or drought alone has the highest mean value.
# Blue  : heat alone has the highest mean value.
# White : no eligible forest plots, tie, or no assigned category.
fill_color_compound      <- "#F5C266"
fill_color_water_extreme <- "#66B966"
fill_color_heat_alone    <- "#7DA7CA"
fill_color_no_data       <- "white"

# Slightly darker outline colors for mapped ecoregions.
outline_color_compound      <- "#efa112"
outline_color_water_extreme <- "#219a21"
outline_color_heat_alone    <- "#4682b4"

# Albers Equal Area projection for the conterminous United States.
us_albers_crs <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")


#==================================================
# 4. Define helper functions
#==================================================

#--------------------------------------------------
# 4.1 Summarize one extreme type to the plot level
#--------------------------------------------------

summarize_extreme_by_plot <- function(data, extreme_code, response_type) {
  # Arguments:
  #   data:
  #     Input data frame containing one or more climate-extreme types.
  #   extreme_code:
  #     Value in the ext_type column identifying the target extreme type.
  #   response_type:
  #     Either "compound" or "individual". This determines which input columns
  #     are used for forest resistance and recovery.
  #
  # Returns:
  #   A plot-level data frame with mean forest resistance and recovery for the
  #   selected extreme type.

  if (!response_type %in% c("compound", "individual")) {
    stop("response_type must be either 'compound' or 'individual'.")
  }

  if (response_type == "compound") {
    resistance_column  <- "Rs_com"
    recovery_column    <- "Le_com"
    disturbance_column <- "PGR_dis_com"
    recovery_gr_column <- "PGR_leg_com"
  } else {
    resistance_column  <- "Rs_sin"
    recovery_column    <- "Le_sin"
    disturbance_column <- "PGR_dis_sin"
    recovery_gr_column <- "PGR_leg_sin"
  }

  data %>%
    filter(ext_type == extreme_code) %>%
    mutate(ecoregion_code = as.character(ECOSUBCD)) %>%
    group_by(plot_ID, ecoregion_code) %>%
    summarise(
      forest_resistance             = mean(.data[[resistance_column]]),
      forest_recovery               = mean(.data[[recovery_column]]),
      productivity_growth_disturbance = mean(.data[[disturbance_column]]),
      productivity_growth_recovery    = mean(.data[[recovery_gr_column]]),
      .groups = "drop"
    )
}

#--------------------------------------------------
# 4.2 Build the plot-level comparison dataset for one panel
#--------------------------------------------------

build_panel_comparison_data <- function(compound_plot_data,
                                        individual_plot_data_1,
                                        individual_plot_data_2,
                                        metric) {
  # Arguments:
  #   compound_plot_data:
  #     Plot-level data for the compound extreme.
  #   individual_plot_data_1:
  #     Plot-level data for the first individual extreme, either moisture excess
  #     alone or drought alone depending on the panel.
  #   individual_plot_data_2:
  #     Plot-level data for heat alone.
  #   metric:
  #     Either "forest_resistance" or "forest_recovery".
  #
  # Returns:
  #   A plot-level comparison data frame restricted to eligible forest plots.

  if (!metric %in% c("forest_resistance", "forest_recovery")) {
    stop("metric must be either 'forest_resistance' or 'forest_recovery'.")
  }

  compound_metric <- compound_plot_data %>%
    dplyr::select(plot_ID, ecoregion_code, compound_metric = all_of(metric))

  individual_metric_1 <- individual_plot_data_1 %>%
    dplyr::select(plot_ID, ecoregion_code, individual_metric_1 = all_of(metric))

  individual_metric_2 <- individual_plot_data_2 %>%
    dplyr::select(plot_ID, ecoregion_code, individual_metric_2 = all_of(metric))

  compound_metric %>%
    inner_join(individual_metric_1, by = c("plot_ID", "ecoregion_code")) %>%
    inner_join(individual_metric_2, by = c("plot_ID", "ecoregion_code")) %>%
    mutate(
      log2_difference_individual_1_vs_compound = log2(individual_metric_1) - log2(compound_metric),
      log2_difference_individual_2_vs_compound = log2(individual_metric_2) - log2(compound_metric),
      compound_vs_individual_1_category = case_when(
        individual_metric_1 > compound_metric ~ "compound_lower",
        individual_metric_1 < compound_metric ~ "compound_higher",
        TRUE                                  ~ "equal"
      ),
      compound_vs_individual_2_category = case_when(
        individual_metric_2 > compound_metric ~ "compound_lower",
        individual_metric_2 < compound_metric ~ "compound_higher",
        TRUE                                  ~ "equal"
      )
    )
}


#--------------------------------------------------
# 4.3 Identify the highest-mean extreme type for each ecoregion
#--------------------------------------------------

summarize_highest_mean_by_ecoregion <- function(panel_comparison_data,
                                                individual_type_1,
                                                individual_type_2 = "heat_alone") {
  # Arguments:
  #   panel_comparison_data:
  #     Output from build_panel_comparison_data().
  #   individual_type_1:
  #     Label for the first individual extreme. Use "moisture_excess_alone"
  #     or "drought_alone".
  #   individual_type_2:
  #     Label for the second individual extreme. In this figure, this is always
  #     "heat_alone".
  #
  # Returns:
  #   An ecoregion-level data frame indicating which extreme type has the
  #   highest mean log2-transformed forest resistance or recovery.

  panel_comparison_data %>%
    group_by(ecoregion_code) %>%
    summarise(
      mean_compound_metric     = mean(log2(compound_metric)),
      mean_individual_metric_1 = mean(log2(individual_metric_1)),
      mean_individual_metric_2 = mean(log2(individual_metric_2)),
      number_of_eligible_plots = n(),
      .groups = "drop"
    ) %>%
    mutate(
      highest_mean_extreme_type = case_when(
        mean_compound_metric     > mean_individual_metric_1 &
          mean_compound_metric   > mean_individual_metric_2 ~ "compound_extreme",
        mean_individual_metric_1 > mean_compound_metric &
          mean_individual_metric_1 > mean_individual_metric_2 ~ individual_type_1,
        mean_individual_metric_2 > mean_compound_metric &
          mean_individual_metric_2 > mean_individual_metric_1 ~ individual_type_2,
        TRUE ~ "tie"
      )
    )
}


#--------------------------------------------------
# 4.4 Draw one inset map
#--------------------------------------------------

draw_inset_map <- function(ecoregion_map,
                           ecoregion_summary,
                           individual_type_1) {
  # Arguments:
  #   ecoregion_map:
  #     sf object containing ecoregion polygons and ecoregion_code.
  #   ecoregion_summary:
  #     Ecoregion-level data frame from summarize_highest_mean_by_ecoregion().
  #   individual_type_1:
  #     Either "moisture_excess_alone" or "drought_alone".
  #     Both are drawn in green because they represent the non-heat individual
  #     extreme in the corresponding panel.
  #
  # Returns:
  #   A ggplot object for one inset map.

  map_data <- ecoregion_map %>%
    left_join(
      ecoregion_summary %>%
        dplyr::select(ecoregion_code, highest_mean_extreme_type, number_of_eligible_plots),
      by = "ecoregion_code"
    ) %>%
    mutate(
      map_fill_color = case_when(
        highest_mean_extreme_type == "compound_extreme" ~ fill_color_compound,
        highest_mean_extreme_type == individual_type_1   ~ fill_color_water_extreme,
        highest_mean_extreme_type == "heat_alone"        ~ fill_color_heat_alone,
        TRUE                                             ~ fill_color_no_data
      ),
      map_outline_color = case_when(
        highest_mean_extreme_type == "compound_extreme" ~ outline_color_compound,
        highest_mean_extreme_type == individual_type_1   ~ outline_color_water_extreme,
        highest_mean_extreme_type == "heat_alone"        ~ outline_color_heat_alone,
        TRUE                                             ~ NA_character_
      )
    )

  mapped_ecoregions <- map_data %>%
    filter(!is.na(ecoregion_code), map_fill_color != fill_color_no_data)

  unmapped_ecoregions <- map_data %>%
    filter(!is.na(ecoregion_code), map_fill_color == fill_color_no_data)

  missing_code_ecoregions <- map_data %>%
    filter(is.na(ecoregion_code))

  ggplot(data = map_data) +
    # Draw ecoregions with eligible forest plots and a uniquely identified
    # highest-mean extreme type.
    geom_sf(
      data = mapped_ecoregions,
      aes(fill = map_fill_color, color = map_outline_color),
      size = 0.01
    ) +

    # Draw ecoregions without eligible forest plots, ties, or no assigned category.
    geom_sf(
      data = unmapped_ecoregions,
      aes(fill = map_fill_color),
      color = NA
    ) +

    # Draw any polygons with missing ecoregion codes.
    geom_sf(
      data = missing_code_ecoregions,
      fill = "white",
      color = "black",
      alpha = 0.1,
      size = 0.2
    ) +

    # Draw the outer boundary of all ecoregions.
    geom_sf(
      data = ecoregion_map %>% st_union(),
      fill = NA,
      color = "black",
      size = 0.2
    ) +

    # Use the color values already stored in the data frame.
    scale_fill_identity(guide = "none") +
    scale_color_identity(guide = "none") +

    # Use a minimal map theme and the Albers Equal Area projection.
    theme_minimal() +
    coord_sf(crs = us_albers_crs) +

    # Remove axis and grid elements because the map is used as an inset.
    theme(
      plot.margin       = unit(c(0, 0, 0, 0), "cm"),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      legend.position   = "none",
      panel.background  = element_rect(fill = "transparent", color = NA),
      plot.background   = element_rect(fill = "transparent", color = NA),
      panel.border      = element_blank()
    )
}


#==================================================
# 5. Create plot-level datasets for each extreme type
#==================================================

# Compound heat and moisture excess.
compound_heat_moisture_by_plot <- summarize_extreme_by_plot(
  data          = compound_extreme_data,
  extreme_code  = "pup_tup",
  response_type = "compound"
)

# Compound heat and drought.
compound_heat_drought_by_plot <- summarize_extreme_by_plot(
  data          = compound_extreme_data,
  extreme_code  = "pdown_tup",
  response_type = "compound"
)

# Moisture excess alone.
moisture_excess_alone_by_plot <- summarize_extreme_by_plot(
  data          = individual_extreme_data,
  extreme_code  = "pup",
  response_type = "individual"
)

# Drought alone.
drought_alone_by_plot <- summarize_extreme_by_plot(
  data          = individual_extreme_data,
  extreme_code  = "pdown",
  response_type = "individual"
)

# Heat alone.
heat_alone_by_plot <- summarize_extreme_by_plot(
  data          = individual_extreme_data,
  extreme_code  = "tup",
  response_type = "individual"
)


#==================================================
# 6. Panel a: forest resistance under compound heat and moisture excess
#==================================================

panel_a_resistance_plot_comparison <- build_panel_comparison_data(
  compound_plot_data     = compound_heat_moisture_by_plot,
  individual_plot_data_1 = moisture_excess_alone_by_plot,
  individual_plot_data_2 = heat_alone_by_plot,
  metric                 = "forest_resistance"
)

panel_a_resistance_ecoregion_summary <- summarize_highest_mean_by_ecoregion(
  panel_comparison_data = panel_a_resistance_plot_comparison,
  individual_type_1     = "moisture_excess_alone",
  individual_type_2     = "heat_alone"
)

panel_a_map <- draw_inset_map(
  ecoregion_map     = ecoregion_map,
  ecoregion_summary = panel_a_resistance_ecoregion_summary,
  individual_type_1 = "moisture_excess_alone"
)


#==================================================
# 7. Panel b: forest resistance under compound heat and drought
#==================================================

panel_b_resistance_plot_comparison <- build_panel_comparison_data(
  compound_plot_data     = compound_heat_drought_by_plot,
  individual_plot_data_1 = drought_alone_by_plot,
  individual_plot_data_2 = heat_alone_by_plot,
  metric                 = "forest_resistance"
)

panel_b_resistance_ecoregion_summary <- summarize_highest_mean_by_ecoregion(
  panel_comparison_data = panel_b_resistance_plot_comparison,
  individual_type_1     = "drought_alone",
  individual_type_2     = "heat_alone"
)

panel_b_map <- draw_inset_map(
  ecoregion_map     = ecoregion_map,
  ecoregion_summary = panel_b_resistance_ecoregion_summary,
  individual_type_1 = "drought_alone"
)


#==================================================
# 8. Panel c: forest recovery under compound heat and moisture excess
#==================================================

panel_c_recovery_plot_comparison <- build_panel_comparison_data(
  compound_plot_data     = compound_heat_moisture_by_plot,
  individual_plot_data_1 = moisture_excess_alone_by_plot,
  individual_plot_data_2 = heat_alone_by_plot,
  metric                 = "forest_recovery"
)

panel_c_recovery_ecoregion_summary <- summarize_highest_mean_by_ecoregion(
  panel_comparison_data = panel_c_recovery_plot_comparison,
  individual_type_1     = "moisture_excess_alone",
  individual_type_2     = "heat_alone"
)

panel_c_map <- draw_inset_map(
  ecoregion_map     = ecoregion_map,
  ecoregion_summary = panel_c_recovery_ecoregion_summary,
  individual_type_1 = "moisture_excess_alone"
)


#==================================================
# 9. Panel d: forest recovery under compound heat and drought
#==================================================

panel_d_recovery_plot_comparison <- build_panel_comparison_data(
  compound_plot_data     = compound_heat_drought_by_plot,
  individual_plot_data_1 = drought_alone_by_plot,
  individual_plot_data_2 = heat_alone_by_plot,
  metric                 = "forest_recovery"
)

panel_d_recovery_ecoregion_summary <- summarize_highest_mean_by_ecoregion(
  panel_comparison_data = panel_d_recovery_plot_comparison,
  individual_type_1     = "drought_alone",
  individual_type_2     = "heat_alone"
)

panel_d_map <- draw_inset_map(
  ecoregion_map     = ecoregion_map,
  ecoregion_summary = panel_d_recovery_ecoregion_summary,
  individual_type_1 = "drought_alone"
)


#==================================================
# 10. Export inset maps
#==================================================

# Save each inset map as an individual PDF file.
# File names are written in English to make the script suitable for code upload.
ggsave(
  filename = file.path(output_directory, "figure3_inset_map_panel_a_resistance_heat_moisture.pdf"),
  plot     = panel_a_map,
  width    = 6,
  height   = 4,
  units    = "in"
)

ggsave(
  filename = file.path(output_directory, "figure3_inset_map_panel_b_resistance_heat_drought.pdf"),
  plot     = panel_b_map,
  width    = 6,
  height   = 4,
  units    = "in"
)

ggsave(
  filename = file.path(output_directory, "figure3_inset_map_panel_c_recovery_heat_moisture.pdf"),
  plot     = panel_c_map,
  width    = 6,
  height   = 4,
  units    = "in"
)

ggsave(
  filename = file.path(output_directory, "figure3_inset_map_panel_d_recovery_heat_drought.pdf"),
  plot     = panel_d_map,
  width    = 6,
  height   = 4,
  units    = "in"
)


#==================================================
# 11. Optional preview in RStudio
#==================================================

# Uncomment the following lines to preview the maps in the RStudio Plots pane.
# panel_a_map
# panel_b_map
# panel_c_map
# panel_d_map
