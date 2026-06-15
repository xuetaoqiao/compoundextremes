# ============================================================
# Figure 1, panel b: Spatial distribution of compound heat and moisture extremes
# ============================================================
# Purpose:
# This script generates the map component for Figure 1, panel b.
# The map shows the spatial distribution of compound heat and moisture extremes
# across forest plots during 2001-2020.
#
# Workflow:
# 1. Load compound extreme dataset.
# 2. Count how many compound heat and moisture extreme events occurred at each plot.
# 3. Convert plot coordinates into an sf point layer.
# 4. Read the USFS ecological subregion shapefile as the map background.
# 5. Plot forest points over the ecological subregion background.
# 6. Colour each forest plot by the number of compound heat and moisture extremes.
# 7. Export the map as a PDF file.
#
# Important note:
# The map-background-related ggplot layers are intentionally kept consistent with
# the original script to preserve the ecological subregion base map.
# ============================================================



# Ecological subregion boundary shapefile.
# Source: Cleland et al. (2007), downloaded from Data Basin:
# https://databasin.org/datasets/662c543156c14313b87d9b99b7a78221/
#
# Reference:
# Cleland, D. T. et al. (2007). Ecological Subregions: Sections and Subsections
# for the conterminous United States. U.S. Department of Agriculture, Forest Service.



# ------------------------------------------------------------
# 0. Clear workspace
# ------------------------------------------------------------
# Remove all existing objects from the current R environment.
# This helps avoid conflicts with objects created in previous sessions.

rm(list = ls())


# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------
# dplyr: data manipulation, grouping, and summarising.
# nlme: mixed-effects models; loaded here for consistency with the full analysis workflow.
# ggplot2: main plotting system.
# ggpubr: publication-ready ggplot utilities.
# cowplot: plot arrangement and theme utilities.
# ggnewscale: allows multiple colour/fill scales in ggplot when needed.
# sf: handling spatial vector data.
# USAboundaries: US boundary data; loaded for possible map-related operations.
# grid: controls low-level graphical units, such as legend size and plot margins.

library(dplyr)
library(nlme)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggnewscale)
library(sf)
library(USAboundaries)
library(grid)


# ------------------------------------------------------------
# 2. Define input and output paths
# ------------------------------------------------------------
# Replace these placeholder paths with the corresponding local paths before running.
# Keeping all paths in one section makes the script easier to reuse and upload.

compound_extreme_data_path <- "/path/to/data_com_consecutive_events.csv"

ecological_subregion_shapefile_path <- "/path/to/province2007.shp"

output_directory <- "/path/to/output_directory"

output_file_name <- "figure1_panel_b_compound_heat_moisture_extreme_frequency_map.pdf"



# ------------------------------------------------------------
# 3. Load the compound heat and moisture extreme dataset
# ------------------------------------------------------------
# Load the plot-level dataset used to map the spatial distribution
# of compound heat and moisture extremes across forest plots.
# Each row corresponds to one forest plot with its associated
# compound extreme frequency and geographic coordinates.

compound_extreme_data <- read.csv(
  compound_extreme_data_path,
  header = TRUE
)


# ------------------------------------------------------------
# 4. Check the number of unique forest plots
# ------------------------------------------------------------
# Count the number of unique forest plots included in the dataset.
# This provides a basic sample-size check before mapping.

length(unique(compound_extreme_data$No_plot))


# ------------------------------------------------------------
# 5. Prepare compound extreme frequency for each forest plot
# ------------------------------------------------------------
# Rename the compound extreme frequency column to a more descriptive name.
# Here, compound_extreme_count indicates the number of compound heat and
# moisture extreme events recorded for each forest plot during 2001–2020.

compound_extreme_frequency_by_plot <- compound_extreme_data %>%
  rename(compound_extreme_count = com_numb)


# ------------------------------------------------------------
# 6. Summarise compound extreme frequency
# ------------------------------------------------------------
# Check the range of compound extreme counts per plot.
# In the current dataset, the expected range is 1 to 6.

range(compound_extreme_frequency_by_plot$compound_extreme_count)


# Calculate the total number of compound extreme records across all forest plots.

sum(compound_extreme_frequency_by_plot$compound_extreme_count)


# Calculate the average number of compound heat and moisture extremes per plot.
# This describes how frequently, on average, each forest plot experienced
# compound heat and moisture extremes during 2001-2020.

sum(compound_extreme_frequency_by_plot$compound_extreme_count) /
  length(unique(compound_extreme_frequency_by_plot$No_plot))


# Count the final number of unique forest plots used in the map.

length(unique(compound_extreme_frequency_by_plot$No_plot))


# ============================================================
# 7. Map preparation
# ============================================================

# ------------------------------------------------------------
# 7.1 Read ecological subregion shapefile
# ------------------------------------------------------------
# Read the USFS ecological subregion shapefile.
# This polygon layer provides the ecological subregion background for the map.

ecological_subregions <- st_read(
  ecological_subregion_shapefile_path
)


# ------------------------------------------------------------
# 7.2 Clean the PROVINCE field
# ------------------------------------------------------------
# The PROVINCE field may contain both letters and numbers.
# This step removes non-numeric characters and keeps only the numeric part.
# The result is first converted to numeric format and then to character format.

# Keep only the numeric part of the ecological subregion code.
ecological_subregions$PROVINCE <- as.numeric(
  gsub("\\D", "", ecological_subregions$PROVINCE)
)

# Convert PROVINCE to character format so it can be treated as a categorical code.
ecological_subregions$PROVINCE <- as.character(ecological_subregions$PROVINCE)


# ------------------------------------------------------------
# 7.3 Inspect input data and coordinate reference system
# ------------------------------------------------------------
# Print the first few rows of the plot-level frequency dataset.
# This helps check whether plot_ID, LAT, LON, and compound_extreme_count
# are correctly stored.

print(head(compound_extreme_frequency_by_plot))


# Print the coordinate reference system of the ecological subregion layer.
# The forest plot coordinates will be assigned to the same CRS.

print(st_crs(ecological_subregions))


# ------------------------------------------------------------
# 7.4 Convert forest plot coordinates to an sf object
# ------------------------------------------------------------
# Convert the plot-level data frame into an sf point object.
# LON and LAT are used as spatial coordinates.
#
# Note:
# - coords = c("LON", "LAT") means longitude is the x-coordinate and latitude
#   is the y-coordinate.
# - crs = st_crs(ecological_subregions) assigns the same CRS as the ecological
#   subregion shapefile, ensuring spatial compatibility.

compound_extreme_frequency_sf <- st_as_sf(
  compound_extreme_frequency_by_plot,
  coords = c("LON", "LAT"),
  crs = st_crs(ecological_subregions)
)


# Check the first few rows of the polygon and point sf objects.
# This is useful for confirming that the spatial data were read and created correctly.

head(ecological_subregions)
head(compound_extreme_frequency_sf)


# ============================================================
# 8. Draw the map
# ============================================================

# ------------------------------------------------------------
# 8.1 Build the spatial map
# ------------------------------------------------------------
# The map contains:
# - A light grey ecological subregion background.
# - A special layer for polygons with missing PROVINCE values.
# - Forest plot points coloured by the number of compound heat and moisture extremes.
#
# The background-related map layers below are kept in the same structure as the
# original code to avoid changing the appearance of the ecological subregion base map.

figure1_panel_b_map <- ggplot(data = ecological_subregions) +
  
  # Draw ecological subregions as a light grey spatial background.
  geom_sf(
    fill = "lightgrey",
    alpha = 0.1
  ) +
  
  # Draw polygons with missing PROVINCE values using a light blue fill.
  # This helps keep unmatched or undefined regions visible.
  geom_sf(
    data = ecological_subregions[is.na(ecological_subregions$PROVINCE), ],
    fill = "#0fc0fc",
    alpha = 0.1
  ) +
  
  # Add forest plot locations.
  # Each point is coloured according to the number of compound heat and moisture
  # extremes recorded at that forest plot during 2001-2020.
  geom_sf(
    data = compound_extreme_frequency_sf,
    aes(
      color = compound_extreme_count,
      fill  = compound_extreme_count
    ),
    shape = 20,
    size  = 0.5,
    alpha = 0.8
  ) +
  
  # Remove the main title.
  # A title can be added later during final multi-panel figure assembly if needed.
  labs(
    title = NULL
  ) +
  
  # Use a clean minimal theme as the base map style.
  theme_minimal() +
  
  # Project the map using Albers Equal Area projection.
  # This projection is commonly used for maps of the contiguous United States
  # because it preserves area relatively well.
  coord_sf(
    crs = st_crs(
      "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"
    )
  ) +
  
  # Colour scale for compound heat and moisture extreme frequency.
  # The viridis palette is perceptually uniform and colourblind-friendly.
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of\ncompound extremes\n(2001-2020)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5
    ),
    breaks = c(1, 2, 3, 4, 5, 6)
  ) +
  
  # Fill scale for compound heat and moisture extreme frequency.
  # This is retained from the original code.
  # For shape = 20, the fill aesthetic may have little or no visual effect,
  # but retaining this scale helps preserve the original plotting structure.
  scale_fill_viridis_c(
    option = "viridis",
    name = "Number of\ncompound extremes\n(2001-2020)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5
    ),
    breaks = c(1, 2, 3, 4, 5, 6)
  ) +
  
  # ----------------------------------------------------------
# 8.2 Customise map appearance
# ----------------------------------------------------------
theme(
  # Set plot margins: top, right, bottom, left.
  plot.margin = unit(c(0.3, 0.35, 0.3, 0.35), "cm"),
  
  # Remove map grid lines for a cleaner publication-style figure.
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  # Remove axis text and tick marks because geographic coordinates
  # are not the focus of this panel.
  axis.text  = element_blank(),
  axis.ticks = element_blank(),
  
  # Place the legend inside the plotting area.
  # Values are relative coordinates from 0 to 1:
  # c(0.2, 0.16) means 20% from the left and 16% from the bottom.
  legend.position = c(0.2, 0.16),
  
  # Format legend title and text.
  legend.title = element_text(
    hjust = 0.5,
    size = 13
  ),
  legend.text = element_text(
    size = 13
  ),
  
  # Arrange the colourbar horizontally.
  legend.direction = "horizontal",
  
  # Control legend colourbar size.
  legend.key.height = unit(0.3, "cm"),
  legend.key.width  = unit(1, "cm")
)


# Display the map in the current R plotting device.
figure1_panel_b_map


# ============================================================
# 9. Export the map
# ============================================================
# Save the map as a PDF file.
# The output size is 7.5 inches wide and 5 inches high.

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

ggsave(
  filename = file.path(output_directory, output_file_name),
  plot     = figure1_panel_b_map,
  width    = 7.5,
  height   = 5,
  units    = "in"
)

