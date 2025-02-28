library(rflsgen)
library(raster)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)

# Creating class targets with more diverse specifications
forest <- flsgen_create_class_targets("forest", 
                                      NP = c(25, 25), 
                                      AREA = c(800, 3500),
                                      PLAND = c(15, 15))

shrubland <- flsgen_create_class_targets("shrubland", 
                                         NP = c(35, 35), 
                                         AREA = c(600, 2500),
                                         PLAND = c(18, 18))

savanna <- flsgen_create_class_targets("savanna", 
                                       NP = c(20, 20), 
                                       AREA = c(400, 2000),
                                       PLAND = c(8, 8))

grassland <- flsgen_create_class_targets("grassland", 
                                         NP = c(45, 45), 
                                         AREA = c(300, 1200),
                                         PLAND = c(12, 12))

wetland <- flsgen_create_class_targets("wetland", 
                                       NP = c(15, 15), 
                                       AREA = c(500, 1800),
                                       PLAND = c(7, 7))

# Creating the landscape targets
ls_targets <- flsgen_create_landscape_targets(600, 600, 
                                              list(forest, shrubland, savanna, grassland, wetland))

# Generate the structure
structure <- flsgen_structure(ls_targets, search_strategy = "DOM_OVER_W_DEG_REF")

# Generate the landscape
landscape <- flsgen_generate(structure, 
                             roughness = 0.4, 
                             terrain_dependency = 0.7,
                             min_distance = 5, 
                             verbose = FALSE)

# Define class names for the legend
class_names <- c("Bare Areas", "Forest", "Shrubland", "Savanna", "Grassland", "Wetland")

# Convert raster to dataframe for ggplot
landscape_df <- as.data.frame(landscape, xy = TRUE)
colnames(landscape_df) <- c("x", "y", "class")

# Convert class to factor for better control in ggplot
landscape_df$class_factor <- factor(landscape_df$class, 
                                    levels = c(-1, 0, 1, 2, 3, 4),
                                    labels = class_names)

# Define a custom color palette that looks professional
custom_colors <- c(
  "Bare Areas" = "#E8DCC9",   # Light tan
  "Forest" = "#1A5E1A",       # Deep forest green
  "Shrubland" = "#8E6C3E",    # Rich brown
  "Savanna" = "#E6B943",      # Golden yellow
  "Grassland" = "#8BCF5A",    # Bright green
  "Wetland" = "#4A8DB5"       # Medium blue
)

# Create enhanced ggplot visualization
ggplot_landscape <- ggplot(landscape_df, aes(x = x, y = y, fill = class_factor)) +
  geom_raster() +
  scale_fill_manual(values = custom_colors, name = "Land Cover Classes") +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Simulated Landscape Composition",
    subtitle = "Generated with rflsgen package",
    caption = "Note: Spatial units are in meters"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 11),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  xlab("Easting (m)") +
  ylab("Northing (m)")

ggsave("landscape.png", height = 8, width = 8, dpi = 350)



# Calculate and display land cover percentage (optional)
coverage_stats <- landscape_df %>%
  group_by(class_factor) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the statistics
print(coverage_stats)

# Save high-quality plot
ggsave("enhanced_landscape_visualization.png", width = 10, height = 8, dpi = 300)

################################
library(rflsgen)
library(raster)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(tidyverse)

# First create our basic landscape targets with five land cover classes
forest <- flsgen_create_class_targets("forest", 
                                      NP = c(25, 25), 
                                      AREA = c(800, 3500),
                                      PLAND = c(15, 15))

shrubland <- flsgen_create_class_targets("shrubland", 
                                         NP = c(35, 35), 
                                         AREA = c(600, 2500),
                                         PLAND = c(18, 18))

savanna <- flsgen_create_class_targets("savanna", 
                                       NP = c(20, 20), 
                                       AREA = c(400, 2000),
                                       PLAND = c(8, 8))

grassland <- flsgen_create_class_targets("grassland", 
                                         NP = c(45, 45), 
                                         AREA = c(300, 1200),
                                         PLAND = c(12, 12))

wetland <- flsgen_create_class_targets("wetland", 
                                       NP = c(15, 15), 
                                       AREA = c(500, 1800),
                                       PLAND = c(7, 7))

# Create landscape targets - using 500x500 to reduce computation time
ls_targets <- flsgen_create_landscape_targets(500, 500, 
                                              list(forest, shrubland, savanna, grassland, wetland))

# Generate a single structure that will be used for all landscapes
set.seed(123) # For reproducibility
structure <- flsgen_structure(ls_targets, search_strategy = "DOM_OVER_W_DEG_REF")

# Parameters to vary
# 1. Different terrain_dependency values
td_values <- c(0.1, 0.5, 0.9)
# 2. Different roughness values
roughness_values <- c(0.2, 0.5, 0.8)

# Define class names for the legend
class_names <- c("Bare Areas", "Forest", "Shrubland", "Savanna", "Grassland", "Wetland")

# Define a custom color palette
custom_colors <- c(
  "Bare Areas" = "#E8DCC9",   # Light tan
  "Forest" = "#1A5E1A",       # Deep forest green
  "Shrubland" = "#8E6C3E",    # Rich brown
  "Savanna" = "#E6B943",      # Golden yellow
  "Grassland" = "#8BCF5A",    # Bright green
  "Wetland" = "#4A8DB5"       # Medium blue
)

# Function to generate a landscape and convert to ggplot-ready dataframe
create_landscape_df <- function(structure, roughness, terrain_dependency, min_distance = 5) {
  landscape <- flsgen_generate(structure, 
                               roughness = roughness, 
                               terrain_dependency = terrain_dependency,
                               min_distance = min_distance, 
                               verbose = FALSE)
  
  landscape_df <- as.data.frame(landscape, xy = TRUE)
  colnames(landscape_df) <- c("x", "y", "class")
  
  landscape_df$class_factor <- factor(landscape_df$class, 
                                      levels = c(-1, 0, 1, 2, 3, 4),
                                      labels = class_names)
  return(landscape_df)
}

# Generate landscapes with varying terrain_dependency
td_landscapes <- lapply(td_values, function(td) {
  create_landscape_df(structure, roughness = 0.5, terrain_dependency = td)
})

# Generate landscapes with varying roughness
roughness_landscapes <- lapply(roughness_values, function(r) {
  create_landscape_df(structure, roughness = r, terrain_dependency = 0.5)
})

# Function to create a ggplot for each landscape
create_landscape_plot <- function(df, title, legend = FALSE) {
  p <- ggplot(df, aes(x = x, y = y, fill = class_factor)) +
    geom_raster() +
    scale_fill_manual(values = custom_colors, name = "Land Cover Classes") +
    coord_equal() +
    theme_minimal() +
    labs(
      subtitle = title
    ) +
    theme(
      legend.position = if(legend) "right" else "none",
      plot.subtitle = element_text(size = 10, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
    )
  
  return(p)
}

# Create plots for terrain_dependency variations
td_plots <- list()
for(i in seq_along(td_values)) {
  td_plots[[i]] <- create_landscape_plot(
    td_landscapes[[i]], 
    paste("Terrain Dependency =", td_values[i])
  )
}

# Create plots for roughness variations
roughness_plots <- list()
for(i in seq_along(roughness_values)) {
  roughness_plots[[i]] <- create_landscape_plot(
    roughness_landscapes[[i]], 
    paste("Roughness =", roughness_values[i])
  )
}

# Create a combined plot with legend
# Extract legend from a plot with legend
legend_plot <- create_landscape_plot(td_landscapes[[1]], "Legend", legend = TRUE)
legend <- get_legend(legend_plot)

# Create grid of plots without legends
plots_grid <- plot_grid(
  td_plots[[1]], td_plots[[2]], td_plots[[3]],
  roughness_plots[[1]], roughness_plots[[2]], roughness_plots[[3]],
  nrow = 2, ncol = 3
)

# Add title to the grid
title <- ggdraw() + 
  draw_label(
    "Landscape Patterns with Varying Spatial Configurations",
    fontface = 'bold',
    size = 14,
    x = 0.5,
    hjust = 0.5
  )

# Add the legend to the grid
final_plot <- plot_grid(
  title,
  plots_grid,
  legend,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1)
)

# Display the final plot
print(final_plot)

# Save as high-quality image
ggsave("landscapes_comparison.png", final_plot, width = 12, height = 8, dpi = 300)

# Verify all landscapes have the same composition
verify_composition <- function() {
  # Create a function to calculate class percentages
  calc_percentages <- function(df) {
    df %>%
      group_by(class_factor) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
  }
  
  # Calculate for all 6 landscapes
  td_compositions <- lapply(td_landscapes, calc_percentages)
  roughness_compositions <- lapply(roughness_landscapes, calc_percentages)
  
  # Print the first one as reference
  cat("Reference landscape composition:\n")
  print(td_compositions[[1]], n = nrow(td_compositions[[1]]))
  
  # Check if all compositions are the same
  are_equal <- TRUE
  for (i in 2:length(td_compositions)) {
    if (!all(near(td_compositions[[1]]$percentage, td_compositions[[i]]$percentage, tol = 0.1))) {
      are_equal <- FALSE
      cat("Terrain Dependency", td_values[i], "composition differs from reference\n")
    }
  }
  
  for (i in 1:length(roughness_compositions)) {
    if (!all(near(td_compositions[[1]]$percentage, roughness_compositions[[i]]$percentage, tol = 0.1))) {
      are_equal <- FALSE
      cat("Roughness", roughness_values[i], "composition differs from reference\n")
    }
  }
  
  if (are_equal) {
    cat("All landscapes have the same composition within 0.1% tolerance\n")
  }
}

# Run composition verification
verify_composition()
##################################
library(rflsgen)
library(raster)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(viridis)

# First create our basic landscape targets with five land cover classes
forest <- flsgen_create_class_targets("forest", 
                                      NP = c(25, 25), 
                                      AREA = c(800, 3500),
                                      PLAND = c(15, 15))

shrubland <- flsgen_create_class_targets("shrubland", 
                                         NP = c(35, 35), 
                                         AREA = c(600, 2500),
                                         PLAND = c(18, 18))

savanna <- flsgen_create_class_targets("savanna", 
                                       NP = c(20, 20), 
                                       AREA = c(400, 2000),
                                       PLAND = c(8, 8))

grassland <- flsgen_create_class_targets("grassland", 
                                         NP = c(45, 45), 
                                         AREA = c(300, 1200),
                                         PLAND = c(12, 12))

wetland <- flsgen_create_class_targets("wetland", 
                                       NP = c(15, 15), 
                                       AREA = c(500, 1800),
                                       PLAND = c(7, 7))

# Create landscape targets - using 400x400 to reduce computation time and make visualization more compact
ls_targets <- flsgen_create_landscape_targets(400, 400, 
                                              list(forest, shrubland, savanna, grassland, wetland))

# Generate a single structure that will be used for all landscapes
set.seed(123) # For reproducibility
structure <- flsgen_structure(ls_targets, search_strategy = "DOM_OVER_W_DEG_REF")

# Parameters to vary
# 1. Different terrain_dependency values
td_values <- c(0.1, 0.5, 0.9)
# 2. Different roughness values
roughness_values <- c(0.2, 0.5, 0.8)

# Define class names for the legend
class_names <- c("Bare Areas", "Forest", "Shrubland", "Savanna", "Grassland", "Wetland")

# Define a custom color palette
custom_colors <- c(
  "Bare Areas" = "#E8DCC9",   # Light tan
  "Forest" = "#1A5E1A",       # Deep forest green
  "Shrubland" = "#8E6C3E",    # Rich brown
  "Savanna" = "#E6B943",      # Golden yellow
  "Grassland" = "#8BCF5A",    # Bright green
  "Wetland" = "#4A8DB5"       # Medium blue
)

# Create all landscapes and combine into a single dataframe
# This approach allows us to use faceting for a more integrated plot
all_landscapes <- data.frame()

# Generate terrain_dependency landscapes
for (i in seq_along(td_values)) {
  landscape <- flsgen_generate(structure, 
                               roughness = 0.5, 
                               terrain_dependency = td_values[i],
                               min_distance = 5, 
                               verbose = FALSE)
  
  landscape_df <- as.data.frame(landscape, xy = TRUE)
  colnames(landscape_df) <- c("x", "y", "class")
  landscape_df$class_factor <- factor(landscape_df$class, 
                                      levels = c(-1, 0, 1, 2, 3, 4),
                                      labels = class_names)
  
  landscape_df$parameter <- "Terrain Dependency"
  landscape_df$value <- as.character(td_values[i])
  landscape_df$row <- 1
  landscape_df$col <- i
  
  all_landscapes <- rbind(all_landscapes, landscape_df)
}

# Generate roughness landscapes
for (i in seq_along(roughness_values)) {
  landscape <- flsgen_generate(structure, 
                               roughness = roughness_values[i], 
                               terrain_dependency = 0.5,
                               min_distance = 5, 
                               verbose = FALSE)
  
  landscape_df <- as.data.frame(landscape, xy = TRUE)
  colnames(landscape_df) <- c("x", "y", "class")
  landscape_df$class_factor <- factor(landscape_df$class, 
                                      levels = c(-1, 0, 1, 2, 3, 4),
                                      labels = class_names)
  
  landscape_df$parameter <- "Roughness"
  landscape_df$value <- as.character(roughness_values[i])
  landscape_df$row <- 2
  landscape_df$col <- i
  
  all_landscapes <- rbind(all_landscapes, landscape_df)
}

# Create plot titles from parameter and value
all_landscapes$subtitle <- paste(all_landscapes$parameter, "=", all_landscapes$value)

# Create a position factor for proper ordering
all_landscapes$position <- factor(paste(all_landscapes$row, all_landscapes$col), 
                                  levels = c("1 1", "1 2", "1 3", "2 1", "2 2", "2 3"))

# Create an improved, tighter visualization using faceting
landscape_plot <- ggplot(all_landscapes, aes(x = x, y = y, fill = class_factor)) +
  geom_raster() +
  facet_wrap(~subtitle, nrow = 2, ncol = 3) +
  scale_fill_manual(values = custom_colors, name = "Land Cover Classes") +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Landscape Patterns with Varying Spatial Configurations",
    caption = "All landscapes maintain identical class composition"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "lines"),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0.5, margin = margin(t = 10)),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

# Display the final plot
print(landscape_plot)

# Save as high-quality image
ggsave("landscapes_comparison_improved.jpeg", landscape_plot, width = 10, height = 7, dpi = 300)

# Verify all landscapes have the same composition
verify_composition <- function() {
  all_landscapes %>%
    group_by(subtitle, class_factor) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = count / sum(count) * 100, .by = subtitle) %>%
    select(subtitle, class_factor, percentage) %>%
    pivot_wider(names_from = subtitle, values_from = percentage) %>%
    print(n = nrow(class_names))
}

# Run composition verification
verify_composition()

##########################
library(rflsgen)
library(raster)
library(landscapemetrics)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(viridis)

# First create our basic landscape targets with five land cover classes
forest <- flsgen_create_class_targets("forest", 
                                      NP = c(25, 25), 
                                      AREA = c(800, 3500),
                                      PLAND = c(15, 15))

shrubland <- flsgen_create_class_targets("shrubland", 
                                         NP = c(35, 35), 
                                         AREA = c(600, 2500),
                                         PLAND = c(18, 18))

savanna <- flsgen_create_class_targets("savanna", 
                                       NP = c(20, 20), 
                                       AREA = c(400, 2000),
                                       PLAND = c(8, 8))

grassland <- flsgen_create_class_targets("grassland", 
                                         NP = c(45, 45), 
                                         AREA = c(300, 1200),
                                         PLAND = c(12, 12))

wetland <- flsgen_create_class_targets("wetland", 
                                       NP = c(15, 15), 
                                       AREA = c(500, 1800),
                                       PLAND = c(7, 7))

# Create landscape targets
ls_targets <- flsgen_create_landscape_targets(400, 400, 
                                              list(forest, shrubland, savanna, grassland, wetland))

# Generate a single structure that will be used for all landscapes
set.seed(123) # For reproducibility
structure <- flsgen_structure(ls_targets, search_strategy = "DOM_OVER_W_DEG_REF")

# Parameters to vary
# 1. Different terrain_dependency values
td_values <- c(0.1, 0.5, 0.9)
# 2. Different roughness values
roughness_values <- c(0.2, 0.5, 0.8)

# Define class names and human-readable parameter names for plotting
class_names <- c("Bare Areas", "Forest", "Shrubland", "Savanna", "Grassland", "Wetland")
parameter_names <- c("Terrain Dependency", "Roughness")

# Create landscapes and store as list of rasters
landscapes <- list()

# Generate terrain_dependency landscapes
for (i in seq_along(td_values)) {
  landscape <- flsgen_generate(structure, 
                               roughness = 0.5, 
                               terrain_dependency = td_values[i],
                               min_distance = 5, 
                               verbose = FALSE)
  
  # Store the landscape with descriptive name
  landscapes[[paste("TD", td_values[i], sep="_")]] <- landscape
}

# Generate roughness landscapes
for (i in seq_along(roughness_values)) {
  landscape <- flsgen_generate(structure, 
                               roughness = roughness_values[i], 
                               terrain_dependency = 0.5,
                               min_distance = 5, 
                               verbose = FALSE)
  
  # Store the landscape with descriptive name
  landscapes[[paste("R", roughness_values[i], sep="_")]] <- landscape
}

# Function to extract parameters from landscape name
extract_params <- function(name) {
  parts <- strsplit(name, "_")[[1]]
  param_type <- substr(parts[1], 1, 1)
  param_value <- as.numeric(parts[2])
  
  if (param_type == "T") {
    return(list(type = "Terrain Dependency", value = param_value))
  } else {
    return(list(type = "Roughness", value = param_value))
  }
}

# ------------------------
# LANDSCAPE METRICS ANALYSIS
# ------------------------

# Function to calculate multiple metrics for a landscape
calculate_metrics <- function(landscape, name) {
  # Extract parameter information
  params <- extract_params(name)
  
  # Calculate each metric and rename columns to avoid duplicates
  pland_df <- lsm_c_pland(landscape) %>% 
    rename(pland_value = value)
  
  lpi_df <- lsm_c_lpi(landscape) %>% 
    rename(lpi_value = value)
  
  np_df <- lsm_c_np(landscape) %>% 
    rename(np_value = value)
  
  pd_df <- lsm_c_pd(landscape) %>% 
    rename(pd_value = value)
  
  ai_df <- lsm_c_ai(landscape) %>% 
    rename(ai_value = value)
  
  cohesion_df <- lsm_c_cohesion(landscape) %>% 
    rename(cohesion_value = value)
  
  enn_mn_df <- lsm_c_enn_mn(landscape) %>% 
    rename(enn_mn_value = value)
  
  te_df <- lsm_c_te(landscape) %>% 
    rename(te_value = value)
  
  ed_df <- lsm_c_ed(landscape) %>% 
    rename(ed_value = value)
  
  shape_mn_df <- lsm_c_shape_mn(landscape) %>% 
    rename(shape_mn_value = value)
  
  frac_mn_df <- lsm_c_frac_mn(landscape) %>% 
    rename(frac_mn_value = value)
  
  # Join all metrics
  metrics_df <- pland_df %>%
    left_join(lpi_df, by = c("layer", "level", "class", "id")) %>%
    left_join(np_df, by = c("layer", "level", "class", "id")) %>%
    left_join(pd_df, by = c("layer", "level", "class", "id")) %>%
    left_join(ai_df, by = c("layer", "level", "class", "id")) %>%
    left_join(cohesion_df, by = c("layer", "level", "class", "id")) %>%
    left_join(enn_mn_df, by = c("layer", "level", "class", "id")) %>%
    left_join(te_df, by = c("layer", "level", "class", "id")) %>%
    left_join(ed_df, by = c("layer", "level", "class", "id")) %>%
    left_join(shape_mn_df, by = c("layer", "level", "class", "id")) %>%
    left_join(frac_mn_df, by = c("layer", "level", "class", "id")) %>%
    select(-starts_with("metric")) %>%  # Remove all metric columns
    mutate(parameter = params$type,
           parameter_value = params$value,
           landscape_name = name)
  
  return(metrics_df)
}
# Calculate metrics for all landscapes
all_metrics <- map_dfr(names(landscapes), 
                       ~calculate_metrics(landscapes[[.]], .))

# Filter out the "Bare Areas" class (class -1) for focused analysis of focal classes
all_metrics_filtered <- all_metrics %>%
  filter(class >= 0) %>%
  mutate(class_name = factor(class, 
                             levels = 0:4, 
                             labels = class_names[2:6]))

# 1. Patch Size and Fragmentation Plot
patch_size_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = pland_value, fill = class_name)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Landscape Composition",
    y = "Percent of Landscape (%)",
    x = "Parameter Value",
    fill = "Land Cover Class"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# 2. Fragmentation Plot
fragmentation_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = np_value, color = class_name)) +
  geom_point(size = 3) +
  geom_line(aes(group = class_name), linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Landscape Fragmentation",
    y = "Number of Patches",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# 3. Connectivity Plot
connectivity_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = cohesion_value, color = class_name)) +
  geom_point(size = 3) +
  geom_line(aes(group = class_name), linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Landscape Connectivity",
    y = "Cohesion Index",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# 4. Edge Metrics Plot
edge_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = ed_value, color = class_name)) +
  geom_point(size = 3) +
  geom_line(aes(group = class_name), linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Edge Characteristics",
    y = "Edge Density",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# 5. Shape Complexity Plot
shape_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = shape_mn_value, color = class_name)) +
  geom_point(size = 3) +
  geom_line(aes(group = class_name), linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Shape Complexity",
    y = "Mean Shape Index",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# 6. Advanced: Radar Chart for Multi-metric Comparison
# Prepare data for radar chart by normalizing metrics
radar_data <- all_metrics_filtered %>%
  filter(class_name == "Forest") %>%  # Focus on forest class for example
  select(parameter, parameter_value, np_value, cohesion_value, ed_value, shape_mn_value, ai_value) %>%
  group_by(parameter) %>%
  mutate(
    np_norm = scales::rescale(np_value, to = c(0, 1)),
    cohesion_norm = scales::rescale(cohesion_value, to = c(0, 1)),
    ed_norm = scales::rescale(ed_value, to = c(0, 1)),
    shape_norm = scales::rescale(shape_mn_value, to = c(0, 1)),
    ai_norm = scales::rescale(ai_value, to = c(0, 1))
  ) %>%
  ungroup() %>%
  select(parameter, parameter_value, np_norm, cohesion_norm, ed_norm, shape_norm, ai_norm) %>%
  pivot_longer(
    cols = c(np_norm, cohesion_norm, ed_norm, shape_norm, ai_norm),
    names_to = "metric",
    values_to = "normalized_value"
  ) %>%
  mutate(
    metric_name = case_when(
      metric == "np_norm" ~ "Fragmentation",
      metric == "cohesion_norm" ~ "Connectivity",
      metric == "ed_norm" ~ "Edge Density",
      metric == "shape_norm" ~ "Shape Complexity",
      metric == "ai_norm" ~ "Aggregation"
    ),
    landscape_id = paste(parameter, parameter_value, sep = "_")
  )

# Create radar chart for forest class
radar_chart <- radar_data %>%
  ggplot(aes(x = metric_name, y = normalized_value, color = as.factor(parameter_value), group = landscape_id)) +
  geom_polygon(aes(fill = as.factor(parameter_value)), alpha = 0.1, linewidth = 1) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  coord_radar() +
  facet_wrap(~parameter) +
  labs(
    title = "Forest Class: Comparative Landscape Metrics",
    color = "Parameter Value",
    fill = "Parameter Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8),
    panel.grid.major = element_line(linetype = "dashed", color = "gray80"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

# Create a table of summary metrics for publication
summary_table <- all_metrics_filtered %>%
  group_by(parameter, parameter_value, class_name) %>%
  summarise(
    fragmentation = mean(np_value),
    connectivity = mean(cohesion_value),
    edge_density = mean(ed_value),
    shape_complexity = mean(shape_mn_value),
    aggregation = mean(ai_value),
    .groups = "drop"
  ) %>%
  arrange(parameter, parameter_value, class_name)

#################Nature

# Set up theme for publication-quality plots
theme_publication <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(size = rel(1.1), face = "bold", hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 10)),
      plot.caption = element_text(size = rel(0.7), color = "gray30", margin = margin(t = 15)),
      axis.title = element_text(size = rel(0.9), face = "bold"),
      axis.text = element_text(size = rel(0.8)),
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.8)),
      strip.text = element_text(size = rel(0.9), face = "bold"),
      
      # Non-text elements
      panel.grid.major = element_line(color = "gray85", size = 0.2),
      panel.grid.minor = element_line(color = "gray92", size = 0.1),
      panel.border = element_rect(fill = NA, color = "gray70", size = 0.5),
      panel.spacing = unit(1.5, "lines"),
      legend.key.size = unit(0.8, "lines"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      
      # Legend positioning
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10),
      legend.background = element_rect(fill = "white", color = NA)
    )
}

custom_colors <- c(
  "Bare Areas" = "#E8DCC9",   # Light tan
  "Forest" = "#1A5E1A",       # Deep forest green
  "Shrubland" = "#8E6C3E",    # Rich brown
  "Savanna" = "#E6B943",      # Golden yellow
  "Grassland" = "#8BCF5A",    # Bright green
  "Wetland" = "#4A8DB5"       # Medium blue
)

# Custom color palettes
nature_palette1 <- c("#1A5E1A", "#8E6C3E", "#E6B943", "#8BCF5A", "#4A8DB5")
nature_palette2 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")

# Figure labels (a, b, c, etc.) function for multi-panel figures
add_panel_labels <- function(plot, label, x = 0.05, y = 0.95, size = 14, face = "bold") {
  plot + 
    annotate(
      "text", 
      x = x, 
      y = y, 
      label = label, 
      size = size / .pt, 
      fontface = face,
      hjust = 0, 
      vjust = 1
    )
}

# Calculate metrics for all landscapes
all_metrics <- map_dfr(names(landscapes), ~calculate_metrics(landscapes[[.]], .))

# Filter out the "Bare Areas" class for focused analysis
all_metrics_filtered <- all_metrics %>%
  filter(class >= 0) %>%
  mutate(class_name = factor(class, 
                             levels = 0:4, 
                             labels = class_names[2:6]))

# 1. Patch Size and Fragmentation Plot
patch_size_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = pland_value, fill = class_name)) +
  geom_col(position = "dodge", width = 0.7, color = "white", size = 0.2) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Landscape Composition",
    y = "Percent of Landscape (%)",
    x = "Parameter Value",
    fill = "Land Cover Class"
  ) +
  theme_publication() +
  scale_fill_manual(values = nature_palette1) +
  # Add more precise y-axis
  scale_y_continuous(breaks = seq(0, 20, 5), expand = c(0, 1))

patch_size_plot <- add_panel_labels(patch_size_plot, "a")

ggsave("patch_size_plot.jpeg", patch_size_plot, height = 7, width = 8, dpi = 350)

# 2. Fragmentation Plot
fragmentation_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = np_value, color = class_name, group = class_name)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_line(linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Landscape Fragmentation",
    y = "Number of Patches",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_publication() +
  scale_color_manual(values = nature_palette1)

fragmentation_plot <- add_panel_labels(fragmentation_plot, "b")

ggsave("fragmentation_plot.jpeg", fragmentation_plot, height = 7, width = 8, dpi = 350)

# 3. Connectivity Plot
connectivity_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = cohesion_value, color = class_name, group = class_name)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_line(linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Landscape Connectivity",
    y = "Cohesion Index",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_publication() +
  scale_color_manual(values = nature_palette1) +
  # Adjust y-axis for cohesion values
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))

connectivity_plot <- add_panel_labels(connectivity_plot, "c")

ggsave("connectivity_plot.jpeg", connectivity_plot, height = 7, width = 8, dpi = 350)

# 4. Edge Metrics Plot
edge_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = ed_value, color = class_name, group = class_name)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_line(linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Edge Characteristics",
    y = "Edge Density",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_publication() +
  scale_color_manual(values = nature_palette1)

edge_plot <- add_panel_labels(edge_plot, "d")

ggsave("edge_plot.jpeg", edge_plot, height = 7, width = 8, dpi = 350)

# 5. Shape Complexity Plot
shape_plot <- all_metrics_filtered %>%
  ggplot(aes(x = as.factor(parameter_value), y = shape_mn_value, color = class_name, group = class_name)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_line(linewidth = 1) +
  facet_grid(. ~ parameter, scales = "free_x") +
  labs(
    title = "Shape Complexity",
    y = "Mean Shape Index",
    x = "Parameter Value",
    color = "Land Cover Class"
  ) +
  theme_publication() +
  scale_color_manual(values = nature_palette1) +
  # Adjust y-axis for shape values
  scale_y_continuous(labels = function(x) sprintf("%.2f", x))

shape_plot <- add_panel_labels(shape_plot, "e")
ggsave("shape_plot.jpeg", shape_plot, height = 7, width = 8, dpi = 350)

# 6. Advanced: Radar Chart for Multi-metric Comparison
# Function to create radar coordinates with improved styling
coord_radar <- function(...) {
  coord_polar(...) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = scales::percent_format(accuracy = 1)) +
    theme(
      axis.text.y = element_text(size = rel(0.7)),
      axis.title = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", color = "gray80", size = 0.2),
      panel.grid.minor = element_blank()
    )
}

# Prepare data for radar chart by normalizing metrics
radar_data <- all_metrics_filtered %>%
  filter(class_name == "Forest") %>%  # Focus on forest class
  select(parameter, parameter_value, np_value, cohesion_value, ed_value, shape_mn_value, ai_value) %>%
  group_by(parameter) %>%
  mutate(
    np_norm = scales::rescale(np_value, to = c(0, 1)),
    cohesion_norm = scales::rescale(cohesion_value, to = c(0, 1)),
    ed_norm = scales::rescale(ed_value, to = c(0, 1)),
    shape_norm = scales::rescale(shape_mn_value, to = c(0, 1)),
    ai_norm = scales::rescale(ai_value, to = c(0, 1))
  ) %>%
  ungroup() %>%
  select(parameter, parameter_value, np_norm, cohesion_norm, ed_norm, shape_norm, ai_norm) %>%
  pivot_longer(
    cols = c(np_norm, cohesion_norm, ed_norm, shape_norm, ai_norm),
    names_to = "metric",
    values_to = "normalized_value"
  ) %>%
  mutate(
    metric_name = case_when(
      metric == "np_norm" ~ "Fragmentation",
      metric == "cohesion_norm" ~ "Connectivity",
      metric == "ed_norm" ~ "Edge Density",
      metric == "shape_norm" ~ "Shape Complexity",
      metric == "ai_norm" ~ "Aggregation"
    ),
    landscape_id = paste(parameter, parameter_value, sep = "_"),
    parameter_value = as.factor(parameter_value)
  )

# Create radar chart for forest class with enhanced styling
# Create radar chart for forest class with enhanced styling
radar_chart <- radar_data %>%
  ggplot(aes(x = metric_name, y = normalized_value, color = parameter_value, group = landscape_id)) +
  
  geom_polygon(aes(fill = parameter_value), alpha = 0.15, linewidth = 0.8) +
  
  geom_line(linewidth = 1.2, alpha = 0.9) +
  
  geom_point(size = 2.5, alpha = 0.9) +
  
  # Replace coord_radar() with the correct function from the appropriate package
  coord_polar() +  # Use coord_polar() from ggplot2 OR
  # If using another package like fmsb or ggradar, load it first:
  # library(ggradar)  # OR library(fmsb)
  
  facet_wrap(~parameter, ncol = 2) +
  
  labs(
    title = "Forest Class: Comparative Landscape Metrics",
    subtitle = "Normalized metric values (higher values = stronger effect)",
    color = "Parameter Value",
    fill = "Parameter Value"
  ) +
  
  theme_publication(base_size = 11) +
  
  theme(
    axis.text.x = element_text(size = rel(0.8), face = "bold"),
    panel.border = element_rect(fill = NA, color = "gray70"),
    strip.text = element_text(size = rel(1), face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "gray70")
  ) +
  
  scale_color_manual(values = nature_palette2) +
  
  scale_fill_manual(values = nature_palette2)

radar_chart <- add_panel_labels(radar_chart, "f")

ggsave("radar_chart.jpeg", radar_chart, height = 7, width = 8, dpi = 350)

# 7. NEW: Heatmap visualization of metrics
# Let's create a heatmap for a visual comparison of metrics across parameters
heatmap_data <- all_metrics_filtered %>%
  filter(parameter == "Terrain Dependency") %>% # Just using one parameter for example
  select(parameter_value, class_name, np_value, cohesion_value, ed_value, shape_mn_value, ai_value) %>%
  pivot_longer(
    cols = c(np_value, cohesion_value, ed_value, shape_mn_value, ai_value),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric_clean = case_when(
      metric == "np_value" ~ "Number of Patches",
      metric == "cohesion_value" ~ "Cohesion Index",
      metric == "ed_value" ~ "Edge Density",
      metric == "shape_mn_value" ~ "Shape Index",
      metric == "ai_value" ~ "Aggregation Index"
    ),
    # Scale values within each metric
    metric_clean = factor(metric_clean, levels = c("Number of Patches", "Cohesion Index", 
                                                   "Edge Density", "Shape Index", "Aggregation Index"))
  ) %>%
  group_by(metric) %>%
  mutate(scaled_value = scales::rescale(value, to = c(0, 1))) %>%
  ungroup()

heatmap_plot <- heatmap_data %>%
  ggplot(aes(x = parameter_value, y = class_name, fill = scaled_value)) +
  geom_tile(color = "white", size = 0.2) +
  facet_wrap(~metric_clean, ncol = 5) +
  scale_fill_viridis_c(option = "viridis", name = "Scaled Value", 
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5)) +
  labs(
    title = "Comparative Analysis of Landscape Metrics",
    subtitle = "Terrain Dependency Parameter Effects (scaled values)",
    x = "Terrain Dependency Value",
    y = "Land Cover Class",
    caption = "Values are scaled within each metric for comparative visualization"
  ) +
  theme_publication() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.border = element_rect(fill = NA, color = "gray70"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = rel(0.8)),
    strip.background = element_rect(fill = "gray95", color = "gray70")
  )

heatmap_plot <- add_panel_labels(heatmap_plot, "g")
ggsave("heatmap_plot.jpeg", heatmap_plot, height = 7, width = 8, dpi = 350)



# Create a supplementary table of summary metrics with nicer formatting
summary_table <- all_metrics_filtered %>%
  group_by(parameter, parameter_value, class_name) %>%
  summarise(
    fragmentation = round(mean(np_value), 1),
    connectivity = round(mean(cohesion_value), 2),
    edge_density = round(mean(ed_value), 2),
    shape_complexity = round(mean(shape_mn_value), 3),
    aggregation = round(mean(ai_value), 2),
    .groups = "drop"
  ) %>%
  arrange(parameter, parameter_value, class_name)

# Print the summary table
print(summary_table, n = 30)

# Save as CSV for reference
write.csv(summary_table, "landscape_metrics_summary.csv", row.names = FALSE)
