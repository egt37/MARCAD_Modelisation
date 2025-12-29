# Visualizing the data coverage ----

library(ggplot2)
library(tidyr)

GlobalBase_coverage <- GlobalBase %>%
  mutate(
    has_entomologic = !is.na(capture_group) | !is.na(An_funestus),
    has_climate = !is.na(precipitation) | !is.na(temp_max),
    has_epidemic = !is.na(confirmed_cases_all),
    has_coordinates = !is.na(Latitude) & !is.na(Longitude)
  ) 



GlobalBase_coverage %>%   group_by(District, Campagnes) %>%
  summarise(
    entomologic = any(has_entomologic),
    climate = any(has_climate),
    epidemic = any(has_epidemic),
    coordinates = any(has_coordinates),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = entomologic:coordinates, names_to = "layer", values_to = "present") %>% 
  ggplot(aes(x = Campagnes, y = District, fill = present)) +
  geom_tile(color = "white") +
  facet_wrap(~ layer, ncol = 1) +
  scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
                    name = "Data Present",
                    labels = c("No", "Yes")) +
  labs(
    title = "Data Coverage by District and Year",
    x = "Year",
    y = "District"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

write_csv(GlobalBase, "Global_Base.csv")

ggsave(
  filename = "coverage.png",
  width = 10,
  height = 8,
  dpi = 300
)
# Adding geometry to the GlobalBase ----
# loading shapefile
library(sf)
sen_map <- st_read("gadm41_SEN_2.shp")
colnames(sen_map)
sen_map <- sen_map %>% 
  select(-GID_0, -GID_1,-GID_2, -COUNTRY,
         -NL_NAME_1, -CC_2, -VARNAME_2,
         -HASC_2, -ENGTYPE_2, -TYPE_2,
         -NL_NAME_2) %>% # Cleaning
  rename(Regions = NAME_1, 
         Districts = NAME_2)
# adding geometry to GlobalBase by spatial join
GlobalBase_sf <- st_as_sf(GlobalBase, coords = c("Longitude", "Latitude")
                         , crs = 4326, remove = FALSE)
# Spatial join
GlobalBase_sf <- st_join(GlobalBase_sf, sen_map, join = st_within, left = TRUE)
# Now GlobalBase_sf has geometry column
# Let's make a coverage dataframe with geometry
GlobalBase_coverage_sf <- GlobalBase_sf %>%
  mutate(
    has_entomologic = !is.na(capture_group) | !is.na(An_funestus),
    has_coordinates = !is.na(Latitude) & !is.na(Longitude),
    has_climate = !is.na(precipitation) | !is.na(temp_max),
    has_epidemic = !is.na(confirmed_cases_all)
  ) %>%
  group_by(Districts, Campagnes) %>%   
  summarise(
    entomologic = any(has_entomologic),
    coordinates = any(has_coordinates),
    climate = any(has_climate),
    epidemic = any(has_epidemic),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = entomologic:epidemic,
    names_to = "layer",
    values_to = "present"
  )
# Plotting with geometry
ggplot() +
  geom_sf(data = sen_map, fill = "grey90", color = "white") +
  geom_sf(
    data = filter(GlobalBase_coverage_sf, layer == "epidemic"),
    aes(fill = present),
    color = "black",
    size = 0.2
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
    labels = c("No", "Yes")
  ) +
  facet_wrap(~ Campagnes) +
  theme_minimal() +
  labs(
    title = "Epidemic Data Coverage by District and Campaign",
    fill = "Data Present"
  ) +
  theme(
    legend.position = "right"
  )
for (layer_name in unique(GlobalBase_coverage_sf$layer)) {
  
  p <- ggplot() +
    geom_sf(data = sen_map, fill = "grey90", color = "white") +
    geom_sf(
      data = filter(GlobalBase_coverage_sf, layer == layer_name),
      aes(fill = present),
      color = "black",
      size = 0.2
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
      labels = c("No", "Yes")
    ) +
    labs(title = paste("Data coverage:", layer_name)) +
    theme_minimal()
  
  print(p)
}

GlobalBase_samples_sf <- GlobalBase_sf %>%
  mutate(
    has_climate  = !is.na(precipitation) | !is.na(temp_max),
    has_epidemic = !is.na(confirmed_cases_all)
  )
# Plotting sample locations with geometry
p_climate <- ggplot() +
  geom_sf(data = sen_map, fill = "grey90", color = "white") +
  geom_sf(
    data = GlobalBase_samples_sf,
    aes(color = has_climate),
    size = 1
  ) +
  scale_color_manual(
    values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
    labels = c("No climate data","Climate data")
  ) +
  facet_wrap(~ Campagnes) +
  theme_minimal() +
  labs(
    title = "Climatic Metadata Availability per Sample",
    color = "Metadata"
  )
# Plotting sample locations with geometry for epidemic data
p_epidemic <- ggplot() +
  geom_sf(data = sen_map, fill = "grey90", color = "white") +
  geom_sf(
    data = GlobalBase_samples_sf,
    aes(color = has_epidemic),
    size = 1
  ) +
  scale_color_manual(
    values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
    labels = c("No epidemic data","Epidemic data")
  ) +
  facet_wrap(~ Campagnes) +
  theme_minimal() +
  labs(
    title = "Epidemic Metadata Availability per Sample",
    color = "Metadata"
  )
# Saving the p_epidemic plot
library(ggplot2)
ggsave(
  filename = "Epidemic_coverage.pdf",
  plot = p_epidemic,
  width = 10,
  height = 8,
  dpi = 1000
)
# Save climate plot
ggsave(
  filename = "Climatic_coverage.pdf",
  plot = p_climate,
  width = 10,
  height = 8,
  dpi = 1000
)

# Same map but with species presence
GlobalBase_species_sf <- GlobalBase_samples_sf %>%
  mutate(
    has_An_gambiae_sl = An_gambiae_sl > 0,
    has_An_funestus = An_funestus > 0,
    has_other_species = other_anopheles > 0
  )

species_long <- GlobalBase_species_sf %>%
  pivot_longer(
    cols = c(has_An_gambiae_sl, has_An_funestus, has_other_species),
    names_to = "species",
    values_to = "present"
  ) %>%
  filter(present == TRUE)

p_species <- ggplot() +
  geom_sf(data = sen_map, fill = "grey90", color = "white") +
  geom_sf(
    data = species_long,
    aes(color = species, shape = species),
    size = 2
  ) +
  facet_wrap(~ Campagnes) +
  scale_color_manual(
    name = "Species",
    values = c(
      has_An_gambiae_sl = "#1b9e77",
      has_An_funestus   = "#d95f02",
      has_other_species = "#7570b3"
    ),
    labels = c(
      has_An_gambiae_sl = "An. gambiae s.l.",
      has_An_funestus   = "An. funestus",
      has_other_species = "Other species"
    )
  ) +
  scale_shape_manual(
    name = "Species",
    values = c(
      has_An_gambiae_sl = 16,
      has_An_funestus   = 17,
      has_other_species = 15
    ),
    labels = c(
      has_An_gambiae_sl = "An. gambiae s.l.",
      has_An_funestus   = "An. funestus",
      has_other_species = "Other species"
    )
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3)),
    shape = guide_legend(override.aes = list(size = 3))
  ) +
  theme_minimal() +
  labs(
    title = "Species Presence"
  )

p_species
ggsave(
  filename = "Species_Presence.pdf",
  plot = p_species,
  width = 10,
  height = 8,
  dpi = 1500
)
# End of Visualisations.R ----