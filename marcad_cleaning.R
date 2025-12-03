# Loading Data ----

BaseG <- read_csv ('Base travail Edouard_GC-Climate_OS_2025.csv')
str(BaseG)
unique(BaseG$District)
BaseG <- BaseG %>% drop_na(District)
 # Try cleaning


library(dplyr)
unique(BaseG_Cleaned$District)

# 🧮 Step 1: Filter only relevant capture methods ----
BaseG_Cleaned <- BaseG %>%
  filter(`Methode de collecte` %in% c("CAH", "FMR", "CDC_LT", "Prokopack")) %>% 

# 🧵 Step 2: Create capture group variable
  mutate(
    capture_group = case_when(
      `Methode de collecte` == "CAH" & `Lieu de capture` == "Intérieur" ~ "CAH_Intérieur",
      `Methode de collecte` == "CAH" & `Lieu de capture` == "Extérieur" ~ "CAH_Extérieur",
      `Methode de collecte` == "FMR" ~ "FMR",
      `Methode de collecte` == "CDC_LT" ~ "CDC_LT",
      `Methode de collecte` == "Prokopack" ~ "Prokopack",
      TRUE ~ NA_character_
    )) %>% 
      mutate(
          Espèces = case_when(
          Espèces == "Anopheles gambiae s.l." ~ "Anopheles gambiae s.l.",
          Espèces == "Anopheles funestus" ~ "Anopheles funestus",
          TRUE ~ "other"
        )) %>%
      
  filter(!is.na(capture_group))  # Remove undefined combinations

# 📊 Step 3: Final compressed summary ----
EntoBase <- BaseG_Cleaned %>%
  group_by(District, Village, Campagnes, capture_group) %>%
  summarise(
    # Species counts
    An_gambiae_sl = sum(Espèces== "Anopheles gambiae s.l.", na.rm = TRUE),
    An_funestus = sum(Espèces == "Anopheles funestus", na.rm = TRUE),
    other_anopheles = sum(Espèces == "other", na.rm = TRUE),
    total_records = n(),
    
    # ELISA CSP results
    CSP_positive = sum(`ELISA CSP` == "Positif", na.rm = TRUE),
    CSP_negative = sum(`ELISA CSP` == "Négatif", na.rm = TRUE),
    
    # Blood meal sources (overlapping counts allowed)
    mixed_blood_meal = sum(grepl("Homme", `ELISA Repas`, ignore.case = TRUE) &
                             grepl("mouton|boeuf|cheval|poule", `ELISA Repas`, ignore.case = TRUE), na.rm = TRUE),
    human_blood_meal = sum(grepl("Homme", `ELISA Repas`, ignore.case = TRUE), na.rm = TRUE),
    animal_blood_meal = sum(grepl("mouton|boeuf|cheval|poule", `ELISA Repas`, ignore.case = TRUE), na.rm = TRUE),
    
    .groups = "drop"
  )

# Epidemic data ----

EpiBase <- read_csv('Base_Epi_Compil_2010-2021.csv')
str(EpiBase)
library(dplyr)
EntoBase <- EntoBase %>%  mutate(District = case_when(
  District == "Diofior" ~ "Dioffior",
  TRUE ~ District
)) # Try cleaning
EpiBase <- EpiBase %>%
  group_by(Districts, Campagnes) %>%
  summarise(
    confirmed_cases_under5 = sum(Cas_Paludisme_Enfant_moins_de_5_ans, na.rm = TRUE),
    confirmed_cases_pregnant = sum(Cas_Paludisme_Femme_enceinte, na.rm = TRUE),
    confirmed_cases_all = sum(Total_cas_paludisme, na.rm = TRUE), # proxy if no direct column
    
    deaths_under5 = sum(Deces_Paludisme_Enfant_moins_de_5_ans, na.rm = TRUE),
    deaths_pregnant = sum(Deces_Paludisme_Femme_enceinte, na.rm = TRUE),
    deaths_all = sum(Total_deces, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  rename(
    Campagnes = Annee,
    District = Districts
  )
# Combining EntoBase and Epibase ----
unique(BaseG$District)
unique(EntoBase$District)
unique(EpiBase$District)
EntoBase_EpiBase <- EntoBase %>%
  left_join(EpiBase, by = c("District", "Campagnes"))


## Climatic data ----

ClimaBase <- read.csv("climatic_districts.csv")

unique(ClimaBase$District)
unique(BaseG$District)

# Joining Climabase to the sum of ento and epi base ----
colnames(EntoBase_EpiBase)
colnames(ClimaBase)
ClimaBase <- ClimaBase %>% rename(Campagnes = Annee)

to_see <- ClimaBase %>%
  group_by(District, Campagnes) %>%
  filter(n() > 1) %>%
  ungroup()


ClimaBase_unique <- ClimaBase %>%
  distinct(District, Campagnes, .keep_all = TRUE) # Keeps the first row it encounters

EntoBase_EpiBase_ClimaBase <- EntoBase_EpiBase %>%
  left_join(ClimaBase_unique, by = c("District", "Campagnes"))
# Checks ----
n_distinct(BaseG$Village)
unique(BaseG$Village)
n_distinct(EntoBase_EpiBase_ClimaBase$Village)
unique(EntoBase_EpiBase_ClimaBase$Village)
names(EntoBase_EpiBase_ClimaBase)
# checking for missing data audit
EntoBase_EpiBase_ClimaBase %>%
  summarise(
    missing_precip = sum(is.na(precipitation)),
    missing_temp_max = sum(is.na(temp_max)),
    missing_CSP = sum(is.na(CSP_positive)),
    missing_blood = sum(is.na(mixed_blood_meal))
  )
n_distinct(EntoBase$Village)  # Should be 73
EntoBase_EpiBase_ClimaBase %>%
  count(District, Campagnes, Village) %>%
  filter(n > 1)


# Visualysing data 
library(ggplot2)

EntoBase_EpiBase_ClimaBase %>%
  count(District, Campagnes) %>%
  ggplot(aes(x = Campagnes, y = District, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()


# Adding coordinates data ----

Coordinates <- read_csv("Districts GCA_Village_coordonnees.csv")
names(Coordinates)
sum(is.na(Coordinates))

Coordinates %>%
  count(Village) %>%
  filter(n > 1)
# Actual joint

unique(EntoBase_EpiBase_ClimaBase$Village)

GlobalBase <- EntoBase_EpiBase_ClimaBase %>%
  left_join(Coordinates, by = c("District", "Village"))

GlobalBase %>%
  filter(is.na(Latitude) | is.na(Longitude)) %>%
  distinct(Village, District)

# Reordering the columns

GlobalBase <- GlobalBase %>%
  select(District, Village, Campagnes, Latitude, Longitude, everything())

# exporting the base ----
write.csv(EntoBase, "EntoBase_Final.csv", row.names = FALSE)
library(openxlsx)
write.xlsx(EntoBase, "EntoBase_Final.xlsx")
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

# Additional Climatic data (Just for the sake or coverage) ----
getwd()
ClimToClean <- read_csv("ClimToClean.csv")
AllBase <-  read.csv("AllBase.csv")

ClimToClean <- ClimToClean %>% 
  mutate(across(
    .cols = -(STATION:ANNEE),
    .fns = ~na_if(tolower(.), "x")))

str(ClimToClean)

ClimCleaned <- ClimToClean %>%
  pivot_longer(
    # Selects all columns from the one after ANNEE to the end (the month columns)
    cols = -(STATION:ANNEE),
    # Uses the values from the existing 'PARAMETRE' column as part of the new column names
    names_to = "Mois", 
    values_to = "Value"
  ) %>% mutate(Value = as.numeric(Value))
               
str(ClimCleaned)
unique(ClimCleaned$PARAMETRE)

ClimCleaned_aggregated <- ClimCleaned %>%
  # 1. Group by the station, year (ANNEE), and the parameter type
  group_by(STATION, ANNEE, PARAMETRE) %>%
  # 2. Summarize the 'Value' column conditionally
  summarize(
    Annual_Result = case_when(
      # If the parameter is precipitation, calculate the total sum
      PARAMETRE == "Cumul de pluie en mm" ~ sum(Value, na.rm = TRUE),
      
      # If the parameter is a temperature or humidity, calculate the mean
      PARAMETRE %in% c("Température moyenne maximale sous abri en ° celsuis",
                       "Température moyenne minimale sous abri en ° celsuis",
                       "Humidité maximale en %") ~ mean(Value, na.rm = TRUE),
      
      # Default condition
      TRUE ~ mean(Value, na.rm = TRUE) 
    ),
    .groups = 'drop' # Drop grouping levels after summarization
  ) %>% 
  distinct()

str(ClimCleaned_aggregated)

ClimCleaned_wide <- ClimCleaned_aggregated %>%
  pivot_wider(
    names_from = PARAMETRE,  # Column whose values will be the new column names
    values_from = Annual_Result # Column whose values will populate the new columns
  )

# View the structure of the new wide format tibble
str(ClimCleaned_wide)
colnames(GlobalBase)
colnames(ClimCleaned_wide)
ClimCleaned_wide <- ClimCleaned_wide %>% 
  rename(precipitation = `Cumul de pluie en mm`,
         temp_max = `Température moyenne maximale sous abri en ° celsuis`,
         temp_min = `Température moyenne minimale sous abri en ° celsuis`,
         District = STATION,
         Campagnes = ANNEE) 
ClimUpdated <-  ClimCleaned_wide %>% select(-`Humidité maximale en %`)


# Function to find non-numeric values in a vector
find_non_numeric <- function(x) {
  unique(x[is.na(as.numeric(x))])
}

# Check precipitation column for problematic values
print("Non-numeric values in precipitation:")
find_non_numeric(GlobalBase$precipitation)

# Check temp_max column for problematic values
print("Non-numeric values in temp_max:")
find_non_numeric(GlobalBase$temp_max)

# Check temp_min column for problematic values
print("Non-numeric values in temp_min:")
find_non_numeric(GlobalBase$temp_min)



library(dplyr)
library(stringr) # Usually loaded with tidyverse

# Define the columns that need cleaning and conversion
cols_to_clean <- c("actual_evapotranspi", "potential_evapotranspi", 
                   "soil_moisture", "windspeed", "precipitation", 
                   "temp_max", "temp_min")

GlobalBase_updated <- GlobalBase %>%
  mutate(
    # Apply operations across all specified columns
    across(
      .cols = all_of(cols_to_clean),
      .fns = ~ {
        # First, replace all commas with periods
        cleaned_strings <- str_replace_all(., pattern = ",", replacement = ".")
        # Second, convert the cleaned string to a numeric type
        as.numeric(cleaned_strings)
      }
    )
  )

# Verify the data types are now numeric (double) without warnings
str(GlobalBase_updated)



GlobalBase_updated <- GlobalBase_updated %>%
  rows_update(
    ClimUpdated,
    by = c("District", "Campagnes"),
    unmatched = "ignore" # Ignore rows in ClimUpdate that don't match GlobalBase keys
  )

# View the structure of the updated data frame
str(GlobalBase_updated)



Data_Cov <- GlobalBase_updated %>%
  mutate(
    has_entomologic = !is.na(capture_group) | !is.na(An_funestus),
    has_climate = !is.na(precipitation) | !is.na(temp_max),
    has_epidemic = !is.na(confirmed_cases_all),
    has_coordinates = !is.na(Latitude) & !is.na(Longitude)
  )

Data_Cov %>% group_by(District, Campagnes) %>%
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
getwd()
write_csv(GlobalBase_updated, "Global_Base_Update.csv")

ggsave(
  filename = "coverage_update.pdf",
  width = 10,
  height = 8,
  dpi = 300
)
