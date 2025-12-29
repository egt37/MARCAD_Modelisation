library(tidyverse)
library(stringr)
getwd()
BaseG <- read_csv ('Base travail Edouard_GC-Climate_OS_2025.csv')
str(BaseG)
library(dplyr)

GloSum <- BaseG %>%
  group_by(Village, Campagnes) %>%
  summarise(
    total_records = n(),
    
    # Species count: Anopheles arabiensis
    arabiensis_count = sum(Identification.moléculaire.des.espèces == "Anopheles arabiensis", na.rm = TRUE),
    
    # ELISA CSP results
    CSP_positive = sum(ELISA.CSP == "Positif", na.rm = TRUE),
    CSP_negative = sum(ELISA.CSP == "Négatif", na.rm = TRUE),
    
    # Capture method breakdown
    CAH_method = sum(Methode.de.collecte == "CAH", na.rm = TRUE),
    CDC_method = sum(Methode.de.collecte == "CDC", na.rm = TRUE),
    OTHER_method = sum(!Methode.de.collecte %in% c("CAH", "CDC") & !is.na(Methode.de.collecte), na.rm = TRUE),
    
    .groups = "drop"
  )



# 🧲 Recode capture methods: only CAH and FMR retained
BaseG <- BaseG %>%
  mutate(
    Methode_recode = case_when(
      Methode.de.collecte %in% c("CAH", "CDC_LT") ~ "CAH",
      Methode.de.collecte %in% c("FMR","Prokopack", "piège de sortie") ~ "FMR",
      TRUE ~ NA_character_  # Exclude all other methods
    )
  )

# 🧬 Recode species from Espèces
BaseG <- BaseG %>%
  mutate(
    Espèce_recode = case_when(
      Espèces == "Anopheles gambiae s.l." ~ "Anopheles gambiae s.l.",
      Espèces == "Anopheles funestus" ~ "Anopheles funestus",
      TRUE ~ "other"
    )
  )


# 🧪 Recode ELISA Repas with flexible mixed detection
BaseG <- BaseG %>%
  mutate(
    blood_meal_type = case_when(
      # Mixed: Homme + any animal
      grepl("Homme", ELISA.Repas, ignore.case = TRUE) &
        grepl("mouton|boeuf|cheval|poule", ELISA.Repas, ignore.case = TRUE) ~ "mixed",
      
      # Human only
      grepl("Homme", ELISA.Repas, ignore.case = TRUE) ~ "human",
      
      # Animal only (no Homme)
      grepl("mouton|boeuf|cheval|poule", ELISA.Repas, ignore.case = TRUE) ~ "animal",
      
      # All others (NA or unclassified)
      TRUE ~ NA_character_
    )
  )





library(dplyr)

# 🧮 Step 1: Filter only relevant capture methods
BaseG_filtered <- BaseG %>%
  filter(Methode_recode %in% c("CAH", "FMR"))

# 🧵 Step 2: Create capture group variable
BaseG_filtered <- BaseG_filtered %>%
  mutate(
    capture_group = case_when(
      Methode_recode == "CAH" & Lieu.de.capture == "Intérieur" ~ "CAH_Intérieur",
      Methode_recode == "CAH" & Lieu.de.capture == "Extérieur" ~ "CAH_Extérieur",
      Methode_recode == "FMR" ~ "FMR",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(capture_group))  # Remove undefined combinations

# 📊 Step 3: Final compressed summary
CompressedSum <- BaseG_filtered %>%
  group_by(District, Village, Campagnes, capture_group) %>%
  summarise(
    total_records = n(),
    
    # Species counts
    gambiae_sl = sum(Espèce_recode == "Anopheles gambiae s.l.", na.rm = TRUE),
    funestus = sum(Espèce_recode == "Anopheles funestus", na.rm = TRUE),
    other_species = sum(Espèce_recode == "other", na.rm = TRUE),
    
    # ELISA CSP results
    CSP_positive = sum(ELISA.CSP == "Positif", na.rm = TRUE),
    CSP_negative = sum(ELISA.CSP == "Négatif", na.rm = TRUE),
    
    # Blood meal sources (overlapping counts allowed)
    mixed_blood_meal = sum(grepl("Homme", ELISA.Repas, ignore.case = TRUE) &
                             grepl("mouton|boeuf|cheval|poule", ELISA.Repas, ignore.case = TRUE), na.rm = TRUE),
    human_blood_meal = sum(grepl("Homme", ELISA.Repas, ignore.case = TRUE), na.rm = TRUE),
    animal_blood_meal = sum(grepl("mouton|boeuf|cheval|poule", ELISA.Repas, ignore.case = TRUE), na.rm = TRUE),
    
    .groups = "drop"
  )
EntoBase <- CompressedSum

# Epidemic data

EpiBase <- read.xlsx('C:/Users/edoug/OneDrive/Documents/MARCAD Modelisation/Bases/Bases Epi/PNLmalaria indicators_2010_2021.xlsx')
str(EpiBase)
library(dplyr)

EpiCompressed <- EpiBase %>%
  group_by(DISTRICT, ANNEE) %>%
  summarise(
    confirmed_cases_all = sum(`Nb..de.cas.de.paludisme.confirmé.Tout.âge`, na.rm = TRUE),
    confirmed_cases_under5 = sum(`Nb..de.cas.de.paludisme.confirmé.chez.les.Enfants.de.-.5.Ans`, na.rm = TRUE),
    confirmed_cases_pregnant = sum(`CPN.1er.Contact`, na.rm = TRUE),  # proxy if no direct column
    
    deaths_all = sum(`Décès.de.paludisme.:.Tout.âge`, na.rm = TRUE),
    deaths_under5 = sum(`Décès.de.paludisme.chez.les.moins.de.5.ans`, na.rm = TRUE),
    deaths_pregnant = sum(`Décès.de.paludisme.:.Femmes.Enceintes`, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  rename(
    Campagnes = ANNEE,
    District = DISTRICT
  )
# Combining EntoBase and Epibase
EntoBase <- CompressedSum %>%
  left_join(EpiCompressed, by = c("District", "Campagnes"))
EntoBase<- EntoBase %>%
  rename(Year = Campagnes)
# Cleaning
BaseG <- BaseG %>%
  mutate(District = case_when(
    District == "mbour" ~ "Mbour",
    TRUE ~ District
  ))

## Climatic data

ClimaBase <- read.csv("C:/Users/edoug/OneDrive/Documents/MARCAD Modelisation/Bases/Bases Climatique/climatic_variable_districts.csv", sep = ";")

unique(ClimaBase$district)
unique(BaseG$District)

ClimaBase <- ClimaBase %>%
  rename(District = district)
# renaming ClimaBase

ClimaBase <- ClimaBase %>%
  mutate(District = case_when(
    District == "KEDOUGOU" ~ "Kedougou",
    District == "VELINGARA" ~ "Velingara",
    District == "MBOUR" ~ "Mbour",
    District == "KAOLACK" ~ "Kaolack",
    District == "FATICK" ~ "Fatick",
    District == "DAGANA" ~ "Dagana",
    District == "NIORO DU RIP" ~ "Nioro",
    District == "NDOFFANE" ~ "Ndoffane",
    District == "RICHARD-TOLL" ~ "Richard Toll",
    District == "DIOFFIOR" ~ "Diofior",
    District == "KANEL" ~ "Kanel",
    District == "NIAKHAR" ~ "Niakhar",
    TRUE ~ District
  ))
 # Joint bases
JoinedBase <- BaseG %>%
  inner_join(ClimaBase, by = "District")
# Adding the year to the joint
names(BaseG)
names(ClimaBase)
BaseG <- BaseG %>% rename(Year = Campagnes)
ClimaBase <- ClimaBase %>% rename(Year = year)
JoinedBase <- BaseG %>%
  inner_join(ClimaBase, by = c("District", "Year"))
# Verifying
anti_join(BaseG, ClimaBase, by = c("District", "Year")) %>%
  distinct(District, Year)

# Joining Climabase to the sum of ento and epi base

EntoBase <- EntoBase %>%
  left_join(ClimaBase, by = c("District", "Year"))
# Checks
n_distinct(BaseG$Village)
n_distinct(EntoBase$Village)
names(EntoBase)
# checking for missing data audit
EntoBase %>%
  summarise(
    missing_precip = sum(is.na(precipitation)),
    missing_temp_max = sum(is.na(temp_max)),
    missing_CSP = sum(is.na(CSP_positive)),
    missing_blood = sum(is.na(mixed_blood_meal))
  )
n_distinct(EntoBase$Village)  # Should be 73
EntoBase %>%
  count(District, Year, Village) %>%
  filter(n > 1)
EntoBase %>%
  filter(is.na(Latitude) | is.na(Longitude))  # Only if those columns exist
EntoBase %>%
  count(Village, Year) %>%
  filter(n > 3)
EntoBase %>%
  count(Village, Year, capture_group) %>%
  group_by(Village, Year) %>%
  summarise(n_methods = n_distinct(capture_group)) %>%
  filter(n_methods > 3)
library(ggplot2)

EntoBase %>%
  count(District, Year) %>%
  ggplot(aes(x = Year, y = District, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()
EntoBase %>%
  filter(Village %in% c("Haouré", "Ndorong", "Tawa Mboudaye")) %>%
  count(Village, Year) %>%
  filter(n > 3)
names(EpiCompressed)
EpiCompressed <- EpiCompressed%>% rename(Year = Campagnes)
# Global joint Ento_Clima_Epi

EntoBase <- EntoBase %>%
  left_join(EpiCompressed, by = c("District", "Year"))
# Adding coordinates data

Coordinates <- read.xlsx("C:/Users/edoug/OneDrive/Documents/MARCAD Modelisation/Bases/Bases ENTO/Districts GCA_Village_coordonnees.xlsx")
names(Coordinates)
Coordinates <- Coordinates %>%
  select(-Colonne1)
Coordinates %>%
  count(Village) %>%
  filter(n > 1)
# Actual joint

EntoBase <- EntoBase %>%
  left_join(Coordinates, by = c("District", "Village"))
EntoBase %>%
  filter(is.na(Latitude) | is.na(Longitude)) %>%
  distinct(Village, District)

# Reordering the columns

EntoBase <- EntoBase %>%
  select(District, Village, Year, Latitude, Longitude, everything())

# exporting the base
write.csv(EntoBase, "EntoBase_Final.csv", row.names = FALSE)
library(openxlsx)
write.xlsx(EntoBase, "EntoBase_Final.xlsx")
# Visualizing the data coverage

library(ggplot2)
library(tidyr)

EntoBase_coverage <- EntoBase %>%
  mutate(
    has_climate = !is.na(precipitation) | !is.na(temp_max),
    has_epidemic = !is.na(confirmed_cases_all),
    has_coordinates = !is.na(Latitude) & !is.na(Longitude)
  ) %>%
  group_by(District, Year) %>%
  summarise(
    climate = any(has_climate),
    epidemic = any(has_epidemic),
    coordinates = any(has_coordinates),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = climate:coordinates, names_to = "layer", values_to = "present")

ggplot(EntoBase_coverage, aes(x = Year, y = District, fill = present)) +
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
# Exploring EntoBase
str(EntoBase)
names(EntoBase)
# Distinct species within an_gambiae_sl
EntoBase %>%
  distinct(An_gambiae_sl)
# Distinct species within An_gambiae_sl names
BaseG %>% group_by(Campagnes) %>%
  distinct(`Identification moléculaire des espèces`, Campagnes) %>% 
  # remove Nas and negatif
filter(!is.na(`Identification moléculaire des espèces`)) %>%
  filter(`Identification moléculaire des espèces` != "Négatif") %>%
  # drop the grping
  ungroup() %>% View()

