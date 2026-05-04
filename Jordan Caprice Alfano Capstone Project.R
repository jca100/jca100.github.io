# ============================================================
# DATA CLEANING, PREP, AND VISUALIZATIONS
# ============================================================

# ============================================================
# 0. LOAD LIBRARIES
# ============================================================

library(tidyverse)
library(sf)
library(tigris)
library(readxl)
library(janitor)
library(lubridate)
library(stringr)

options(tigris_use_cache = TRUE)

# ============================================================
# 1. SET FILE PATHS
# ============================================================

arrest_path <- "C:/Users/jcas2/Downloads/jca100.github.io/NYPD_Arrests_Data_(Historic)_-_Dangerous_Drugs_20260331.csv"
acs1_path   <- "C:/Users/jcas2/Downloads/jca100.github.io/acs1-data.csv"
acs5_path   <- "C:/Users/jcas2/Downloads/jca100.github.io/acs5-data.csv"
od_file     <- "C:/Users/jcas2/Downloads/jca100.github.io/arrests_cd_overdose_clean_2006_2021.csv"
drug_path   <- "C:/Users/jcas2/Downloads/jca100.github.io/nyc_cd_drug_related_2022_2023.csv"

# ============================================================
# 2. READ DATA
# ============================================================

arrests <- read_csv(arrest_path, show_col_types = FALSE) |>
  mutate(
    ARREST_DATE = mdy(ARREST_DATE),
    arrest_year = year(ARREST_DATE)
  ) |>
  filter(!is.na(Longitude), !is.na(Latitude))

acs1 <- read_csv(acs1_path, show_col_types = FALSE) |>
  mutate(source = "acs1")

acs5 <- read_csv(acs5_path, show_col_types = FALSE) |>
  mutate(source = "acs5")

overdose_cd <- read_csv(od_file, show_col_types = FALSE)

drug_pop <- read_csv(drug_path, show_col_types = FALSE)

# ============================================================
# 3. BUILD SPATIAL ARRESTS DATA
# ============================================================

arrests_sf <- arrests |>
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )

# ============================================================
# 4. GEOGRAPHIES
# ============================================================

puma_sf_nyc <- tigris::pumas(state = "NY", year = 2020, cb = TRUE) |>
  st_transform(4326) |>
  rename(
    GEOID  = GEOID20,
    PUMACE = PUMACE20,
    NAME   = NAMELSAD20
  ) |>
  filter(str_detect(NAME, "Bronx|Brooklyn|Manhattan|Queens|Staten Island")) |>
  select(GEOID, PUMACE, NAME, geometry)

cd_sf <- st_read(
  "https://data.cityofnewyork.us/resource/5crt-au7u.geojson?$limit=1000",
  quiet = TRUE
) |>
  st_transform(4326) |>
  mutate(
    boro_cd = as.integer(boro_cd),
    boro_name = case_when(
      boro_cd %/% 100 == 1 ~ "Manhattan",
      boro_cd %/% 100 == 2 ~ "Bronx",
      boro_cd %/% 100 == 3 ~ "Brooklyn",
      boro_cd %/% 100 == 4 ~ "Queens",
      boro_cd %/% 100 == 5 ~ "Staten Island",
      TRUE ~ NA_character_
    )
  ) |>
  select(boro_cd, boro_name, geometry)

# ============================================================
# 5. SPATIAL JOINS
# ============================================================

arrests_geo <- arrests_sf |>
  st_join(puma_sf_nyc, join = st_within, left = TRUE) |>
  st_join(cd_sf, join = st_within, left = TRUE)

# ============================================================
# 6. CLEAN VARIABLES
# ============================================================

arrests_geo <- arrests_geo |>
  mutate(
    drug_category = case_when(
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 7" ~ "Controlled substance possession: 7th degree",
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 5" ~ "Controlled substance possession: 5th degree",
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 4" ~ "Controlled substance possession: 4th degree",
      PD_DESC == "CONTROLLED SUBSTANCE,POSSESS. 3" ~ "Controlled substance possession: 3rd degree",
      PD_DESC == "CONTROLLED SUBSTANCE,POSSESS. 2" ~ "Controlled substance possession: 2nd degree",
      PD_DESC == "CONTROLLED SUBSTANCE,POSSESS. 1" ~ "Controlled substance possession: 1st degree",
      PD_DESC %in% c(
        "CONTROLLED SUBSTANCE, POSSESSI",
        "CONTROLLED SUBSTANCE,POSSESS."
      ) ~ "Controlled substance possession: unspecified degree",
      PD_DESC == "DRUG, INJECTION OF" ~ "Drug injection offense",
      PD_DESC == "MARIJUANA, POSSESSION 4 & 5" ~ "Marijuana possession: lower degree",
      PD_DESC == "MARIJUANA, POSSESSION 1, 2 & 3" ~ "Marijuana possession: higher degree",
      PD_DESC == "USE CHILD TO COMMIT CONT SUB OFF" ~ "Aggravated drug offense involving child",
      TRUE ~ "Other or unknown drug offense"
    ),
    drug_category_broad = case_when(
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 7" ~ "controlled_substance_possession_low",
      PD_DESC %in% c(
        "CONTROLLED SUBSTANCE, POSSESSION 5",
        "CONTROLLED SUBSTANCE, POSSESSION 4"
      ) ~ "controlled_substance_possession_mid",
      PD_DESC %in% c(
        "CONTROLLED SUBSTANCE,POSSESS. 3",
        "CONTROLLED SUBSTANCE,POSSESS. 2",
        "CONTROLLED SUBSTANCE,POSSESS. 1"
      ) ~ "controlled_substance_possession_high",
      PD_DESC == "MARIJUANA, POSSESSION 4 & 5" ~ "marijuana_possession_low",
      PD_DESC == "MARIJUANA, POSSESSION 1, 2 & 3" ~ "marijuana_possession_high",
      PD_DESC %in% c(
        "DRUG, INJECTION OF",
        "USE CHILD TO COMMIT CONT SUB OFF",
        "CONTROLLED SUBSTANCE, POSSESSI",
        "CONTROLLED SUBSTANCE,POSSESS."
      ) ~ "other_drug_offense",
      TRUE ~ "other_or_unknown_drug_offense"
    ),
    law_cat_label = case_when(
      LAW_CAT_CD == "F" ~ "Felony",
      LAW_CAT_CD == "M" ~ "Misdemeanor",
      LAW_CAT_CD == "V" ~ "Violation",
      TRUE ~ "Other / missing"
    ),
    age_group_clean = case_when(
      AGE_GROUP %in% c("<18", "18-24", "25-44", "45-64", "65+") ~ AGE_GROUP,
      AGE_GROUP == "UNKNOWN" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    perp_race_acs = case_when(
      PERP_RACE == "WHITE" ~ "white",
      PERP_RACE == "BLACK" ~ "black",
      PERP_RACE == "ASIAN / PACIFIC ISLANDER" ~ "asian",
      PERP_RACE %in% c("WHITE HISPANIC", "BLACK HISPANIC") ~ "hispanic",
      PERP_RACE %in% c("AMERICAN INDIAN/ALASKAN NATIVE", "OTHER", "UNKNOWN") ~ "other_or_unknown",
      TRUE ~ NA_character_
    ),
    perp_sex_label = case_when(
      PERP_SEX == "F" ~ "Female",
      PERP_SEX == "M" ~ "Male",
      PERP_SEX == "U" ~ "Unknown",
      TRUE ~ "Other / missing"
    )
  )

# ============================================================
# 7. ACS PANEL AT PUMA-YEAR LEVEL
# ============================================================

acs_all <- bind_rows(
  acs1 |> filter(year <= 2008),
  acs5 |> filter(year >= 2009)
) |>
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  )

arrests_puma_agg <- arrests_geo |>
  st_drop_geometry() |>
  filter(!is.na(GEOID), !is.na(arrest_year)) |>
  group_by(GEOID, NAME, arrest_year) |>
  summarize(
    arrests_total = n(),
    arrests_felony = sum(law_cat_label == "Felony", na.rm = TRUE),
    arrests_misdemeanor = sum(law_cat_label == "Misdemeanor", na.rm = TRUE),
    cs_possession_low = sum(drug_category_broad == "controlled_substance_possession_low", na.rm = TRUE),
    cs_possession_mid = sum(drug_category_broad == "controlled_substance_possession_mid", na.rm = TRUE),
    cs_possession_high = sum(drug_category_broad == "controlled_substance_possession_high", na.rm = TRUE),
    mj_possession_low = sum(drug_category_broad == "marijuana_possession_low", na.rm = TRUE),
    mj_possession_high = sum(drug_category_broad == "marijuana_possession_high", na.rm = TRUE),
    other_drug_offense = sum(drug_category_broad == "other_drug_offense", na.rm = TRUE),
    arrests_female = sum(perp_sex_label == "Female", na.rm = TRUE),
    arrests_male = sum(perp_sex_label == "Male", na.rm = TRUE),
    arrests_under18 = sum(age_group_clean == "<18", na.rm = TRUE),
    arrests_18_24 = sum(age_group_clean == "18-24", na.rm = TRUE),
    arrests_25_44 = sum(age_group_clean == "25-44", na.rm = TRUE),
    arrests_45_64 = sum(age_group_clean == "45-64", na.rm = TRUE),
    arrests_65plus = sum(age_group_clean == "65+", na.rm = TRUE),
    arrests_white = sum(perp_race_acs == "white", na.rm = TRUE),
    arrests_black = sum(perp_race_acs == "black", na.rm = TRUE),
    arrests_asian = sum(perp_race_acs == "asian", na.rm = TRUE),
    arrests_hispanic = sum(perp_race_acs == "hispanic", na.rm = TRUE),
    .groups = "drop"
  )

arrests_acs <- arrests_puma_agg |>
  left_join(
    acs_all,
    by = c("GEOID", "arrest_year" = "year")
  )

# ============================================================
# 8. COMMUNITY DISTRICT-YEAR PANEL
# ============================================================

arrests_cd_agg <- arrests_geo |>
  st_drop_geometry() |>
  filter(!is.na(boro_cd), !is.na(arrest_year)) |>
  group_by(boro_cd, boro_name, arrest_year) |>
  summarize(
    arrests_total = n(),
    arrests_felony = sum(law_cat_label == "Felony", na.rm = TRUE),
    arrests_misdemeanor = sum(law_cat_label == "Misdemeanor", na.rm = TRUE),
    cs_possession_low = sum(drug_category_broad == "controlled_substance_possession_low", na.rm = TRUE),
    cs_possession_mid = sum(drug_category_broad == "controlled_substance_possession_mid", na.rm = TRUE),
    cs_possession_high = sum(drug_category_broad == "controlled_substance_possession_high", na.rm = TRUE),
    mj_possession_low = sum(drug_category_broad == "marijuana_possession_low", na.rm = TRUE),
    mj_possession_high = sum(drug_category_broad == "marijuana_possession_high", na.rm = TRUE),
    other_drug_offense = sum(drug_category_broad == "other_drug_offense", na.rm = TRUE),
    arrests_female = sum(perp_sex_label == "Female", na.rm = TRUE),
    arrests_male = sum(perp_sex_label == "Male", na.rm = TRUE),
    arrests_under18 = sum(age_group_clean == "<18", na.rm = TRUE),
    arrests_18_24 = sum(age_group_clean == "18-24", na.rm = TRUE),
    arrests_25_44 = sum(age_group_clean == "25-44", na.rm = TRUE),
    arrests_45_64 = sum(age_group_clean == "45-64", na.rm = TRUE),
    arrests_65plus = sum(age_group_clean == "65+", na.rm = TRUE),
    arrests_white = sum(perp_race_acs == "white", na.rm = TRUE),
    arrests_black = sum(perp_race_acs == "black", na.rm = TRUE),
    arrests_asian = sum(perp_race_acs == "asian", na.rm = TRUE),
    arrests_hispanic = sum(perp_race_acs == "hispanic", na.rm = TRUE),
    .groups = "drop"
  )

overdose_cd_one <- overdose_cd |>
  mutate(
    boro_cd = as.integer(boro_cd),
    arrest_year = as.integer(arrest_year)
  ) |>
  group_by(boro_cd, arrest_year) |>
  summarize(
    overdose_total = sum(overdose_total, na.rm = TRUE),
    .groups = "drop"
  )

arrests_cd_overdose <- arrests_cd_agg |>
  left_join(overdose_cd_one, by = c("boro_cd", "arrest_year"))

# ============================================================
# 9. LOOKUP TABLE FOR COMMUNITY DISTRICT NAMES
# ============================================================

cd_lookup <- tribble(
  ~boro_cd, ~cd_name,
  101, "Battery Park / Tribeca",
  102, "Greenwich Village / SoHo",
  103, "Lower East Side / Chinatown",
  104, "Chelsea / Clinton",
  105, "Midtown Business District",
  106, "Murray Hill / Stuyvesant Town",
  107, "Upper West Side",
  108, "Upper East Side",
  109, "Manhattanville / West Harlem",
  110, "Central Harlem",
  111, "East Harlem",
  112, "Washington Heights / Inwood",
  201, "Mott Haven / Melrose",
  202, "Hunts Point / Longwood",
  203, "Morrisania / Crotona",
  204, "Concourse / Highbridge",
  205, "Fordham / University Heights",
  206, "Belmont / East Tremont",
  207, "Kingsbridge Heights / Bedford",
  208, "Riverdale / Fieldston",
  209, "Parkchester / Soundview",
  210, "Throgs Neck / Co-op City",
  211, "Morris Park / Bronxdale",
  212, "Williamsbridge / Baychester",
  301, "Greenpoint / Williamsburg",
  302, "Fort Greene / Brooklyn Heights",
  303, "Bedford-Stuyvesant",
  304, "Bushwick",
  305, "East New York / Starrett City",
  306, "Park Slope / Carroll Gardens",
  307, "Sunset Park",
  308, "Crown Heights North / Prospect Heights",
  309, "Crown Heights South / Lefferts Gardens",
  310, "Bay Ridge / Dyker Heights",
  311, "Bensonhurst",
  312, "Borough Park",
  313, "Coney Island",
  314, "Flatbush / Midwood",
  315, "Sheepshead Bay",
  316, "Brownsville",
  317, "East Flatbush",
  318, "Flatlands / Canarsie",
  401, "Long Island City / Astoria",
  402, "Woodside / Sunnyside",
  403, "Jackson Heights",
  404, "Elmhurst / Corona",
  405, "Ridgewood / Maspeth",
  406, "Rego Park / Forest Hills",
  407, "Flushing",
  408, "Hillcrest / Fresh Meadows",
  409, "Kew Gardens / Woodhaven",
  410, "South Ozone Park / Howard Beach",
  411, "Bayside / Little Neck",
  412, "Jamaica / Hollis",
  413, "Queens Village",
  414, "Rockaway / Broad Channel",
  501, "St. George / Stapleton",
  502, "South Beach / Willowbrook",
  503, "Tottenville / Great Kills"
)

arrests_cd_overdose <- arrests_cd_overdose |>
  left_join(cd_lookup, by = "boro_cd") |>
  relocate(cd_name, .after = boro_cd)

# ============================================================
# 10. MAP DATA
# ============================================================

cd_map <- cd_sf |>
  left_join(arrests_cd_overdose, by = "boro_cd")

cd_map_2021 <- cd_map |>
  filter(arrest_year == 2021)

# ============================================================
# 11. SAVE CLEAN FILES
# ============================================================

write_csv(arrests_acs, "arrests_puma_acs_clean.csv")

arrests_cd_overdose_2006_2021 <- arrests_cd_overdose |>
  filter(arrest_year <= 2021)

write_csv(
  arrests_cd_overdose_2006_2021,
  "arrests_cd_overdose_clean_2006_2021.csv"
)

# ============================================================
# 12. VISUALIZATIONS
# ============================================================

# Overdose deaths in NYC over time
city_year <- arrests_cd_overdose |>
  group_by(arrest_year) |>
  summarize(
    overdose_total = sum(overdose_total, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(city_year, aes(x = arrest_year, y = overdose_total)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Year",
    y = "Overdose Deaths",
    title = "Overdose Deaths in NYC Over Time"
  ) +
  theme_minimal()

# Drug arrests and overdoses over time
city_year <- arrests_cd_overdose |>
  group_by(arrest_year) |>
  summarize(
    arrests_total = sum(arrests_total, na.rm = TRUE),
    overdose_total = sum(overdose_total, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(city_year, aes(x = arrest_year)) +
  geom_line(aes(y = arrests_total, color = "Arrests")) +
  geom_line(aes(y = overdose_total, color = "Overdoses")) +
  labs(
    x = "Year",
    y = "Count",
    color = "",
    title = "Drug Arrests and Overdose Deaths in NYC Over Time"
  ) +
  theme_minimal()

# Scatterplot
ggplot(arrests_cd_overdose, aes(x = arrests_total, y = overdose_total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Higher Drug Arrests Are Associated with Lower Overdose Deaths",
    subtitle = "Community district-level relationship in NYC",
    x = "Drug-related arrests",
    y = "Overdose deaths"
  ) +
  theme_minimal()

# Borough comparison
borough_2021 <- arrests_cd_overdose |>
  filter(arrest_year == 2021) |>
  group_by(boro_name) |>
  summarize(
    arrests_total = sum(arrests_total, na.rm = TRUE),
    overdose_total = sum(overdose_total, na.rm = TRUE),
    .groups = "drop"
  )

borough_long <- borough_2021 |>
  pivot_longer(
    cols = c(arrests_total, overdose_total),
    names_to = "type",
    values_to = "count"
  )

ggplot(
  borough_long,
  aes(x = reorder(boro_name, count), y = count, fill = type)
) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Comparing Drug-Related Arrests and Overdose Deaths by Borough (2021)",
    subtitle = "Borough-level comparison in NYC",
    x = "",
    y = "Total Count",
    fill = ""
  ) +
  scale_fill_manual(
    values = c(
      "overdose_total" = "#F9844A",
      "arrests_total" = "#277DA1"
    ),
    labels = c("Overdose Deaths", "Drug-Related Arrests")
  ) +
  theme_minimal()

# Harlem hotspot chart
manhattan <- drug_pop |>
  filter(year == 2023, borough == "Manhattan") |>
  mutate(
    area = case_when(
      community_district %in% c(
        "Central Harlem",
        "East Harlem",
        "Manhattanville",
        "Manhattanville / West Harlem"
      ) ~ "Harlem",
      TRUE ~ "Other Manhattan"
    )
  )

ggplot(
  manhattan,
  aes(
    x = reorder(community_district, crude_rate_per_100k),
    y = crude_rate_per_100k,
    fill = area
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Harlem" = "#F94144",
      "Other Manhattan" = "grey70"
    )
  ) +
  labs(
    title = "Harlem Stands Out as an Overdose Hotspot",
    subtitle = "Crude overdose death rates per 100,000 residents in Manhattan (2023)",
    x = "",
    y = "Crude Rate per 100,000",
    fill = ""
  ) +
  theme_minimal()

#Overdose deaths for Manhattan Community District 
manhattan <- drug_pop |>
  filter(year == 2023, borough == "Manhattan")

ggplot(
  manhattan,
  aes(
    x = reorder(community_district, crude_rate_per_100k),
    y = crude_rate_per_100k,
    fill = community_district
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Overdose Rates by Manhattan Community District (2023)",
    subtitle = "Crude overdose death rates per 100,000 residents",
    x = "",
    y = "Crude Rate per 100,000",
    fill = "Community District"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # removes messy legend

#All NON-Manhattan boroughs our outside of Manhattan
not_manhattan <- drug_pop |>
  filter(year == 2023, borough != "Manhattan")

ggplot(
  not_manhattan,
  aes(
    x = reorder(community_district, crude_rate_per_100k),
    y = crude_rate_per_100k,
    fill = borough
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Overdose Rates Outside Manhattan (2023)",
    subtitle = "Crude overdose death rates per 100,000 residents",
    x = "",
    y = "Crude Rate per 100,000",
    fill = "Borough"
  ) +
  theme_minimal()

#All boroughs and neighborhoods 
all_nyc <- drug_pop |>
  filter(year == 2023)

ggplot(
  all_nyc,
  aes(
    x = reorder(community_district, crude_rate_per_100k),
    y = crude_rate_per_100k,
    fill = borough
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Overdose Rates by NYC Community District (2023)",
    subtitle = "Crude overdose death rates per 100,000 residents",
    x = "",
    y = "Crude Rate per 100,000",
    fill = "Borough"
  ) +
  theme_minimal()

# Overdose map
ggplot(cd_map_2021) +
  geom_sf(aes(fill = overdose_total), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Overdose deaths"
  ) +
  labs(
    title = "Overdose Deaths by Community District (NYC, 2021)",
    caption = "Source: NYC DOHMH & NYPD data"
  ) +
  theme_minimal()

# Arrest map
ggplot(cd_map_2021) +
  geom_sf(aes(fill = arrests_total), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    trans = "log",
    option = "viridis",
    name = "Drug arrests (log scale)"
  ) +
  labs(
    title = "Drug Arrests by Community District (NYC, 2021)",
    caption = "Source: NYPD Arrest Data"
  ) +
  theme_minimal()

# ============================================================
# 13. LAG MODELS
# ============================================================

analysis_lag <- arrests_cd_overdose |>
  arrange(boro_cd, arrest_year) |>
  group_by(boro_cd) |>
  mutate(
    arrests_lag1 = lag(arrests_total, 1),
    arrests_lag2 = lag(arrests_total, 2),
    arrests_lag3 = lag(arrests_total, 3)
  ) |>
  ungroup() |>
  mutate(
    log_overdose = log1p(overdose_total),
    log_arrests_lag1 = log1p(arrests_lag1),
    log_arrests_lag3 = log1p(arrests_lag3)
  )

model_lag1 <- lm(overdose_total ~ arrests_lag1, data = analysis_lag)
summary(model_lag1)

model_lag3 <- lm(overdose_total ~ arrests_lag3, data = analysis_lag)
summary(model_lag3)

model_log_lag1 <- lm(log_overdose ~ log_arrests_lag1, data = analysis_lag)
summary(model_log_lag1)

model_log_lag3 <- lm(log_overdose ~ log_arrests_lag3, data = analysis_lag)
summary(model_log_lag3)

model_control <- lm(
  log_overdose ~ log_arrests_lag1 + arrests_total,
  data = analysis_lag
)

summary(model_control)

ggplot(analysis_lag, aes(x = arrests_lag1, y = overdose_total)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Past Drug Arrests Are Associated with Lower Future Overdose Deaths",
    subtitle = "1-year lag relationship",
    x = "Drug arrests (previous year)",
    y = "Overdose deaths"
  ) +
  theme_minimal()