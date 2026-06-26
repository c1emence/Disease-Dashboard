# Centralized Setup Page for all .qmd files to use for rendering
# Last Updated: June 23, 2026

# read libraries
library(tidyverse)
library(tigris)
library(sf)
library(lubridate)
library(leaflet)
library(plotly)
library(bslib)
library(DT)

#read data
fake_cases <- read_csv(
  "data/fake_cases.csv",
  col_types = cols(
    GEOID = col_character()
  )
)

fake_cases_clean <- fake_cases %>%
  mutate(
    GEOID = as.character(GEOID),
    report_date = as.Date(report_date),
    disease = as.character(disease)
  )

# Census Tracts ####
brazos_tracts <- tigris::tracts(
  state = "TX",
  county = "Brazos",
  year = 2025,
  cb = TRUE
) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate(GEOID = as.character(GEOID))

str(fake_cases_clean$GEOID)
str(brazos_tracts$GEOID)

# DATA MANIPULATION ####

# Influenza
flu_counts <- fake_cases_clean %>%
  filter(disease == "Influenza") %>%
  count(GEOID, name = "cases")

flu_map <- brazos_tracts %>%
  left_join(flu_counts, by = "GEOID") %>%
  mutate(cases = replace_na(cases, 0))

flu_table <- flu_map %>%
  st_drop_geometry() %>%
  select(GEOID, cases) %>%
  arrange(desc(cases))

total_flu_cases <- sum(flu_table$cases)

flu_table <- flu_table %>%
  rename(
    "Census Tract" = GEOID,
    "Influenza Cases" = cases
  )

# COVID-19
covid_counts <- fake_cases %>%
  filter(disease == "COVID-19") %>%
  count(GEOID, name = "cases")

covid_map <- brazos_tracts %>%
  left_join(covid_counts, by = "GEOID") %>%
  mutate(cases = replace_na(cases, 0))

covid_table <- covid_map %>%
  st_drop_geometry() %>%
  select(GEOID, cases) %>%
  arrange(desc(cases))

total_covid_cases <- sum(covid_table$cases)

covid_table <- covid_table %>%
  rename(
    "Census Tract" = GEOID,
    "COVID Cases" = cases
  )

# FUNCTIONS ####

# color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = flu_map$cases,
  na.color = "transparent"
)
