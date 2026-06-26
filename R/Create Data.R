# Date: 06/23/2026

# Objective: Create Fake Dataset

# Project: BCHD Dashboard
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages ####
install.packages("pacman")
pacman::p_load(DT, tidyverse, tigris, sf, pacman, lubridate, leaflet, ggplot2, plotly, bslib)
library(tidyverse)
library(tigris)
library(sf)
library(lubridate)
library(leaflet)
library(plotly)
library(bslib)

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

glimpse(brazos_tracts)

plot(brazos_tracts["GEOID"])

# Create Dataset ####
set.seed(0905)
fake_cases <- tibble(
  case_id = 1:1000,
  disease = sample(
    c("COVID-19", "Influenza", "Pertussis", "Salmonella", "STI"),
    1000,
    replace = TRUE
  ),
  report_date = sample(
    seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
    1000,
    replace = TRUE
  ),
  age = sample(0:95, 1000, replace = TRUE),
  sex = sample(c("Female", "Male", "Unknown"), 1000, replace = TRUE),
  GEOID = sample(brazos_tracts$GEOID, 1000, replace = TRUE)
)

# Write Re-Useable Permanent Dataset
write_csv(fake_cases, "data/fake_cases.csv")
