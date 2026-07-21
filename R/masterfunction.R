library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)

build_disease_report <- function(data, disease_name) {
  
  disease_data <- data |>
    filter(disease == disease_name)
  
  if (nrow(disease_data) == 0) {
    stop(paste("No records found for:", disease_name))
  }
  
  # Create standardized variables
  disease_data <- disease_data |>
    mutate(
      report_date = as.Date(report_date),
      
      age_group = cut(
        age,
        breaks = c(-Inf, 4, 17, 24, 44, 64, Inf),
        labels = c(
          "0–4",
          "5–17",
          "18–24",
          "25–44",
          "45–64",
          "65+"
        )
      ),
      
      report_month = floor_date(report_date, unit = "month"),
      
      report_week = floor_date(
        report_date,
        unit = "week",
        week_start = 1
      )
    )
  
  # Summary counts
  total_cases <- nrow(disease_data)
  
  age_summary <- disease_data |>
    count(age_group, name = "cases", .drop = FALSE)
  
  sex_summary <- disease_data |>
    count(sex, name = "cases") |>
    mutate(percent = cases / sum(cases) * 100)
  
#  race_ethnicity_summary <- disease_data |>
 #   count(race_ethnicity, name = "cases") |>
  #  mutate(percent = cases / sum(cases) * 100)
  
  weekly_summary <- disease_data |>
    filter(!is.na(report_week)) |>
    count(report_week, name = "cases") |>
    tidyr::complete(
      report_week = seq(
        min(report_week),
        max(report_week),
        by = "week"
      ),
      fill = list(cases = 0)
    )
  
  monthly_summary <- disease_data |>
    filter(!is.na(report_month)) |>
    count(report_month, name = "cases") |>
    tidyr::complete(
      report_month = seq(
        min(report_month),
        max(report_month),
        by = "month"
      ),
      fill = list(cases = 0)
    )
  
  # Plots
  age_plot <- ggplot(
    age_summary,
    aes(x = age_group, y = cases)
  ) +
    geom_col() +
    labs(
      title = paste("Cases by Age Group:", disease_name),
      x = "Age group",
      y = "Cases"
    ) +
    theme_minimal()
  
  sex_plot <- ggplot(
    sex_summary,
    aes(x = fct_reorder(sex, cases), y = cases)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Cases by Sex:", disease_name),
      x = NULL,
      y = "Cases"
    ) +
    theme_minimal()
  
#  race_ethnicity_plot <- ggplot(
 #   race_ethnicity_summary,
  #  aes(
   #   x = fct_reorder(race_ethnicity, cases),
    #  y = cases
#    )
 # ) +
  #  geom_col() +
   # coord_flip() +
    #labs(
     # title = paste(
      #  "Cases by Race and Ethnicity:",
       # disease_name
  #    ),
   #   x = NULL,
    #  y = "Cases"
#    ) +
#    theme_minimal()
  
  weekly_plot <- ggplot(
    weekly_summary,
    aes(x = report_week, y = cases)
  ) +
    geom_col() +
    labs(
      title = paste(
        "Weekly Cases by Date of report:",
        disease_name
      ),
      x = "Week of report",
      y = "Cases"
    ) +
    theme_minimal()
  
  monthly_plot <- ggplot(
    monthly_summary,
    aes(x = report_month, y = cases)
  ) +
    geom_line() +
    geom_point() +
    labs(
      title = paste(
        "Monthly Cases by Date of report:",
        disease_name
      ),
      x = "Month of report",
      y = "Cases"
    ) +
    theme_minimal()
  
  # Return everything as a named list
  list(
    disease_name = disease_name,
    data = disease_data,
    total_cases = total_cases,
    age_summary = age_summary,
    sex_summary = sex_summary,
 #   race_ethnicity_summary = race_ethnicity_summary,
    weekly_summary = weekly_summary,
    monthly_summary = monthly_summary,
    age_plot = age_plot,
    sex_plot = sex_plot,
  #  race_ethnicity_plot = race_ethnicity_plot,
    weekly_plot = weekly_plot,
    monthly_plot = monthly_plot
  )
}
