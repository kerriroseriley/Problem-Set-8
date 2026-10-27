

# Import Packages
library(tidyverse)

# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 8")

# Load in the 2 data sets (Executive Memos and Public Law Titles)
exec <- read_csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Executive_Orders.csv")
public <- read_csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Public_Laws.csv")


exec_titles <- exec |>
  filter(
    year > 2020,
    subtopic %in% c(1604, 1610, 1007)
  ) |>
  select(year, subtopic, eo_number, description) |>
  drop_na(year, subtopic, eo_number, description) |>
  arrange(year, subtopic, eo_number, description)

exec_titles

public_titles <- public |>
  filter(
    year > 2020,
    subtopic %in% c(1604, 1610, 1007)
  ) |>
  select(year, subtopic, public_law_no, law_description) |>
  drop_na(year, subtopic, public_law_no, law_description) |>
  arrange(year, subtopic, public_law_no, law_description)

public_titles
