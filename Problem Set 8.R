#Title: Problem Set 8
#Author: Kerri Rose Riley
#Purpose: To use multiple Policy Agendas project datasets to visualize how attention has varied across major policy/topic areas from 1950 (or prior) to the present.
#Requires: 
#Output: Figures and Quarto Document typeset as a .pdf

# Import Packages
library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 8")

# Load in the 2 data sets (Executive Memos and Public Law Titles)
exec <- read.csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Executive_Orders.csv")
public <- read.csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Public_Laws.csv")

# Select subset of rows we need for analysis 
# Select subset of rows we need for analysis 
exec_subset <- exec |> select(year, majortopic, subtopic)
public_subset <- public |> select(year, majortopic, subtopic)

# Graphing

# Aggregate annual counts for Topics 10 and 16
fig1_annual <- exec_subset |> 
  filter(year >= 1945, majortopic %in% c(10, 16)) |> 
  mutate(majortopic = factor(majortopic, 
                             levels = c(16, 10), 
                             labels = c("Defense (Topic 16)", "Transportation (Topic 10)"))) |> 
  count(year, majortopic)

# Figure 1: 
ggplot(fig1_annual, aes(x = year, y = n, color = majortopic, group = majortopic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Defense (Topic 16)" = "#1f77b4", 
                                "Transportation (Topic 10)" = "#ff7f0e")) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  labs(
    title = "Trends in Presidential Executive Orders: National Defense vs. Transportation, 1945–2025",
    subtitle = "Annual count of executive directives (Comparative Agendas Project)",
    x = "Year",
    y = "Number of Executive Orders",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12)
  )

library(ggplot2)
library(dplyr)

# Aggregate annual counts for Subtopics 1604 and 1610
fig2_annual <- exec_subset |> 
  filter(year >= 1945, subtopic %in% c(1604, 1610)) |> 
  mutate(subtopic = factor(subtopic, 
                           levels = c(1604, 1610), 
                           labels = c("Military Readiness (1604)", "Naval & Weapons Procurement (1610)"))) |> 
  count(year, subtopic)

# Figure 2: 
ggplot(fig2_annual, aes(x = year, y = n, color = subtopic, group = subtopic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Military Readiness (1604)" = "#2ca02c", 
                                "Naval & Weapons Procurement (1610)" = "#d62728")) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  labs(
    title = "Executive Priorities in Defense: Military Readiness vs. Procurement, 1945–2025",
    subtitle = "Annual count of executive directives (Comparative Agendas Project)",
    x = "Year",
    y = "Number of Executive Orders",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 11)
  )


# Aggregate annual counts for Subtopic 1610 across both datasets
eo_1610 <- exec_subset |> 
  filter(year >= 1945, subtopic == 1610) |> 
  count(year, name = "Executive Orders")

pl_1610 <- public_subset |> 
  filter(year >= 1945, subtopic == 1610) |> 
  count(year, name = "Public Laws")

# Join or merge datasets into a unified long-format DataFrame
fig3_annual <- full_join(eo_1610, pl_1610, by = "year") |> 
  replace_na(list(`Executive Orders` = 0, `Public Laws` = 0)) |> 
  pivot_longer(
    cols = c("Executive Orders", "Public Laws"),
    names_to = "institution",
    values_to = "n"
  )

# Figure 3: 
ggplot(fig3_annual, aes(x = year, y = n, color = institution, group = institution)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Executive Orders" = "#d62728", 
                                "Public Laws" = "#1f77b4")) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  labs(
    title = "Institutional Division of Labor in Defense Procurement, 1945–2025",
    subtitle = "Comparing Executive Directives vs. Passed Legislation (Subtopic 1610)",
    x = "Year",
    y = "Total Policy Outputs (Subtopic 1610)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 11)
  )




