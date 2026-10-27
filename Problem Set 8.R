#Title: Problem Set 8
#Author: Kerri Rose Riley
#Purpose: To use multiple Policy Agendas project data sets to visualize how attention has varied across major policy/topic areas from 1950 (or prior) to the present.
#Requires: The Public Law Titles data set and the Executive Orders data set from the Competitive Agendas Project
#Output: Figures and Quarto Document typeset as a .pdf

# Import Packages
library(tidyverse)

# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 8")

# Load in the 2 data sets (Executive Memos and Public Law Titles)
exec <- read_csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Executive_Orders.csv")
public <- read_csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Public_Laws.csv")

# Select subset of rows we need for analysis and save in new DataFrames
exec_subset <- exec |> select(year, majortopic, subtopic)
public_subset <- public |> select(year, majortopic, subtopic)

# Aggregate annual counts for Topics 10 and 16
# Create new df 'fig1_annual' and then pass exec_subset into next transformation
fig1_annual <- exec_subset |> 
  # Filter rows for majortopic is 10 or 16 
  filter(majortopic %in% c(10, 16)) |> 
  # Drops columns with missing values for majortopic
  drop_na(year, majortopic) |> 
  # convert majortopic into a categorical factor
  mutate(majortopic = factor(majortopic, 
                             levels = c(16, 10),
                             # Renames labels
                             labels = c("Defense (Topic 16)", "Transportation (Topic 10)"))) |> 
  # Groups by year and majortopic and counts the number of rows for each combination
  count(year, majortopic)

# Figure 1: Trends in Presidential Executive Orders: National Defense (Topic 16) vs. Transportation (Topic 10), 1945–2025
# aes is the aesthetic layer
# Initializes the plot1 using the fig1_annual data set and maps year to the x-axis and count(n) to the y-axis
plot1 <- ggplot(fig1_annual, aes(x = year, y = n, color = majortopic, group = majortopic)) +
  # Connect points with line
  geom_line(linewidth = 1) +
  # Add individual points at each year with partial transparency
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Defense (Topic 16)" = "#1f77b4", 
                                "Transportation (Topic 10)" = "#ff7f0e")) +
  # Sets axis ticks/breaks
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  # Styling: Sets main plot title, subtitle, x, and y-axis labels
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
    plot.title = element_text(face = "bold", size = 11)
  )


# Aggregate annual counts for Maritime (Subtopic 1007) vs. Other Transportation (Topic 10)
fig2_annual <- exec |> 
  filter(majortopic == 10) |> 
  drop_na(year) |> 
  mutate(category = ifelse(subtopic == 1007, 
                           "Maritime Transportation (Subtopic 1007)", 
                           "Other Transportation Directives (Topic 10)")) |> 
  count(year, category) |> 
  complete(year = 1945:2025, category, fill = list(n = 0))

# Figure 2: Executive Priorities in Transportation: Maritime Policy vs. General Transit, 1940s–2025
plot2 <- ggplot(fig2_annual, aes(x = year, y = n, color = category, group = category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Maritime Transportation (Subtopic 1007)" = "#1f77b4", 
                                "Other Transportation Directives (Topic 10)" = "#ff7f0e")) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  labs(
    title = "Executive Priorities in Transportation: Maritime Policy vs. General Transit, 1945–2025",
    subtitle = "Annual count of executive directives (Comparative Agendas Project Topic 10)",
    x = "Year",
    y = "Number of Executive Orders",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 11)
  )


# Aggregate annual counts for subtopics 1604 and 1610
fig3_annual <- exec_subset |> 
  # Filter for subtopic
  filter(subtopic %in% c(1604, 1610)) |> 
  # Drop records with missing values for subtopic
  drop_na(year, subtopic) |> 
  # convert subtopic into a categorical factor
  mutate(subtopic = factor(subtopic, 
                           # Sets legend ordering
                           levels = c(1604, 1610), 
                           labels = c("Military Readiness (1604)", "Naval & Weapons Procurement (1610)"))) |> 
  # Groups by year and subtopic and counts the number of rows for each combination
  count(year, subtopic)

# Figure 3: Executive Priorities in Defense: Military Readiness & Strategic Mobilization (Subtopic 1604) vs. Weapons & Naval Procurement (Subtopic 1610), 1945–2025
plot3 = ggplot(fig3_annual, aes(x = year, y = n, color = subtopic, group = subtopic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Military Readiness (1604)" = "#1f77b4", 
                                "Naval & Weapons Procurement (1610)" = "#ff7f0e")) +
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


# Aggregate annual counts for Subtopic 1610 across both data sets
eo_1610 <- exec_subset |> 
  # Filter for subtopic 1610
  filter(subtopic == 1610) |> 
  # Calculate total counts under column name "Executive Orders"
  count(year, name = "Executive Orders")

pl_1610 <- public_subset |> 
  # Filter for subtopic 1610
  filter(subtopic == 1610) |> 
  # Calculate total counts under column name "Public Laws"
  count(year, name = "Public Laws")

# Merge into df 'fig4_annual' by joining using year column
fig4_annual <- full_join(eo_1610, pl_1610, by = "year") |> 
  # Ensure every year from 1945 to 2025 has a row in the data set
  complete(year = 1945:2025) |> 
  # Fill  missing values (NAs created by full_join/complete) with 0
  replace_na(list(`Executive Orders` = 0, `Public Laws` = 0)) |> 
  # Reshape data from wide to long format so "institution" maps to legend/colors in ggplot
  pivot_longer(
    # columns to collapse
    cols = c("Executive Orders", "Public Laws"),
    # New column storing category labels
    names_to = "institution",
    # New column storing the aggregated counts (n)
    values_to = "n"
  )

# Figure 4: Institutional Division of Labor in Defense Procurement: Presidential Orders vs. Enacted Legislation (Subtopic 1610), 1940s–2020s
plot4 = ggplot(fig4_annual, aes(x = year, y = n, color = institution, group = institution)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Executive Orders" = "#1f77b4", 
                                "Public Laws" = "#ff7f0e")) +
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

# Aggregate subtopic 1604
# Create new df 'eo_1604
eo_1604 <- exec_subset |> 
  # Filter for subtopic 1604
  filter(subtopic == 1604) |> 
  # Calculate total counts under column name "Executive Order" grouped by year
  count(year, name = "Executive Orders")

# Aggregate subtopic 1604
pl_1604 <- public_subset |> 
  # Filter for subtopic = 1604
  filter(subtopic == 1604) |> 
  # Calculate total counts under column name "Public Laws" grouped by year
  count(year, name = "Public Laws")

# Merge or join both counts into a data set by Year
fig5_annual <- full_join(eo_1604, pl_1604, by = "year") |> 
  # Expand time series so every year from 1945 to 2025 is explicitly represented
  complete(year = 1945:2025) |> 
  # Fill  missing values (NAs created by full_join/complete) with 0
  replace_na(list(`Executive Orders` = 0, `Public Laws` = 0)) |> 
  # Reshape data from wide to long format so "institution" maps to legend/colors in ggplot
  pivot_longer(
    cols = c("Executive Orders", "Public Laws"),
    names_to = "institution",
    values_to = "n"
  )

plot5 = ggplot(fig5_annual, aes(x = year, y = n, color = institution, group = institution)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Executive Orders" = "#1f77b4", 
                                "Public Laws" = "#ff7f0e")) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  labs(
    title = "Institutional Division of Labor in Military Readiness, 1945–2025",
    subtitle = "Comparing Executive Orders vs. Passed Legislation (Subtopic 1604)",
    x = "Year",
    y = "Total Policy Outputs (Subtopic 1604)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11)
        )

# Save the figures
ggsave("figures/figure1.png", plot = plot1, width = 8, height = 5, dpi = 300)
ggsave("figures/figure2.png", plot = plot2, width = 8, height = 5, dpi = 300)
ggsave("figures/figure3.png", plot = plot3, width = 8, height = 5, dpi = 300)
ggsave("figures/figure4.png", plot = plot4, width = 8, height = 5, dpi = 300)
ggsave("figures/figure5.png", plot = plot5, width = 8, height = 5, dpi = 300)

