#Title: Problem Set 8
#Author: Kerri Rose Riley
#Purpose: To use multiple Policy Agendas project datasets to visualize how attention has varied across major policy/topic areas from 1950 (or prior) to the present.
#Requires: 
#Output: Figures and Quarto Document typeset as a .pdf

# Import Packages
library(tidyverse)
library(haven)

# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 8")

# Load in the 2 datasets (Executive Memos and Public Law Titles)
exec <- read.csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Executive_Orders.csv")
public <- read.csv("~/Desktop/Current Classes/R/Problem Set 8/data/US_Public_Laws.csv")

# Select subset of rows we need for analysis 
# Select subset of rows we need for analysis 
exec_subset <- exec |> select(year, majortopic, subtopic)
public_subset <- public |> select(year, majortopic, subtopic)

# Graph

