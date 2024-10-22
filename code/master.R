library(realtalk)
library(epiextractr)
library(tidyverse)
library(here)
library(epidatatools)
library(MetricsWeighted)
library(fixest)
library(bea.R)
library(blsR)
library(data.table)
library(formattable)
library(openxlsx)

# define wage years
current_year <- 2023

#define the varlist
var_list <- c("year", "month", "selfemp", "selfinc", "age", "wage", "female", "wbho", "educ",
              "emp", "division", "schenrl", "unmem", "union", "tc_weekpay", "lfstat",
              "ptecon", "discwork", "unemp", "unempdur", "lookdur", "nilf", "statefips", "mind03",
              "ind90", "ind80", "ind70", "mind16")


#### DATA IMPORTATION ####
# Load data for the "may" period (pre-1978)
may <- load_may(1973:1981, all_of(c(var_list, "finalwgt", "orgwgt"))) %>% mutate(sample = "may")

# Load Outgoing Rotation Group (ORG) data
org <- load_org(1982:current_year, all_of(c(var_list, "orgwgt"))) %>% mutate(sample = "org")

# standard restrictions
data <- bind_rows(org, may) %>% 
  filter(age>=16)

#### MASTER WAGE DATASET ####
# master wage dataset
union_master <- data %>%
  # flatten labels and adjust weight variables
  mutate(across(female | wbho | educ, ~ as.character(as_factor(.x))),
         # annual weight adjustment
         wgt = case_when(
           # may data wgt ~ finalwgt
           year == 1981 ~ orgwgt,           # Use orgwgt specifically in 1981
           sample == "may" ~ finalwgt,      # Use finalwgt if sample is "may"
           sample == "org" ~ orgwgt / 12 ),
         # helpful indicator to group by all simultaneously
         all = "all",
         # create age group category
         age_group = case_when(between(age, 16, 24) ~ "_a-16-24", between(age, 25, 54) ~ "_a-25-54",
                               between(age, 55, 64) ~ "_a-55-64", age >= 65 ~ "_a-65-over")) %>%
  # redefine demographic labels to fit WP format
  mutate(female = paste0("_g-", tolower(female)),
         wbho = case_when(
           wbho == "Hispanic" ~ "_r-hisp",
           TRUE ~ paste0("_r-", tolower(wbho))),
         educ = case_when(
           educ == "Less than high school" ~ "_e-lths",
           educ == "High school" ~ "_e-hs",
           educ == "Some college" ~ "_e-smcoll",
           educ == "College" ~ "_e-coll",
           educ == "Advanced" ~ "_e-adv")) %>%
  arrange(year)


## Comparison Tables 

#Union Levels 
source(here("code/density.R"), echo = TRUE)

# Unions Wage premiums 
source(here("code/wage_differentials.R"), echo = TRUE)

# State-level union density 
source(here("code/state_level.R"), echo = TRUE)