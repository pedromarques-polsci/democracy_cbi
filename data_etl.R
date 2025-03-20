# Packages -----------------------------------------------------------
library(countrycode)
library(devtools)
library(haven)
library(janitor)
library(readxl)
library(tidyverse)
library(vdemdata)
library(wbstats)

# vdemdata package installation:
# devtools::install_github("vdeminstitute/vdemdata")

# 1. OUTCOME -----------------------------------------------------------------
cbi_garriga <- read_dta("raw_data/cbi_2025_garriga.dta")

# Making missings explicit
cbi_garriga_fill <- cbi_garriga %>% 
  complete(cowcode = cowcode, year = 1970:2023) %>% 
  arrange(cowcode, year) %>% 
  group_by(cowcode) %>% 
  fill(cname, ccodewb, .direction = "downup") %>% 
  ungroup() %>% 
  arrange(cowcode, year)

# Checking primary key
cbi_garriga_fill %>% 
  count(year, cowcode) %>% filter(n > 1)

# Counting explicit missings per country
cbi_missings <- cbi_garriga_fill %>% 
  group_by(cowcode) %>% 
  summarise(cbi_missing = sum(is.na(lvau_garriga)),
            cname = first(cname)) %>% select(cname, everything())

# 2. TREATMENT ----------------------------------------------------------------
vdem_dataset <- vdemdata::vdem %>% 
  clean_names() %>% 
  select(country_name, country_text_id, country_id, year, co_wcode, 
         v2x_polyarchy, e_boix_regime, e_gdppc, v2x_feduni, v2x_regime)

# Checking primary key
vdem_dataset %>% 
  count(year, co_wcode) %>% filter(n > 1)

# Countries with missing CoW codes that are not present in CBI Dataset can be
# ignored since they will not be in the final dataset
vdem_dataset %>% filter(is.na(co_wcode)) %>% distinct(country_name)

# 3. COVARIATES --------------------------------------------------------------

## 3.1 Exchange Regime -------------------------------------------------------
raw_file <- read_excel("raw_data/ERA_Classification_Monthly_1940-2019.xlsx",
           sheet = 3, skip = 3)

# Concatanating country names
raw_file[1, ] <- raw_file %>%
  slice(1:2) %>%
  summarise(across(everything(), ~ paste(na.omit(.), collapse = " ")))

# Tidying up
era_class <- raw_file %>% 
  slice(-2,-3) %>% select(-1) %>%
  row_to_names(row_number = 1) %>% rename(month = Country) %>% 
  pivot_longer(!month, names_to = "country_name", values_to = "fine_era") %>% 
  mutate(month = ym(month)) %>% 
  arrange(country_name, month) %>% 
  select(country_name, month, fine_era)

# CoW coding
era_class_adj <- era_class %>% 
  mutate(year = year(month),
         
         # Disambiguation
         country_name = 
           case_when(country_name == "West Bank and Gaza" ~ 
                       "Palestine/West Bank and Gaza",
                     country_name == "Serbia, Rep. of" ~ 
                       "Serbia",
                     .default = country_name),
         
         # Matching country names to CoW Codes
         cowcode_temp = countrycode(country_name, origin = 'country.name', 
                               destination = 'cown')) %>% 
  
  # Getting additional CoW codes where countrycode package failed
  left_join(cbi_garriga_fill %>% select(cname, cowcode), 
            relationship = "many-to-many",
            join_by(country_name == cname)) %>% 
  mutate(cowcode = ifelse(is.na(cowcode_temp), cowcode, cowcode_temp)) %>% 
  select(-cowcode_temp)

era_class_adj %>% filter(is.na(cowcode)) %>% distinct(country_name)

## 3.2 Redistributive data ----------------------------------------------------
load("raw_data/swiid9_7.RDA")

# CoW coding
gini <- swiid_summary %>% 
  select(country, year, gini_disp, gini_mkt) %>% 
  mutate(cowcode_temp = countrycode(country, origin = 'country.name', 
                       destination = 'cown')) %>% 
  left_join(cbi_garriga_fill %>% select(cname, cowcode), 
            relationship = "many-to-many",
            join_by(country == cname)) %>% 
  mutate(cowcode = ifelse(is.na(cowcode_temp), cowcode, cowcode_temp)) %>% 
  select(-cowcode_temp)

gini %>% filter(is.na(cowcode)) %>% distinct(country)

## 3.3 Populism -------------------------------------------------------------
populism <- read_excel("raw_data/PLE_panel.xlsx") %>% 
  mutate(cowcode = countrycode(iso, origin = 'iso3c', destination = 'cown'))

populism %>% filter(is.na(cowcode)) %>% distinct(country)

## 3.4 Globalization ---------------------------------------------------------
ecopen_wb <- wb_data(indicator = "NE.IMP.GNFS.ZS") %>% 
  mutate(cowcode_temp = countrycode(iso3c, origin = 'iso3c', 
                               destination = 'cown'),
         country_name = 
           case_when(country == "West Bank and Gaza" ~ 
                       "Palestine/West Bank and Gaza",
                     country == "Cayman Islands" ~ 
                       "Cayman",
                     .default = country)) %>% 
  left_join(cbi_garriga_fill %>% select(cname, cowcode), 
            relationship = "many-to-many",
            join_by(country == cname)) %>% 
  mutate(cowcode = ifelse(is.na(cowcode_temp), cowcode, cowcode_temp)) %>% 
  select(-cowcode_temp) %>% 
  rename(ecopen = NE.IMP.GNFS.ZS)

ecopen_wb %>% filter(is.na(cowcode)) %>% distinct(country) %>% print(n = 30)

# 4. FINAL DATASET --------------------------------------------------------
