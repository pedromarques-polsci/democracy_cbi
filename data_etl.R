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
cbi_garriga_adj <- cbi_garriga %>% 
  complete(cowcode = cowcode, year = 1970:2023) %>% 
  arrange(cowcode, year) %>% 
  group_by(cowcode) %>% 
  fill(cname, ccodewb, .direction = "downup") %>% 
  ungroup() %>% 
  arrange(cowcode, year)

# Checking primary key
cbi_garriga_adj %>% 
  count(year, cowcode) %>% filter(n > 1)

# Counting explicit missings per country
cbi_missings <- cbi_garriga_adj %>% 
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
  mutate(date = ym(month),
         month = month(date),
         year = year(date),
         fine_era = as.numeric(fine_era)) %>% 
  arrange(country_name, date) %>% 
  select(country_name, date, year, month, fine_era)

# CoW coding
era_class_adj <- era_class %>% 
  mutate(
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
  left_join(cbi_garriga_adj %>% select(cname, cowcode) %>% 
              distinct(cname, cowcode), 
            join_by(country_name == cname)) %>% 
  mutate(cowcode = ifelse(is.na(cowcode_temp), cowcode, cowcode_temp)) %>% 
  select(-cowcode_temp)

era_class_adj %>% filter(is.na(cowcode)) %>% distinct(country_name)

era_class_final <- era_class_adj %>% 
  select(-date) %>% 
  pivot_wider(names_from = month,
              values_from = fine_era, names_prefix = "m") %>% 
  mutate(flex = ifelse(m12 > m1, 1, 0),
         unflex = ifelse(m12 < m1, 1, 0))

## 3.2 Redistributive data ----------------------------------------------------
load("raw_data/swiid9_7.RDA")

# CoW coding
gini <- swiid_summary %>% 
  select(country, year, gini_disp, gini_mkt) %>% 
  mutate(cowcode_temp = countrycode(country, origin = 'country.name', 
                       destination = 'cown')) %>% 
  left_join(cbi_garriga_adj %>% select(cname, cowcode) %>% 
              distinct(cname, cowcode), 
            join_by(country == cname)) %>% 
  mutate(cowcode = ifelse(is.na(cowcode_temp), cowcode, cowcode_temp)) %>% 
  select(-cowcode_temp)

gini %>% filter(is.na(cowcode)) %>% distinct(country)

## 3.3 Populism -------------------------------------------------------------
populism <- read_excel("raw_data/PLE_panel.xlsx") %>% 
  mutate(cowcode = countrycode(iso, origin = 'iso3c', destination = 'cown'))

populism %>% filter(is.na(cowcode)) %>% distinct(country)

## 3.4 Globalization ---------------------------------------------------------
ecopen_wb <- wb_data(indicator = "NE.TRD.GNFS.ZS") %>% 
  mutate(cowcode_temp = countrycode(iso3c, origin = 'iso3c', 
                               destination = 'cown'),
         country_name = 
           case_when(country == "West Bank and Gaza" ~ 
                       "Palestine/West Bank and Gaza",
                     country == "Cayman Islands" ~ 
                       "Cayman",
                     .default = country)) %>% 
  left_join(cbi_garriga_adj %>% select(cname, cowcode) %>% 
              distinct(cname, cowcode), 
            join_by(country == cname)) %>% 
  mutate(cowcode = ifelse(is.na(cowcode_temp), cowcode, cowcode_temp)) %>% 
  select(-cowcode_temp) %>% 
  rename(ecopen = NE.TRD.GNFS.ZS)

ecopen_wb %>% filter(is.na(cowcode)) %>% distinct(country) %>% print(n = 30)

## 3.4 World Economic Outlook (IMF) ----------------------------------------
# October 2024
weo <- read.csv2("raw_data/weo_data.csv", na = c("n/a", "", "--"), 
                 dec = ".") %>% 
  slice(1:2157) %>% 
  pivot_longer(cols = c(6:48), names_to = "year") %>% 
  mutate(year = substr(year, 2, 5),
         value = gsub(",", "", value),
         value = as.numeric(value)) %>% 
  clean_names() %>% 
  pivot_wider(id_cols = c("iso", "country", "year"), 
              names_from = c("subject_descriptor", "units"),
              values_from = "value") %>% 
  clean_names() %>% 
  filter(!is.na(country)) %>% 
  mutate(country = ifelse(country == "West Bank and Gaza", 
                          "Palestine/West Bank and Gaza", country),
         cowcode_temp = countrycode(iso, origin = 'iso3c', 
                                    destination = 'cown')) %>% 
  left_join(cbi_garriga_adj %>% select(cname, cowcode) %>% 
              distinct(cname, cowcode), 
            join_by(country == cname)) %>% 
  mutate(cowcode = ifelse(is.na(cowcode_temp), cowcode, cowcode_temp)) %>% 
  rename(gdp_current_dollar = gross_domestic_product_current_prices_u_s_dollars,
         real_gdp_pcp_ppp = gross_domestic_product_per_capita_constant_prices_purchasing_power_parity_2021_international_dollar,
         inf_avg_cpi = inflation_average_consumer_prices_index,
         inf_avg_pp = inflation_average_consumer_prices_percent_change,
         inf_eop_cpi = inflation_end_of_period_consumer_prices_index,
         inf_eop_pp = inflation_end_of_period_consumer_prices_percent_change,
         unemp = unemployment_rate_percent_of_total_labor_force,
         population = population_persons, # Millions
         ggov_revenue_gdp = general_government_revenue_percent_of_gdp,
         ggov_net_debt_gdp = general_government_net_debt_percent_of_gdp,
         acc_balance_gdp = current_account_balance_percent_of_gdp)
                          
weo %>% filter(is.na(cowcode)) %>% distinct(country)  
    
weo %>% 
  count(iso, year) %>% 
  filter(n > 1)

# 4. FINAL DATASET --------------------------------------------------------
final_dataset <- cbi_garriga_adj %>% 
  left_join(vdem_dataset %>% select(year:v2x_regime),
            join_by(year, cowcode == co_wcode)) %>% 
  left_join(era_class_final %>% select(year, cowcode, flex, unflex),
            join_by(year, cowcode)) %>% 
  left_join(gini %>% select(year:cowcode),
            join_by(year, cowcode)) %>% 
  left_join(populism %>% select(year:cowcode),
            join_by(year, cowcode)) %>% 
  left_join(ecopen_wb %>% select(date, ecopen, cowcode),
            join_by(year == date, cowcode)) %>% 
  left_join(weo %>% select(year:acc_balance_gdp, cowcode) %>% 
              mutate(year = as.numeric(year)),
            join_by(year, cowcode))

# EXPORTING FINAL DATASET -------------------------------------------------
write_csv(final_dataset, "processed_data/final_dataset.csv")
write_rds(final_dataset, "processed_data/final_dataset.rds")
write_dta(final_dataset, "processed_data/final_dataset.dta")