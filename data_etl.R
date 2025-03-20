# Packages -----------------------------------------------------------
library(countrycode)
library(devtools)
library(haven)
library(janitor)
library(readxl)
library(tidyverse)
library(vdemdata)

# vdemdata package installation:
# devtools::install_github("vdeminstitute/vdemdata")


# Temporary (might be useful)
# library(gridExtra)
# library(Hmisc)
# library(readxl)
# library(rvest)
# library(wbstats)

# 1. OUTCOME -----------------------------------------------------------------
cbi_garriga <- read_dta("raw_data/cbi_2025_garriga.dta")

# Making missings explicit
cbi_garriga_fill <- cbi_garriga %>% 
  complete(cowcode = cowcode, year = 1970:2023) %>% 
  arrange(cowcode, year) %>% 
  group_by(cowcode) %>% 
  fill(cname, cowcode, ccodewb, .direction = "downup") %>% 
  ungroup() %>% 
  arrange(cowcode, year)

# Checking primary key
cbi_garriga_fill %>% 
  count(year, cowcode) %>% filter(n > 1)

# Counting missings per country
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
vdem_dataset_adj %>% 
  count(year, co_wcode) %>% filter(n > 1)

# Checking countries with missing cowcode
vdem_dataset %>% filter(is.na(co_wcode)) %>% distinct(country_name)

# 3. COVARIATES --------------------------------------------------------------

# Exchange Regime ---------------------------------------------------------


