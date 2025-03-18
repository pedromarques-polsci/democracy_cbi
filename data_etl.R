# Packages -----------------------------------------------------------
library(countrycode)
#library(gridExtra)
library(haven)
#library(Hmisc)
library(janitor)
#library(readxl)
#library(rvest)
library(tidyverse)
#library(wbstats)


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