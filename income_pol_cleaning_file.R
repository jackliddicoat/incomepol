library(patchwork)
library(tidyverse)
library(dplyr)
library(jtools)
library(huxtable)
library(rvest)

url <- "https://en.wikipedia.org/wiki/Household_income_in_the_United_States"
url_reader <- url %>% 
  read_html()
tables <- url_reader %>% 
  html_elements("table") %>% 
  html_table()
tables %>% 
  View()

med_income_135 <- tables[[12]]
med_income_135 <- med_income_135[-c(1),]
med_income_125 <- tables[[13]]
med_income_125 <- med_income_125[-c(1),]
med_income_115 <- tables[[14]]
med_income_115 <- med_income_115[-c(1),]
med_income_105 <- tables[[15]]
med_income_105 <- med_income_105[-c(1),]
med_income_95 <- tables[[16]]
med_income_95 <- med_income_95[-c(1),]
med_income_85 <- tables[[17]]
med_income_85 <- med_income_85[-c(1),]
med_income_75 <- tables[[18]]
med_income_75 <- med_income_75[-c(1),]

df <- bind_rows(med_income_135, med_income_125, med_income_115, med_income_105,
                med_income_95, med_income_85, med_income_75)


income_pol <- df %>% 
  select(-Category) %>% 
  rename(county = `County or Equivalent`,
         state = State,
         mhshld_inc =  `Median Household Income`,
         election_00 = `Presidential Election Result...7`,
         election_04 = `Presidential Election Result...8`,
         election_08 = `Presidential Election Result...9`,
         election_12 = `Presidential Election Result...10`,
         election_16 = `Presidential Election Result...11`,
         election_20 = `Presidential Election Result...12`) %>% 
  mutate(mhshld_inc = gsub("\\$", "", mhshld_inc),
         mhshld_inc = gsub(",", "", mhshld_inc),
         mhshld_inc = as.numeric(mhshld_inc)) %>% 
  mutate(mhshld_inc = mhshld_inc/10000)