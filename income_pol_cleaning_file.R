# load in the libraries. Install these before running this script.
library(patchwork)
library(tidyverse)
library(dplyr)
library(jtools)
library(huxtable)
library(rvest)

# read the data from this wikipedia page with info about median household income in the US
url <- "https://en.wikipedia.org/wiki/Household_income_in_the_United_States"
url_reader <- url %>% 
  read_html()
tables <- url_reader %>% 
  html_elements("table") %>% 
  html_table()
tables %>% 
  View()

# Collect the tables that we care about
# We need to remove the first row in every table because it is going to mess up the analysis if we do not.
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

# using bind_rows() from dplyr, essentially add all of these data tables together
df <- bind_rows(med_income_135, med_income_125, med_income_115, med_income_105,
                med_income_95, med_income_85, med_income_75)

# Finally, rename cols with unhelpful names and change make sure the vectors are of the class that you want them to be
# We can also rename the cols by first making a character list of different strings with the names we want (call it x = list("col1"..."coln"))
# Then do
# colnames(df) <- x
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
  mutate(mhshld_inc = mhshld_inc/10000) # this is going to show really small odds ratios if we don't change this variable
