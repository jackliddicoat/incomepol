library(rvest)
library(usmap)
library(viridis)

# there is a wikipedia page of fips codes by county that we can read in
url <- "https://en.wikipedia.org/wiki/List_of_United_States_FIPS_codes_by_county"
fips <- url %>% 
  read_html()
fips <- fips %>% 
  html_elements("table") %>% 
  html_table()
fips <- fips[[2]] # we want to grab the second table

# this gives us the fips codes
head(fips)
# here is our other dataframe
head(income_pol)

# lets change the column to have the [Name] County, [State] format 
income_pol <- income_pol %>% 
  mutate(county = paste(county, "County ,", state))

fips <- fips %>% 
  mutate(county = paste(`County or equivalent`,",", `State or equivalent`))

# join the dataframes on the county variable
df <- right_join(income_pol, fips, by = 'county')
df <- df %>% 
  select(county, mhshld_inc, FIPS) %>% 
  mutate(mhshld_inc = mhshld_inc*10000) %>% 
  rename(fips = FIPS)

plot_usmap(regions = "counties", values = "mhshld_inc",
           data = df, color = "lightgrey", linewidth = 0) +
  scale_fill_continuous(low = "white", high = "darkred",
                        label = scales::dollar_format(),
                        name = "Median Household Income") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold")) +
  labs(title = "Counties in the US with a median household income greater than $75,000")

