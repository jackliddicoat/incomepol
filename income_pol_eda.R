library(fastDummies)
library(sjPlot)

glm1 <- income_pol %>% 
  mutate(election_20 = ifelse(election_20 == "Trump", 1, 0)) %>% 
  glm(election_20 ~ mhshld_inc, data = ., family = "binomial")
glm2 <- income_pol %>% 
  mutate(election_16 = ifelse(election_16 == "Trump", 1, 0)) %>% 
  glm(election_16 ~ mhshld_inc, data = ., family = "binomial")
glm3 <- income_pol %>% 
  mutate(election_12 = ifelse(election_12 == "Romney", 1, 0)) %>% 
  glm(election_12 ~ mhshld_inc, data = ., family = "binomial")
glm4 <- income_pol %>% 
  mutate(election_08 = ifelse(election_08 == "McCain", 1, 0)) %>% 
  glm(election_08 ~ mhshld_inc, data = ., family = "binomial")
glm5 <- income_pol %>% 
  mutate(election_04 = ifelse(election_04 == "Bush", 1, 0)) %>% 
  glm(election_04 ~ mhshld_inc, data = ., family = "binomial")
glm6 <- income_pol %>% 
  mutate(election_00 = ifelse(election_00 == "Bush", 1, 0)) %>% 
  glm(election_00 ~ mhshld_inc, data = ., family = "binomial")

p_combined <- (plot_model(glm1, show.values = T, axis.lim = c(.5, 1.5)) + plot_model(glm2, show.values = T,
                                                                                     axis.lim = c(.5, 1.5))) / 
  (plot_model(glm3, axis.lim = c(.5, 1.5), show.values = T) + plot_model(glm4, axis.lim = c(.5, 1.5), show.values = T)) / 
  (plot_model(glm5, axis.lim = c(.5, 1.5), show.values = T) + plot_model(glm6, axis.lim = c(.5, 1.5), show.values = T))

p_combined +
  plot_annotation('Odds Ratios of a County Voting Republican \n Given a $10,000 increase in 2020 Median Household Income', caption = 'Data from all counties with a median household income >$75,000\nSignificance levels: * = .1, ** = .05, *** = .01\n(N = 358)',theme=theme(plot.title=element_text(hjust=0.5, size = 12)))

# most counties were not majority minority status
income_pol %>% 
  count(`Majority Minority Status`)

# lets see how that affects election outcomes
glm6 <- income_pol %>% 
  mutate(trump_vote_20 = ifelse(election_20=="Trump", 1, 0),
         majority_minority = ifelse(`Majority Minority Status`=="YES", 1, 0)) %>% 
  glm(trump_vote_20 ~ majority_minority, data = ., family = "binomial")

glm7 <- income_pol %>% 
  mutate(trump_vote_16 = ifelse(election_16=="Trump", 1, 0),
         majority_minority = ifelse(`Majority Minority Status`=="YES", 1, 0)) %>% 
  glm(trump_vote_16 ~ majority_minority, data = ., family = "binomial")

glm8 <- income_pol %>% 
  mutate(romney_vote_12 = ifelse(election_12=="Romney", 1, 0),
         majority_minority = ifelse(`Majority Minority Status`=="YES", 1, 0)) %>% 
  glm(romney_vote_12 ~ majority_minority, data = ., family = "binomial")

glm9 <- income_pol %>% 
  mutate(mccain_vote_08 = ifelse(election_08=="McCain", 1, 0),
         majority_minority = ifelse(`Majority Minority Status`=="YES", 1, 0)) %>% 
  glm(mccain_vote_08 ~ majority_minority, data = ., family = "binomial")

glm10 <- income_pol %>% 
  mutate(bush_vote_04 = ifelse(election_04=="Bush", 1, 0),
         majority_minority = ifelse(`Majority Minority Status`=="YES", 1, 0)) %>% 
  glm(bush_vote_04 ~ majority_minority, data = ., family = "binomial")

glm11 <- income_pol %>% 
  mutate(bush_vote_00 = ifelse(election_00=="Bush", 1, 0),
         majority_minority = ifelse(`Majority Minority Status`=="YES", 1, 0)) %>% 
  glm(bush_vote_00 ~ majority_minority, data = ., family = "binomial")

# use the tab_model function to get a nice regression table
tab_model(glm6, glm7, glm8, glm9, glm10, glm11,
          title = "Odds County Voted for Republican by Election and Minority Status")



