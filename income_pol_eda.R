library(fastDummies)
library(sjPlot)
library(hrbrthemes)
library(scales)
library(patchwork)

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

# we can also create models that looks at the probability that a county voted republican
# by its income level
income_seq = seq(from = min(income_pol$mhshld_inc),
                 to = max(income_pol$mhshld_inc),
                 length = 30)
p_trump_20 <- predict(glm1,
                      newdata = data.frame(mhshld_inc = income_seq),
                      type = "response")

p_trump_16 <- predict(glm2,
                      newdata = data.frame(mhshld_inc = income_seq),
                      type = "response")

p_romney_12 <- predict(glm3,
                      newdata = data.frame(mhshld_inc = income_seq),
                      type = "response")

p_mccain_08 <- predict(glm4,
                      newdata = data.frame(mhshld_inc = income_seq),
                      type = "response")

p_bush_04 <- predict(glm5,
                       newdata = data.frame(mhshld_inc = income_seq),
                       type = "response")

p_bush_00 <- predict(glm6,
                     newdata = data.frame(mhshld_inc = income_seq),
                     type = "response")

p1 <- ggplot() +
  geom_line(aes(x = income_seq*10000, y = p_trump_20)) +
  scale_x_continuous(labels = dollar_format(scale_cut = cut_short_scale())) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Median Household Income",
       y = "Probability", title = "2020")

p2 <- ggplot() +
  geom_line(aes(x = income_seq*10000, y = p_trump_16)) +
  scale_x_continuous(labels = dollar_format(scale_cut = cut_short_scale())) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Median Household Income",
       y = "Probability", title = "2016")

p3 <- ggplot() +
  geom_line(aes(x = income_seq*10000, y = p_romney_12)) +
  scale_x_continuous(labels = dollar_format(scale_cut = cut_short_scale())) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Median Household Income",
       y = "Probability", title = "2012")

p4 <- ggplot() +
  geom_line(aes(x = income_seq*10000, y = p_mccain_08)) +
  scale_x_continuous(labels = dollar_format(scale_cut = cut_short_scale())) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Median Household Income",
       y = "Probability", title = "2008")

p5 <- ggplot() +
  geom_line(aes(x = income_seq*10000, y = p_bush_04)) +
  scale_x_continuous(labels = dollar_format(scale_cut = cut_short_scale())) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Median Household Income",
       y = "Probability", title = "2004")

p6 <- ggplot() +
  geom_line(aes(x = income_seq*10000, y = p_bush_00)) +
  scale_x_continuous(labels = dollar_format(scale_cut = cut_short_scale())) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Median Household Income",
       y = "Probability", title = "2000")

(p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_annotation(title = "Probability a County Voted Republican in the ____ Election by Income Level")

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
  
  
  

