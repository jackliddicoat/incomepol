# running the cleaning file before running this one
library(marginaleffects)
library(mgcv)
library(patchwork)

# view the data
income_pol %>% glimpse()

# we just want the 2020 election
election_20 <- income_pol %>% 
  select(county, state, mhshld_inc, election_20) %>% 
  rename(winner = election_20) %>% 
  mutate(winner = ifelse(winner == "Trump", 1, 0))

# the places where trump or biden won
election_20$winner

# view our data
election_20 %>% head()

# the first regression is just going to be an ordinary logistic regression
glm1 <- glm(winner ~ mhshld_inc, data = election_20, family = binomial)

# for binning lets look at the summary stats for mhshld_inc
election_20$mhshld_inc %>% summary()

# the second regression is going to be binned
glm2 <- glm(winner ~ I(mhshld_inc <= 7.892) +
              I(mhshld_inc > 7.892 & mhshld_inc <= 8.627) +
              I(mhshld_inc > 8.627 & mhshld_inc <= 8.994) +
              I(mhshld_inc > 8.8994), data = election_20, family = binomial)

# the third regression is going to be a spline so we use gam()
glm3 <- gam(winner ~ s(mhshld_inc), data = election_20, family = binomial)

# make the plots
linear <- plot_predictions(glm1, condition = "mhshld_inc") + ggtitle("Linear")
discrete_linear <- plot_predictions(glm2, condition = "mhshld_inc") + ggtitle("Discrete")
spline <- plot_predictions(glm3, condition = "mhshld_inc") + ggtitle("Spline")

# display the plots (using patchwork)
(linear + discrete_linear) / spline

# we can see the effects using avg_comparisons()
avg_comparisons(glm1)
avg_comparisons(glm2)
avg_comparisons(glm3)

# we can see the AICs of each model
glm1$aic
glm2$aic
glm3$aic

# we can plot these if we wish
ests <- data.frame(estimates = c(avg_comparisons(glm1)$estimate, 
                                 avg_comparisons(glm2)$estimate,
                                 avg_comparisons(glm3)$estimate),
                   conf_low = c(avg_comparisons(glm1)$conf.low, 
                                avg_comparisons(glm2)$conf.low,
                                avg_comparisons(glm3)$conf.low),
                   conf_high = c(avg_comparisons(glm1)$conf.high, 
                                 avg_comparisons(glm2)$conf.high,
                                 avg_comparisons(glm3)$conf.high),
                   model = c("Linear", "Discrete", "Spline"))

ests %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(model, estimates), width = .4,
           fill = 'lightgreen') +
  geom_errorbar(aes(x = model, y = estimates, ymin = conf_high, ymax = conf_low), width = .2) +
  ylim(-.15, .15) +
  ggtitle("Average Effects of by Model") 

