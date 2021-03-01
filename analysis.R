# load tidyverse
library('dplyr')
library('tidyr')
library('readr')

# Set working director
setwd('~/Documents/data_projects/sc2_pillar/')

# load data file
data = read_csv('data/updated_maps_data.csv')

# Filter only Zerg matchups
z_matchups = data %>%
  filter(Matchup == 'TvZ' | Matchup == 'PvZ') %>%
  select(Matchup, patch, Wins, Losses, pillar_label) %>%
  # add pillar mark
  # combines no pillar and pillar that can't see nat entrance
  mutate(pillar = if_else(pillar_label == 'Pillar can see natural entrance', 1, 0)) %>%
  # calculate total games
  mutate(games = Wins + Losses) %>%
  filter(games > 50) %>%
  # calculate new win and loss rates
  mutate(win_rate = round((Losses / games), 5)) %>%
  mutate(loss_rate = round((Wins / games), 5))

zvp = z_matchups %>%
  filter(Matchup == 'PvZ')

sum(zvp$Losses) / sum(zvp$games)

zvt = z_matchups %>%
  filter(Matchup == 'TvZ')

# subset pillars
z_nopillars = subset(z_matchups, pillar == 0)
z_pillars = subset(z_matchups, pillar == 1)

sum(z_nopillars$Wins)
sum(z_nopillars$Losses)

sum(z_pillars$Wins)
sum(z_pillars$Losses)

sum(z_nopillars$Losses) / sum(z_nopillars$games)
sum(z_pillars$Losses) / sum(z_pillars$games)

sum(z_nopillars$games)
sum(z_pillars$games)

# calculate pillar win rates
z_wr = sum(z_pillars$Losses) / sum(z_pillars$games)

# Print descriptive statistics
summary(z_matchups)

# Run ANOVA on pillar vs no pillar
z.aov = aov(pillar ~ win_rate + patch, data = z_matchups)
summary(z.aov)

testaov = aov(win_rate ~ pillar + patch, data = z_matchups)
summary(testaov)
#plot(testaov)

# Run one tailed t-test around mean win rate
z.ttest = t.test(z_nopillars$win_rate, mu = z_wr)
z.ttest

# ZvP anova and ttest
zvp.aov = aov(pillar + patch ~ win_rate, data = zvp)
summary(zvp.aov)

zvp.test = t.test(zvp$win_rate, mu = 0.5177)
zvp.test

# ZvT anova and ttest
zvt.aov = aov(pillar + patch ~ win_rate, data = zvt)
summary(zvt.aov)

zvt.test = t.test(zvt$win_rate, mu = 0.4935)
zvt.test
