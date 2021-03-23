# load tidyverse
library('dplyr')
library('tidyr')
library('readr')
library('lubridate')
library('DataCombine')
library('effsize')

# Set working directory
# linux
#setwd('~/Documents/data_projects/sc2_pillar/')

# windows
setwd("C:\\Users\\bxiao\\Documents\\games_projects\\sc2_map_pillar_analysis")

# load data file
data = read_csv('data/updated_maps_data2.csv')

# Filter only Zerg matchups
z_matchups = data %>%
  filter(Matchup == 'TvZ' | Matchup == 'PvZ') %>%
  # add pillar mark
  # combines no pillar and pillar that can't see nat entrance
  mutate(pillar = if_else(pillar_label == 'Pillar can see natural entrance', 1, 0)) %>%
  mutate(matchup = if_else(Matchup == 'TvZ', 'ZvT', 'ZvP')) %>%
  mutate(patch_date = as.Date(patch_date, format = '%m-%d-%Y')) %>%
  # calculate total games
  mutate(games = Wins + Losses) %>%
  filter(games > 50) %>%
  # calculate new win and loss rates
  mutate(win_rate = round((Losses / games), 5)) %>%
  mutate(loss_rate = round((Wins / games), 5)) %>%
  mutate(patch_year = year(patch_date)) %>%
  rename(losses = Wins, wins = Losses, map = 'Map Name') %>%
  select(matchup, map, patch, patch_date, patch_year, 
         wins, losses, win_rate, games, loss_rate, pillar_label, pillar)

# create tables for each matchup and patch
# create list of unique patches
patches = unique(z_matchups$patch)
patches

# zvp max and min
zvp = subset(z_matchups, matchup == 'ZvP')

# max
zvp[which.max(zvp$win_rate),]

# min
zvp[which.min(zvp$win_rate),]



## separate matchups and pillar/no pillar

zvt_npillar = subset(z_matchups, (pillar == 0) & (matchup == 'ZvT'))
zvt_pillar = subset(z_matchups, (pillar == 1) & (matchup == 'ZvT'))

zvp_npillar = subset(z_matchups, (pillar == 0) & (matchup == 'ZvP'))
zvp_pillar = subset(z_matchups, (pillar == 1) & (matchup == 'ZvP'))

# calculate both matchup pillar's win rate
zvt_pillar_win_rate = sum(zvt_pillar$wins) / sum(zvt_pillar$games)

zvp_pillar_win_rate = sum(zvp_pillar$wins) / sum(zvp_pillar$games)

# function for matchup, patch
sub_table = function(patchnum, race) {
  tempdata = z_matchups %>%
    filter(patch == patchnum) %>%
    filter(matchup == race)
}

# function to grab summed games


# separate tables for ZvP
zvp_p3.11 = sub_table(patches[1], 'ZvP')
zvp_p3.12 = sub_table(patches[2], 'ZvP')
zvp_p3.14 = sub_table(patches[3], 'ZvP')
zvp_p3.8 = sub_table(patches[4], 'ZvP')
zvp_p4.0 = sub_table(patches[5], 'ZvP')
zvp_p4.1.1 = sub_table(patches[6], 'ZvP')
zvp_p4.1.4 = sub_table(patches[7], 'ZvP')
zvp_p4.10.1 = sub_table(patches[8], 'ZvP')
zvp_p4.11.4 = sub_table(patches[9], 'ZvP')
zvp_p4.2.1 = sub_table(patches[10], 'ZvP')
zvp_p4.3 = sub_table(patches[11], 'ZvP')
zvp_p4.8.3 = sub_table(patches[12], 'ZvP')
zvp_p5.0.2 = sub_table(patches[13], 'ZvP')

zvp_tables = c(zvp_p3.11, zvp_p3.12, zvp_p3.14, zvp_p3.8, zvp_p4.0,
               zvp_p4.1.1, zvp_p4.1.4, zvp_p4.10.1, zvp_p4.11.4,
               zvp_p4.2.1, zvp_p4.3, zvp_p4.8.3, zvp_p5.0.2)

# separate tables for ZvT
zvt_p3.11 = sub_table(patches[1], 'ZvT')
zvt_p3.12 = sub_table(patches[2], 'ZvT')
zvt_p3.14 = sub_table(patches[3], 'ZvT')
zvt_p3.8 = sub_table(patches[4], 'ZvT')
zvt_p4.0 = sub_table(patches[5], 'ZvT')
zvt_p4.1.1 = sub_table(patches[6], 'ZvT')
zvt_p4.1.4 = sub_table(patches[7], 'ZvT')
zvt_p4.10.1 = sub_table(patches[8], 'ZvT')
zvt_p4.11.4 = sub_table(patches[9], 'ZvT')
zvt_p4.2.1 = sub_table(patches[10], 'ZvT')
zvt_p4.3 = sub_table(patches[11], 'ZvT')
zvt_p4.8.3 = sub_table(patches[12], 'ZvT')
zvt_p5.0.2 = sub_table(patches[13], 'ZvT')

zvt_tables = c(zvt_p3.11, zvt_p3.12, zvt_p3.14, zvt_p3.8, zvt_p4.0,
               zvt_p4.1.1, zvt_p4.1.4, zvt_p4.10.1, zvt_p4.11.4, 
               zvt_p4.2.1, zvt_p4.3, zvt_p4.8.3, zvt_p5.0.2)

# calculate pillar map win rates per patch & matchup

# keep count of # of pillar maps, # of no pillar maps, p value, effect size

# t-test functions
zvt_p3.11_npillar = subset(zvt_p3.11, pillar == 0)
zvt_p3.11_pillar = subset(zvt_p3.11, pillar == 1)

t.test(zvt_p3.11_npillar$win_rate, data = zvt_p3.11_npillar, mu = sum(zvt_p3.11_npillar$wins) / sum(zvt_p3.11_npillar$games))

# issue: sample size, samples of number of maps
# 2 win rates versus 5 doesn't do great
# maybe put 2-3 patches together or year over year

# patches by year
years = unique(z_matchups$patch_year)
years

# 2016
# zvt
zvt_2016 = subset(z_matchups, (patch_year == 2016) & ('Matchup' == ''))

# zvp

# 2017

# 2018

# 2019

# 2020

# number of games in each year
for (year in years) {
  #print(year)
  year_slice = subset(z_matchups, patch_year == year)
  tot_games = sum(year_slice$games)
  printout = paste(year, tot_games, sep = ':')
  print(printout)
}

# make new table
# year, patches included, number of games, num games w/ pillar, 
# num games w/o pillar, num maps w/ pillar, num maps w/o pillar
summary_col_names = c('year', 'patches',  'games_pillar', 'games_npillar', 'games',
                      'num_pillar_maps', 'num_npillar_maps', 'num_maps')


summary_years = data.frame(matrix(ncol = length(summary_col_names), nrow=0,
                                  dimnames = list(NULL, summary_col_names)))

zvt_summary_years = data.frame(matrix(ncol = length(summary_col_names), nrow=0,
                                  dimnames = list(NULL, summary_col_names)))

zvp_summary_years = data.frame(matrix(ncol = length(summary_col_names), nrow=0,
                                  dimnames = list(NULL, summary_col_names)))

# create summary table for year by year
for (year in years) {
  year_slice = subset(z_matchups, patch_year == year)
  pillar_subset = subset(year_slice, pillar == 1)
  npillar_subset = subset(year_slice, pillar == 0)
  # grab patches included
  unique_patches = paste(unique(year_slice$patch), collapse = ', ')
  
  # number of games with a pillar
  tot_pillar_games = sum(pillar_subset$games)
  
  # number of games without a pillar
  tot_npillar_games = sum(npillar_subset$games)
  
  # grab total number of games
  tot_games = sum(year_slice$games)
  
  # number of maps with a pillar
  unique_pillar_maps = length(unique(pillar_subset$map))
  
  # number of maps without a pillar
  unique_npillar_maps = length(unique(npillar_subset$map))
  
  # number of maps
  num_maps = length(unique(year_slice$map))
  
  insert_row = c(year, unique_patches, tot_pillar_games, tot_npillar_games,
                 tot_games, unique_pillar_maps, unique_npillar_maps, 
                 num_maps)
  #as.data.frame(insert_row)
  #print(as.data.frame(t(insert_row)))
  
  summary_years = rbind(summary_years, as.data.frame(t(insert_row)))
}

# renaming column names to assigned ones
colnames(summary_years) = summary_col_names

# zvt over the years
for (year in years) {
  year_slice = z_matchups %>%
    filter(matchup == 'ZvT', patch_year == year)
  pillar_subset = subset(year_slice, pillar == 1)
  npillar_subset = subset(year_slice, pillar == 0)
  # grab patches included
  unique_patches = paste(unique(year_slice$patch), collapse = ', ')
  
  # number of games with a pillar
  tot_pillar_games = sum(pillar_subset$games)
  
  # number of games without a pillar
  tot_npillar_games = sum(npillar_subset$games)
  
  # grab total number of games
  tot_games = sum(year_slice$games)
  
  # number of maps with a pillar
  unique_pillar_maps = length(unique(pillar_subset$map))
  
  # number of maps without a pillar
  unique_npillar_maps = length(unique(npillar_subset$map))
  
  # number of maps
  num_maps = length(unique(year_slice$map))
  
  insert_row = c(year, unique_patches, tot_pillar_games, tot_npillar_games,
                 tot_games, unique_pillar_maps, unique_npillar_maps, 
                 num_maps)
  #as.data.frame(insert_row)
  #print(as.data.frame(t(insert_row)))
  
  zvt_summary_years = rbind(zvt_summary_years, as.data.frame(t(insert_row)))
}

colnames(zvt_summary_years) = summary_col_names

# zvp over the years
for (year in years) {
  year_slice = z_matchups %>%
    filter(matchup == 'ZvP', patch_year == year)
  pillar_subset = subset(year_slice, pillar == 1)
  npillar_subset = subset(year_slice, pillar == 0)
  # grab patches included
  unique_patches = paste(unique(year_slice$patch), collapse = ', ')
  
  # number of games with a pillar
  tot_pillar_games = sum(pillar_subset$games)
  
  # number of games without a pillar
  tot_npillar_games = sum(npillar_subset$games)
  
  # grab total number of games
  tot_games = sum(year_slice$games)
  
  # number of maps with a pillar
  unique_pillar_maps = length(unique(pillar_subset$map))
  
  # number of maps without a pillar
  unique_npillar_maps = length(unique(npillar_subset$map))
  
  # number of maps
  num_maps = length(unique(year_slice$map))
  
  insert_row = c(year, unique_patches, tot_pillar_games, tot_npillar_games,
                 tot_games, unique_pillar_maps, unique_npillar_maps, 
                 num_maps)
  #as.data.frame(insert_row)
  #print(as.data.frame(t(insert_row)))
  
  zvp_summary_years = rbind(zvp_summary_years, as.data.frame(t(insert_row)))
}

colnames(zvp_summary_years) = summary_col_names

# what are the maps in 2019 and 2020 that don't have pillars
recent_npillar_maps = z_matchups %>% 
  filter(patch_year == 2019 | patch_year == 2020, pillar == 0) %>%
  select(map)

unique(recent_npillar_maps)

paste(array(unique(z_matchups$patch)), sep=',')
# number of maps in each year

# 2-3 patch rolling window, 1 patch movement

# zvt no pillar / pillar mean win rates, sd, effect size
mean(zvt_npillar$win_rate)
sqrt(var(zvt_npillar$win_rate))
sum(zvt_npillar$games)
zvt_npillar$win_rate

mean(zvt_pillar$win_rate)
sqrt(var(zvt_pillar$win_rate))
sum(zvt_pillar$games)

cohen.d(d = zvt_npillar$win_rate,
        f = zvt_pillar$win_rate)
        #formula = zvt_npillar$win_rate ~ zvt_npillar$pillar)

zvt_pooled_sd = sqrt((var(zvt_npillar$win_rate) + var(zvt_pillar$win_rate))/ 2)
(mean(zvt_npillar$win_rate) - mean(zvt_pillar$win_rate)) / zvt_pooled_sd

# zvp, same ideas
mean(zvp_npillar$win_rate)
sqrt(var(zvp_npillar$win_rate))
sum(zvp_npillar$games)

mean(zvp_pillar$win_rate)
sqrt(var(zvp_pillar$win_rate))
sum(zvp_pillar$games)

zvp_pooled_sd = sqrt((var(zvp_npillar$win_rate) + var(zvp_pillar$win_rate))/ 2)
(mean(zvp_npillar$win_rate) - mean(zvp_pillar$win_rate)) / zvp_pooled_sd

cohen.d(d = zvp_npillar$win_rate,
        f = zvp_pillar$win_rate)
        #formula = zvp_npillar$win_rate ~ zvp_npillar$pillar)

# t-tests

zvt.test = t.test(zvt_npillar$win_rate, mu = mean(zvt_pillar$win_rate))
zvt.test


zvp.test = t.test(zvp_npillar$win_rate, mu = mean(zvp_pillar$win_rate))
zvp.test