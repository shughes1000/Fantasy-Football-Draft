library(dplyr)
library(ggplot2)

fix_ADP = function(df) {
  # for all players without teams, some columns need to be shifted over by two
  iter = seq(12, 5, -1)
  
  for(i in iter) {
    df[which(grepl("\\d", df$Team)), i] = df[which(grepl("\\d", df$Team)), i - 2]
  }
  
  # remove team and bye week columns because they are not accurate for said year
  df = df[ , -c(3, 4)]
  
  colnames(df)[1] = "ADP_rank"
  colnames(df)[10] = "ADP_average"
  
  return(df)
}

fix_finish = function(df) {
  colnames(df)[1] = "points_rank"
  colnames(df)[7] = "points_average"
  return(df)
}

# format data for each year
# note: I only looked at weeks 1-16 (including in 2021), PPR scoring

twenty_12_ADP = read.csv("FantasyPros_2012_Overall_ADP_Rankings.csv")
twenty_12_ADP = fix_ADP(twenty_12_ADP)
twenty_12_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2012.csv")
twenty_12_finish = fix_finish(twenty_12_finish)
twenty_12 = merge(twenty_12_ADP, twenty_12_finish, by = "Player")
twenty_12$year = 2012

twenty_13_ADP = read.csv("FantasyPros_2013_Overall_ADP_Rankings.csv")
twenty_13_ADP = fix_ADP(twenty_13_ADP)
twenty_13_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2013.csv")
twenty_13_finish = fix_finish(twenty_13_finish)
twenty_13 = merge(twenty_13_ADP, twenty_13_finish, by = "Player")
twenty_13$year = 2013

twenty_14_ADP = read.csv("FantasyPros_2014_Overall_ADP_Rankings.csv")
twenty_14_ADP = fix_ADP(twenty_14_ADP)
twenty_14_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2014.csv")
twenty_14_finish = fix_finish(twenty_14_finish)
twenty_14 = merge(twenty_14_ADP, twenty_14_finish, by = "Player")
twenty_14$year = 2014

twenty_15_ADP = read.csv("FantasyPros_2015_Overall_ADP_Rankings.csv")
twenty_15_ADP = fix_ADP(twenty_15_ADP)
twenty_15_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2015.csv")
twenty_15_finish = fix_finish(twenty_15_finish)
twenty_15 = merge(twenty_15_ADP, twenty_15_finish, by = "Player")
twenty_15$year = 2015

twenty_16_ADP = read.csv("FantasyPros_2016_Overall_ADP_Rankings.csv")
twenty_16_ADP = fix_ADP(twenty_16_ADP)
twenty_16_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2016.csv")
twenty_16_finish = fix_finish(twenty_16_finish)
twenty_16 = merge(twenty_16_ADP, twenty_16_finish, by = "Player")
twenty_16$year = 2016

twenty_17_ADP = read.csv("FantasyPros_2017_Overall_ADP_Rankings.csv")
twenty_17_ADP = fix_ADP(twenty_17_ADP)
twenty_17_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2017.csv")
twenty_17_finish = fix_finish(twenty_17_finish)
twenty_17 = merge(twenty_17_ADP, twenty_17_finish, by = "Player")
twenty_17$year = 2017

twenty_18_ADP = read.csv("FantasyPros_2018_Overall_ADP_Rankings.csv")
twenty_18_ADP = fix_ADP(twenty_18_ADP)
twenty_18_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2018.csv")
twenty_18_finish = fix_finish(twenty_18_finish)
twenty_18 = merge(twenty_18_ADP, twenty_18_finish, by = "Player")
twenty_18$year = 2018

twenty_19_ADP = read.csv("FantasyPros_2019_Overall_ADP_Rankings.csv")
twenty_19_ADP = fix_ADP(twenty_19_ADP)
twenty_19_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2019.csv")
twenty_19_finish = fix_finish(twenty_19_finish)
twenty_19 = merge(twenty_19_ADP, twenty_19_finish, by = "Player")
twenty_19$year = 2019

twenty_20_ADP = read.csv("FantasyPros_2020_Overall_ADP_Rankings.csv")
twenty_20_ADP = fix_ADP(twenty_20_ADP)
twenty_20_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2020.csv")
twenty_20_finish = fix_finish(twenty_20_finish)
twenty_20 = merge(twenty_20_ADP, twenty_20_finish, by = "Player")
twenty_20$year = 2020

twenty_21_ADP = read.csv("FantasyPros_2021_Overall_ADP_Rankings.csv")
twenty_21_ADP = fix_ADP(twenty_21_ADP)
twenty_21_finish = read.csv("FantasyPros_Fantasy_Football_Points_PPR_2021.csv")
twenty_21_finish = fix_finish(twenty_21_finish)
twenty_21 = merge(twenty_21_ADP, twenty_21_finish, by = "Player")
twenty_21$year = 2021



# combine yearly data into big data frame

# fix types
twenty_all = rbind(twenty_12, twenty_13, twenty_14, twenty_15, twenty_16, twenty_17, twenty_18, twenty_19, twenty_20, twenty_21)
twenty_all$ESPN = as.integer(twenty_all$ESPN)
twenty_all$RTSports = as.integer(twenty_all$RTSports)
twenty_all$FFC = as.integer(twenty_all$FFC)
twenty_all$ADP_average = as.numeric(twenty_all$ADP_average)

# remove sleeper because it is just NA
twenty_all = twenty_all[ , -9]

# I am going to omit players who played less than 8 games because their points average can be misleading
twenty_all = twenty_all[-which(twenty_all$Games < 8), ]

# reorder data
twenty_all = arrange(twenty_all, ADP_average)


# quick plot to make sure everything looks good
ggplot(data = twenty_all,
       mapping = aes(x = ADP_average, y = points_average, color = Position))+
  geom_point()

# save to csv
write.csv(twenty_all, "twenty_all.csv")
