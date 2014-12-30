#------------------------------------------------------------------------------
# File: NHLExperiences.R
# Author: Martin Cote
# URL: 
# Email: 
# Purpose: Answer the Analytics question: What was the level of experience of each Stanley Cup
#          winning team for each year?
#
# Pre-requisites:
# 1. Data loaded required from hockey_databank processed.
#
#------------------------------------------------------------------------------
#

make.NHLExperiences <- function(inLocationFolder) {
  
  library(plyr)
  
  # goalies_season_reg_overall
  load(path.expand(paste(inLocationFolder, "/goalies_season_reg_overall.RData", sep="")))
  # skaters_season_reg_overall
  load(path.expand(paste(inLocationFolder, "/skaters_season_reg_overall.RData", sep="")))
  
  # SeriesPost
  load(path.expand(paste(inLocationFolder, "/SeriesPost.RData", sep="")))
  # goalies_season_playoff_overall
  load(path.expand(paste(inLocationFolder, "/goalies_season_playoff_overall.RData", sep="")))
  # skaters_season_playoff_overall
  load(path.expand(paste(inLocationFolder, "/skaters_season_playoff_overall.RData", sep="")))

  FORWARD_POS <- c("LW","RW","C","LW/C","LW/D","RW/LW","F","C/LW","F/D")
  DEFENSE_POS <- c("D","D/F","D/RW")
  GOALIE_POS <- c("G")
  
  # calculates players age for each team per year
  # data.frame => season, team, position, avg age
  CALC_EXPERIENCES_AGE <- function() {
    avg_age_skaters <- ddply(skaters_season_reg_overall, .(season, team_short, pos), summarize, age_avg = mean(age))
    avg_age_goalies <- ddply(goalies_season_reg_overall, .(season, team_short), summarize, age_avg = mean(age))
    
    avg_age_goalies$pos <- "G" # add position to merge with 'skaters'
    merge(avg_age_skaters, avg_age_goalies, all=TRUE)
  }
  
  # calculates players numbers of regular season games for each team per year
  # data.frame => season, player, pos, avg_time_on_ice, avg #
  CALC_EXPERIENCES_NUM_SEASON <- function() {

    # Refer to: http://stackoverflow.com/questions/21801111/r-for-each-row-calculate-a-sum-taking-values-of-one-of-the-columns-from-the-rows
    # and research file unittest.NHLExperiences.R
    skaters_season_reg_overall$num_games_tot <- ave(skaters_season_reg_overall$gp, skaters_season_reg_overall$player, FUN=function(x) rev(cumsum(c(0, head(rev(x), -1)))))
    goalies_season_reg_overall$num_games_tot <- ave(goalies_season_reg_overall$gp, goalies_season_reg_overall$player, FUN=function(x) rev(cumsum(c(0, head(rev(x), -1)))))
    
    avg_numgames_skaters <- ddply(skaters_season_reg_overall, .(season, player, team_short, pos, gp, avg_time_on_ice), summarize, num_games_avg = mean(num_games_tot))
    avg_numgames_goalies <- ddply(goalies_season_reg_overall, .(season, player, team_short, gp), summarize, num_games_avg = mean(num_games_tot))
    avg_numgames_goalies$pos <- "G" # add position to merge with 'skaters'
    avg_numgames_goalies$avg_time_on_ice <- 60
    merge(avg_numgames_skaters, avg_numgames_goalies, all=TRUE)
  }
  
  # calculates players numbers of playoff games for each team per year
  # data.frame => season, player, pos, avg_time_on_ice, avg #
  CALC_EXPERIENCES_NUM_PLAYOFF <- function() {
    
    # Refer to: http://stackoverflow.com/questions/21801111/r-for-each-row-calculate-a-sum-taking-values-of-one-of-the-columns-from-the-rows
    # and research file unittest.NHLExperiences.R
    skaters_season_playoff_overall$num_games_tot <- ave(skaters_season_playoff_overall$gp, skaters_season_playoff_overall$player, FUN=function(x) rev(cumsum(c(0, head(rev(x), -1)))))
    goalies_season_playoff_overall$num_games_tot <- ave(goalies_season_playoff_overall$gp, goalies_season_playoff_overall$player, FUN=function(x) rev(cumsum(c(0, head(rev(x), -1)))))
    
    avg_numgames_skaters <- ddply(skaters_season_playoff_overall, .(season, player, team_short, pos, gp, avg_time_on_ice), summarize, num_games_avg = mean(num_games_tot))
    avg_numgames_goalies <- ddply(goalies_season_playoff_overall, .(season, player, team_short, gp), summarize, num_games_avg = mean(num_games_tot))
    avg_numgames_goalies$pos <- "G" # add position to merge with 'skaters'
    avg_numgames_goalies$avg_time_on_ice <- 60
    merge(avg_numgames_skaters, avg_numgames_goalies, all=TRUE)
  }
  
  
  list(calculatesExperiencesWithAge = CALC_EXPERIENCES_AGE,
       calculatesExperiencesWithSeason = CALC_EXPERIENCES_NUM_SEASON,
       calculatesExperiencesWithPlayoff = CALC_EXPERIENCES_NUM_PLAYOFF)
}