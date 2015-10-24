# Run NHL experiences...
#
# Usage:
# 1. Load data files
# 2. Establish winning team for each year
# 3. Establish:
#     3.1 Average age by overall, forward, defense and goalie for each team and winning team.
#     3.2 Average number of games played in regular season by overall, foward, defense and goalie
#         for each team and winning team.
#     3.3 Average number of games played in playoff by overall, forward, defense and goalie
#         for each team and winning team.
# 4. Establish correlation/causation
#------------------------------------------------------------------------------

# load source
#setwd("~/Documents/github-repo/nhlexperiences")
source("make.NHLExperiences.R")

#stanleycup_winners <- read.csv("~/github-repo/nhlstats/stanleycupwinners.csv", header=TRUE)
#stanleycup_winners <- read.csv("C:/Users/martin.cote/Documents/GitHub/nhlstats/stanleycupwinners.csv", header=TRUE)
#stanleycup_winners <- read.csv("/Users/martincote/Documents/github-repo/nhlstats/stanleycupwinners.csv", header=TRUE)

# prep functions
#inLocationFolder <- "~/Documents/github-repo/nhlstats/data"
inLocationFolder <- "C:/Users/martin.cote/Documents/github-repo/nhlstats/data"
#inLocationFolder <- "/Users/martincote/Documents/github-repo/nhlstats/data"
nhlExperiencesFunc <- make.NHLExperiences(inLocationFolder)
calcExpAges <- nhlExperiencesFunc$calculatesExperiencesWithAge
calcExpNumSeason <- nhlExperiencesFunc$calculatesExperiencesWithSeason
calcExpNumPlayoff <- nhlExperiencesFunc$calculatesExperiencesWithPlayoff

# experiments...
# TODO: Should I use 'complete.cases' instead of using the 'na.rm=TRUE'?  To be investigated!

# 1. with age...
avg_age <- calcExpAges()

avg_age_all <- ddply(avg_age, .(season), summarize, age = mean(age_avg, na.rm = TRUE))
avg_age_byteam_all <- ddply(avg_age, .(season, team_short), summarize, age = mean(age_avg, na.rm = TRUE))
avg_age_byteam_bypos <- ddply(avg_age, .(season, team_short, pos), summarize, age = mean(age_avg, na.rm = TRUE))

# ...

# Plot average age by team compared to Stanley Cup winner average age
plot(avg_age_byteam_all$age)

# 2. with numbers of regular seasons games...
system.time(avg_games_byplayer_all <- calcExpNumSeason())
# TODO: Remove the non-regular players?  Should I?  What's the criteria: < 10 gp & < 5 avg_time_on_ice ?  Something related to the median versus avg?  To answer with stats...

avgmed_games_byteambyseason <- ddply(avg_games_byplayer_all, .(season, team_short), summarize, games_avg = mean(num_games_avg, na.rm = TRUE), games_median = median(num_games_avg, na.rm = TRUE))

# ...
plot(avgmed_games_byteambyseason$games_avg)

# 3. with numbers of regular seasons games...
system.time(avg_playoff_byplayer_all <- calcExpNumPlayoff())
# TODO: Remove the non-regular players?  Should I?  What's the criteria in this case?!?  To answer with stats... K-means...

avgmed_playoff_byteambyseason <- ddply(avg_playoff_byplayer_all, .(season, team_short), summarize, games_avg = mean(num_games_avg, na.rm = TRUE), games_median = median(num_games_avg, na.rm = TRUE))

# ... 
plot(avgmed_playoff_byteambyseason$games_avg)