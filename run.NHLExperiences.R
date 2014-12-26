# Run NHL experiences...

# load source
source("make.NHLExperiences.R")

stanleycup_winners <- read.csv("C:/Users/martin.cote/Documents/GitHub/nhlstats/stanleycupwinners.csv", header=TRUE)

# prep functions
inLocationFolder <- "C:/Users/martin.cote/Documents/GitHub/nhlstats/data"
nhlExperiencesFunc <- make.NHLExperiences(inLocationFolder)
calcExpAges <- nhlExperiencesFunc$calculatesExperiencesWithAge
calcExpNumSeason <- nhlExperiencesFunc$calculatesExperiencesWithSeason
calcExpNumPlayoff <- nhlExperiencesFunc$calculatesExperiencesWithPlayoff

# experiments...
# 1. with age...
avg_age <- calcExpAges()

avg_age_all <- ddply(avg_age, .(season), summarize, age = mean(age_avg, na.rm = TRUE))
avg_age_byteam_all <- ddply(avg_age, .(season, team_short), summarize, age = mean(age_avg, na.rm = TRUE))
avg_age_byteam_bypos <- ddply(avg_age, .(season, team_short, pos), summarize, age = mean(age_avg, na.rm = TRUE))

# 2. with numbers of regular seasons games...
avg_games_byplayer_all <- calcExpNumSeason()
# TODO: Remove the non-regular players?  Should I?  What's the criteria: < 10 gp & < 5 avg_time_on_ice ?

avgmed_games_byteambyseason <- ddply(avg_games_byplayer_all, .(season, team_short), summarize, games_avg = mean(num_games_avg, na.rm = TRUE), games_median = median(num_games_avg, na.rm = TRUE))

# 3. with numbers of regular seasons games...
avg_playoff_byplayer_all <- calcExpNumPlayoff()
# TODO: Remove the non-regular players?  Should I?  What's the criteria in this case?!?

avgmed_playoff_byteambyseason <- ddply(avg_playoff_byplayer_all, .(season, team_short), summarize, games_avg = mean(num_games_avg, na.rm = TRUE), games_median = median(num_games_avg, na.rm = TRUE))
