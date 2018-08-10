# Base exploration of the data provided by Jack Davis

# Libraries
library(tidyr)
library(dplyr)
library(Hmisc)

# Loading data example
# Requires the path ~\github\Goalie-Fatigue\Data
sampleData <- read.csv("~/GitHub/Goalie-Fatigue/Data/NHL 20001 20132014 timeout and capt 2017-09-01.csv")

# # Way to iterate
# # First value: Gets repeated 4 times, one for each of the year combos
# fv <- rep(seq(20001, 21230), times = 1, each = 4) %>% as.character()
# 
# # Second value: Year combos
# sv <- c("20132014", "20142015", "20152016", "20162017") %>% rep(times = length(fv) / 4)
# 
# # Combine fv and sv
# yearIter <- paste0("~/GitHub/Goalie-Fatigue/Data/NHL ", fv, " ", sv, " timeout and capt 2017-09-01.csv")
# 
# # Validate
# dataTest <- read.csv(yearIter[1]) # Works as intended
# 
# Full data set, easier way like what Brad did
files <- list.files("Data", pattern = "*.csv", full.names = TRUE)
dataFull <- lapply(files, read.csv)
allGames <- do.call("rbind", dataFull)


# Beginning of the data work, separate into games via group_by and cleanup factors used later
allGames <- allGames %>% group_by(season, gcode) %>% as_tibble() %>% mutate(homezone = as.character(homezone),
                                                                    hometeam = as.character(hometeam),
                                                                    awayteam = as.character(awayteam),
                                                                    ev.team  = as.character(ev.team))
allGames <- allGames %>% filter(etype %nin% c("GOFF", "EGT", "EGPID", "EIEND", "EISTR", "CHANGE"))

year2013 <- allGames %>% filter(season == "20132014")
year2014 <- allGames %>% filter(season == "20142015")
year2015 <- allGames %>% filter(season == "20152016")
year2016 <- allGames %>% filter(season == "20162017")

# Seem to just be stoppages in play, all of which are non-penalties and non-saves. E.g. high stick, hand pass, offside, etc
skippedGames <- allGames %>% filter(season %nin% c("20132014", "20142015", "20152016", "20162017"))

# Choose an arbitrary game to test methods, then apply to all with lapply
# game <- dataFull[[13]] %>% as_tibble() %>% mutate(homezone = as.character(homezone),
#                                                   hometeam = as.character(hometeam),
#                                                   awayteam = as.character(awayteam),
#                                                   ev.team  = as.character(ev.team))

# Need to create a marker for zone continuous
  # homeTeam: Still needs tidying but currently checks prev event for diff zone, then curr ev for off zone for home team and their event
  # awayTeam: Should be play is in neutral or home team's offensive zone (away team's defence) then changes to away team action in home team's def zone
  year2013 <- year2013 %>% mutate(homeMarker = case_when(
                                          (lag(homezone, default = "Neu") != "Off"
                                           | (lag(ev.team) != hometeam & lag(homezone, default = "Neu") == "Off"))
                                          & homezone == "Off"
                                          & ev.team == hometeam
                                          ~ 1,
                                          TRUE ~ 0),
                                  awayMarker = case_when(
                                    (lag(homezone, default = "Neu") != "Def" | 
                                       (lag(ev.team) == hometeam & lag(homezone, default = "Neu") == "Def"))
                                    & homezone == "Def"
                                    & ev.team != hometeam
                                    ~ 1,
                                    TRUE ~ 0),
                                  lazyZonesHome = case_when(homezone == "Off" ~ 1, TRUE ~ 0),
                                  lazyZonesAway = case_when(homezone == "Def" ~ 1, TRUE ~ 0),
                                  lazyEntryHome = case_when(lazyZonesHome == 1 & lag(lazyZonesHome, default = 0) == 0
                                                            ~ 1,
                                                            TRUE ~ 0),
                                  lazyExitHome = case_when(lazyZonesHome == 0 & lag(lazyZonesHome, default = 0) == 1
                                                           ~ 1,
                                                           TRUE ~ 0),
                                  lazyEntryAway = case_when(lazyZonesAway == 1 & lag(lazyZonesAway, default = 0) == 0
                                                            ~ 1,
                                                            TRUE ~ 0),
                                  lazyExitAway = case_when(lazyZonesAway == 0 & lag(lazyZonesAway, default = 0) == 1
                                                           ~ 1,
                                                           TRUE ~ 0),
                                  homeInOffZone = (cumsum(lazyEntryHome) - cumsum(lazyExitHome))*cumsum(lazyEntryHome),
                                  awayInOffZone = (cumsum(lazyEntryAway) - cumsum(lazyExitAway))*cumsum(lazyEntryAway))

  
# Filter out zones that had no generated shots  
homeZonesWithShots2013 <- year2013 %>% group_by(gcode, homeInOffZone, awayInOffZone) %>% filter(homeInOffZone > 0 | awayInOffZone > 0) %>% 
                    dplyr::summarize(nshots = sum(etype %in% c("SHOT", "GOAL"))) %>% filter(nshots > 0, homeInOffZone > 0) %>% select(gcode, homeInOffZone, nshots) %>%
                    mutate(gcodeAndHomeZoneNum = paste0(gcode, homeInOffZone))
      

awayZonesWithShots2013 <- year2013 %>% group_by(gcode, homeInOffZone, awayInOffZone) %>% filter(homeInOffZone > 0 | awayInOffZone > 0) %>% 
                    dplyr::summarize(nshots = sum(etype %in% c("SHOT", "GOAL"))) %>% filter(nshots > 0, awayInOffZone > 0) %>% ungroup() %>% select(gcode, awayInOffZone, nshots)  %>%
                    mutate(gcodeAndAwayZoneNum = paste0(gcode, awayInOffZone))

# Use the zones and gcodes that had shots to filter events from the main data set
zonesWithShots <- year2013 %>% mutate(gcodeAndHomeZoneNum = paste0(gcode, homeInOffZone),
                                      gcodeAndAwayZoneNum = paste0(gcode, awayInOffZone)) %>%
                               filter(gcodeAndHomeZoneNum %in% homeZonesWithShots2013$gcodeAndHomeZoneNum | gcodeAndAwayZoneNum %in% awayZonesWithShots2013$gcodeAndAwayZoneNum) %>%
                               select(-c(homeMarker, awayMarker, lazyZonesHome, lazyZonesAway, lazyEntryHome, lazyEntryAway, lazyExitHome, lazyExitAway))
  
# Identify all the shots a specific goalie faces in a year from the zonesWithShots 
goalieSpecific68 <- year2013 %>% filter((home.G == 68 & awayInOffZone > 0) | (away.G == 68 & homeInOffZone > 0))

# Try to accomplish this, simultaneously, for all goalies
goalieSpecifics <- list()
allGoalies <- c(unique(zonesWithShots$home.G), unique(zonesWithShots$away.G)) %>% unique() %>% sort()
j = 1
# Filter all actions that only involve that goalie, not their team's offense on the other goalie
for(i in allGoalies){
  goalieSpecifics[[j]] <- zonesWithShots %>% filter((home.G == i  & awayInOffZone > 0) | (away.G == i & homeInOffZone > 0)) %>% mutate(goalieFile = i)
  j <- j+1
}

# Goalie ID 1, which is in the first spot, appears to be empty net situations

# Use these goalie IDs to build game profiles for each goaltender
# Might be useful to use more than just shot. Include misses, blocks, goals potentially
actions <- c("SHOT", "MISS", "BLOCK", "GOAL")
goalieSpecific68 <- goalieSpecific68 %>% group_by(gcode, period) %>% mutate(shotsDuringPeriod = cumsum(etype %in% "SHOT"),
                                                                            avgTimeBtwnShots = case_when(
                                                                              (etype %in% "SHOT" & period == 1) ~ seconds / shotsDuringPeriod,
                                                                              (etype %in% "SHOT" & period == 2) ~ (seconds - 1200) / shotsDuringPeriod,
                                                                              (etype %in% "SHOT" & period == 3) ~ (seconds - 2400) / shotsDuringPeriod,
                                                                              TRUE ~ 0),
                                                                            actionsDuringPeriod = cumsum(etype %in% c("SHOT", "MISS", "BLOCK", "GOAL")),
                                                                            avgTimeBtwnAction = case_when(
                                                                              (etype %in% actions & period == 1) ~ seconds / actionsDuringPeriod,
                                                                              (etype %in% c("SHOT", "MISS", "BLOCK", "GOAL") & period == 2) ~ (seconds - 1200) / actionsDuringPeriod,
                                                                              (etype %in% c("SHOT", "MISS", "BLOCK", "GOAL") & period == 3) ~ (seconds - 2400) / actionsDuringPeriod,
                                                                              TRUE ~ 0)) # Shots per period

# Build the function that accomplishes the above
goalieStats <- function(goalieDF){
  actions <- c("SHOT", "MISS", "BLOCK", "GOAL")
  goalieDF <- goalieDF %>% ungroup() %>% mutate(gcode = as.factor(gcode))
  goalieDF <- goalieDF %>% group_by(gcode, period) %>% mutate(shotsDuringPeriod = cumsum(etype %in% c("SHOT", "GOAL")),
                                        avgTimeBtwnShots = case_when(
                                          (etype %in% c("SHOT", "GOAL") & period == 1) ~ seconds / shotsDuringPeriod,
                                          (etype %in% c("SHOT", "GOAL") & period == 2) ~ (seconds - 1200) / shotsDuringPeriod,
                                          (etype %in% c("SHOT", "GOAL") & period == 3) ~ (seconds - 2400) / shotsDuringPeriod,
                                          TRUE ~ 0),
                                        actionsDuringPeriod = cumsum(etype %in% actions),
                                        avgTimeBtwnActions  = case_when(
                                          (etype %in% actions & period == 1) ~ seconds / actionsDuringPeriod,
                                          (etype %in% actions & period == 2) ~ (seconds - 1200) / actionsDuringPeriod,
                                          (etype %in% actions & period == 3) ~ (seconds - 2400) / actionsDuringPeriod,
                                          TRUE ~ 0)) %>% ungroup() %>% group_by(gcode) %>% mutate(
                                        savePerc  = cumsum(etype %in% "SHOT") / cumsum(etype %in% c("SHOT", "GOAL")),
                                        # Shots on net likely contribute to physical fatigue
                                        fastShots = cumsum(etype %in% c("SHOT", "GOAL") & type %in% c("Slap", "Snap")),
                                        softShots = cumsum(etype %in% c("SHOT", "GOAL") & type %in% c("Wrist", "Backhand")),
                                        closeShots= cumsum(etype %in% c("SHOT", "GOAL") & type %in% c("Wrap", "Tip-In", "Deflected")),
                                        totalShots= cumsum(etype %in% c("SHOT", "GOAL")),
                                        # Mental fatigue may be influenced by things such as attempts/blocks, non-shots on net
                                        otherEvents = cumsum(etype %in% c("MISS", "BLOCK") )
                                  )
  return(goalieDF)
}

goalieSpecificStats <- lapply(goalieSpecifics, FUN = goalieStats)

goalieSpecificStats[[2]] %>% filter(etype %in% "SHOT") %>% nrow()
goalieSpecificStats[[2]] %>% filter(gcode == "20004") %>% select(fastShots, softShots, closeShots, shotsDuringPeriod, totalShots, otherEvents, savePerc, home.score, away.score)  %>% View()

#######################
# Integrate Barinder's shot quality work here #
#######################

# More plotting #
# Save perc vs. time
View(goalieSpecificStats[[2]])
goalieSpecificStats[[2]] %>% ggplot(aes(x = seconds, y = savePerc, group = gcode)) + ggtitle("Save Percentage across game time")+
   geom_point() + geom_smooth(aes(colour=gcode))

goalieSpecificStats[[2]] %>% ggplot(aes(x = totalShots, y = savePerc, group = gcode, colour = gcode)) + ggtitle("Save Percentage across game time")+
  geom_point() #+ geom_smooth(aes(colour=gcode))




# Try some basic modeling 
  # GLM
goalieShots <- goalieSpecificStats[[2]] %>% mutate(save = case_when(etype %in% c("SHOT") ~ 1,
                                                                                 etype %in% c("GOAL") ~ 0,
                                                                                 TRUE ~ 0))
goalieShotsEncountered <- goalieShots %>% ungroup() %>% filter(etype %in% c("SHOT", "GOAL"))
dim(goalieShotsEncountered)


library(glmnet)
lambdas <- 10^{seq(from=-2,to=5,length=100)}
Xfull <- model.matrix(save ~ fastShots+ softShots+ closeShots+ shotsDuringPeriod+ otherEvents+ savePerc+ home.score + away.score + avgTimeBtwnShots + avgTimeBtwnActions, 
                      data = goalieShotsEncountered)
#XFull <- goalieShotsEncountered[, c("fastShots", "softShots", "closeShots", "shotsDuringPeriod", "otherEvents", "savePerc", "home.score", "away.score", "avgTimeBtwnShots", "avgTimeBtwnActions")]
Y <- goalieShotsEncountered$save
lsfit <- glm.fit(Xfull, Y)
# LASSO
modeling <-  glmnet(Xfull, Y,
                    alpha = 1,
                    lambda = lambdas)

plot(modeling, xvar = "lambda", ylim = c(-0.2, 0.2))
summary(modeling)

# CV
modelingCV <- cv.glmnet(Xfull, Y,
                        alpha = 1,
                        lambda = lambdas)

plot(modelingCV)
bestLAM <- modelingCV$lambda.1se

lassoBest <- glmnet(Xfull, Y, alpha = 1, lambda=bestLAM)
coef(lassoBest)

# lassoBest is now the model to consider for GLM
# Build that model 

