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
                                          TRUE ~ 0),
                                        savePerc  = cumsum(etype %in% "SHOT") / cumsum(etype %in% c("SHOT", "GOAL")),
                                        # Shots on net likely contribute to physical fatigue
                                        fastShots = cumsum(etype %in% c("SHOT", "GOAL") & type %in% c("Slap", "Snap")),
                                        softShots = cumsum(etype %in% c("SHOT", "GOAL") & type %in% c("Wrist", "Backhand")),
                                        closeShots= cumsum(etype %in% c("SHOT", "GOAL") & type %in% c("Wrap", "Tip-In", "Deflected")),
                                        # Mental fatigue may be influenced by things such as attempts/blocks, non-shots on net
                                        otherEvents = cumsum(etype %in% c("MISS", "BLOCK") )
                                  )
  return(goalieDF)
}

goalieSpecificStats <- lapply(goalieSpecifics, FUN = goalieStats)

goalieSpecificStats[[2]] %>% filter(etype %in% "SHOT") %>% nrow()
goalieSpecificStats[[2]] %>% filter(gcode == "20004") %>% select(fastShots, softShots, closeShots, shotsDuringPeriod, otherEvents, savePerc, home.score, away.score) %>% tail() %>% View()

################# Not quite functional, small errors #################
# Now to create sequences of zone actions, based on either home or away
# year2013 <- year2013 %>% mutate(inOffZoneHome = case_when(homezone == "Off" & # Defines only the offzone situations with a leading action by off
#                                                          (lag(homezone, default = "Neu") == "Off" | homeMarker == 1)~ 1,
#                                                          TRUE ~ 0),
#                                 offSeqHome = homeMarker + inOffZoneHome, # =2 then start of zone entry, =1 then in zone, =0 not in zone
#                                 zoneStartHome = case_when(offSeqHome == 2 & lag(offSeqHome) < 2
#                                                      ~ 1,
#                                                      TRUE ~ 0),
#                                 zoneEndHome = case_when(
#                                                     offSeqHome == 0 & lag(offSeqHome, default = 0) > 0
#                                                     ~ 1,
#                                                     TRUE ~ 0),
#                                 betweenEntryAndExitHome = cumsum(zoneStartHome) - cumsum(zoneEndHome),
#                                 zoneNumHome = case_when(
#                                                     offSeqHome > 0 & betweenEntryAndExitHome > 0 ~ cumsum(zoneStartHome),
#                                                     TRUE ~ 0)
#                                 ) 
# 
# # Create a variable for who the shot was against 
#                                 
# year2013 <- year2013 %>% mutate(inOffZoneAway = case_when(homezone == "Def" & 
#                                                           (lag(homezone, default = "Neu") == "Def" | awayMarker == 1) ~ 1,
#                                                           TRUE ~ 0),
#                                 offSeqAway = awayMarker + inOffZoneAway, # =2 then start of zone entry, =1 then in zone, =0 not in zone
#                                 zoneStartAway = case_when(
#                                                           offSeqAway == 2 & lag(offSeqAway, default = 0) < 2
#                                                           ~ 1,
#                                                           TRUE ~ 0),                      
#                                 zoneEndAway = case_when(
#                                                         offSeqAway == 0 & lag(offSeqAway, default = 0) > 0
#                                                         ~ 1,
#                                                         TRUE ~0),
#                                 betweenEntryAndExitAway = cumsum(zoneStartAway) - cumsum(zoneEndAway),
#                                 zoneNumAway = case_when(
#                                                         offSeqAway > 0 & betweenEntryAndExitAway > 0 ~ cumsum(zoneStartAway),
#                                                         TRUE ~ 0))

# test how to combine the two data sets
# homeTeamCols <- homeTeam %>% select(marker, zoneStart, zoneEnd, zoneNum)
# awayTeamCols <- awayTeam %>% select(markerAway, zoneStartAway, zoneEndAway, zoneNumAway)
# fullGame <- game %>% cbind(homeTeamCols) %>% cbind(awayTeamCols)
# View(fullGame)
# # For each game (already separated because of data setup), enumerate the zone attempt we are on
# 
# # Apply the group_by function to zones, now has zones defined. Currently does not register the 0 shot zones in the following sum
# shotsPerZoneHome <- homeTeam %>% group_by(zoneNum) %>% arrange(zoneNum) %>% filter(zoneNum >0) %>% count(etype == "MISS" | etype == "SHOT")
# shotsPerZoneAway <- awayTeam %>% group_by(zoneNumAway) %>% arrange(zoneNumAway) %>% filter(zoneNumAway >0) %>% count(etype == "MISS" | etype == "SHOT")
# 
# # Plot shots against zone number
# library(ggplot2)
# ggplot(data = shotsPerZoneHome[shotsPerZoneHome$`etype == "MISS" | etype == "SHOT"` == TRUE,], aes(x = zoneNum, y = n))+geom_point()
# # Try to improve by adding TRUE n = 0 for those zones that dont have a shot, same for FALSE when zones dont have a non-shot
# 
# 
# # Extend to an automated assignment for all the files read in
#   # First step is to create a function that achieves what was done in above sample
#   zoneEnumerator <- function(game){
#     # Generate marker home columns
#     home <- game %>% as_tibble() %>% mutate(homezone = as.character(homezone),
#                                             hometeam = as.character(hometeam),
#                                             awayteam = as.character(awayteam),
#                                             ev.team  = as.character(ev.team)) %>% 
#                                      mutate(marker = case_when(
#                                               (lag(homezone, default = "Neu") != "Off"
#                                                | (lag(ev.team) != hometeam & lag(homezone, default = "Neu") == "Off"))
#                                               & homezone == "Off"
#                                               & ev.team == hometeam
#                                               ~ 1,
#                                               TRUE ~ 0)) %>% 
#                                      mutate(zoneStart = marker,
#                                                        marker2= case_when(homezone == "Off" & ev.team == hometeam ~ 1, TRUE ~ 0),
#                                                        marker = case_when(
#                                                          marker2 == 1 | # Are we at a zone entry
#                                                            (lag(marker2, default = 0) == 1 & homezone == "Off") # or still in the off zone
#                                                          ~ 1, # Then 1
#                                                          TRUE ~ 0), # Else 0
#                                                        zoneEnd = case_when(
#                                                          marker == 1 & lead(marker, default = 1) == 0
#                                                          ~ 1,
#                                                          TRUE ~ 0),
#                                                        zoneNum = case_when(
#                                                          marker == 1 ~ cumsum(zoneStart),
#                                                          TRUE ~ 0)) %>% select(-marker2) %>% select(marker, zoneStart, zoneEnd, zoneNum)
#     # Generate away marker columns
#     away <- game %>% as_tibble() %>% mutate(homezone = as.character(homezone),
#                                         hometeam = as.character(hometeam),
#                                         awayteam = as.character(awayteam),
#                                         ev.team  = as.character(ev.team)) %>%
#                                      mutate(markerAway = case_when(
#                                         (lag(homezone, default = "Neu") != "Def" | 
#                                            (lag(ev.team) == hometeam & lag(homezone, default = "Neu") == "Def"))
#                                         & homezone == "Def"
#                                         & ev.team != hometeam
#                                         ~ 1,
#                                         TRUE ~ 0)) %>% 
#                                      mutate(zoneStartAway = markerAway,
#                                                marker2Away= case_when(homezone == "Def" & ev.team != hometeam ~ 1, TRUE ~ 0),
#                                                markerAway = case_when(
#                                                  marker2Away == 1 | # Are we at a zone entry
#                                                    (lag(marker2Away, default = 0) == 1 & homezone == "Def") # or still in home team's def zone
#                                                  ~1, # Then 1
#                                                  TRUE ~ 0), # Else 0
#                                                zoneEndAway = case_when(markerAway == 1 & lead(markerAway, default = 1) == 0
#                                                                        ~ 1,
#                                                                        TRUE ~0),
#                                                zoneNumAway = case_when(markerAway == 1 ~ cumsum(zoneStartAway),
#                                                                        TRUE ~ 0)) %>% 
#                                                select(-marker2Away) %>% select(markerAway, zoneStartAway, zoneEndAway, zoneNumAway)
#     # Bind marker columns to game
#     fullGame <- game %>% cbind(home) %>% cbind(away)
#     return(fullGame)
#   }
# 
# # Seems to be functional  
# testSet <- zoneEnumerator(dataFull[[13]])
# 
# # Run it over all datasets now
# gamesWithZones <- lapply(dataFull, zoneEnumerator)
# View(gamesWithZones[13][[1]])
# 
# 
# # Create player stat lines by combining all events involving a player id together
#   # Step one: Duplicate the event for each player in a1-a5 and h1-h5, adding a target player column
# allGames1314 <- data.frame(gamesWithZones[[1]])
# for(i in 2:length(gamesWithZones)){
#   if(mean(gamesWithZones[[i]]$season[1:5], na.rm = T) == 20132014){
#     allGames1314 <- rbind(allGames1314, gamesWithZones[[i]])
#   }
# }
# # Actually this is easier, arrange and group_by
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# allGames1314 <- allGames1314 %>% arrange(ev.player.1) %>% group_by(ev.player.1, gcode) %>% filter(ev.player.1 > 1) %>% select(-(adjusted.distance:awaycapt.inevent)) 
# summary1314 <- allGames1314 %>% summarize(goals = sum(etype == "GOAL"),
#          shotsAtGoal = sum(etype == "SHOT" | etype == "MISS"),
#          shotsOnGoal = sum(etype == "SHOT"),
#          accuracy = shotsOnGoal / (shotsAtGoal + shotsOnGoal),
#          goalPerc = goals / shotsOnGoal,
#          hits = sum(etype == "HIT"),
#          blocks = sum(etype == "BLOCK"),
#          penalties = sum(etype == "PENL"),
#          season = getmode(season),
#          team = first(ev.team))
# # How do I measure assists?
# 
# allGames1415 <- data.frame()
# for(i in 1:length(gamesWithZones)){
#   if(mean(gamesWithZones[[i]]$season[1:5], na.rm = T) == 20142015){
#     allGames1415 <- rbind(allGames1415, gamesWithZones[[i]])
#   }
# }
# allGames1415 <- allGames1415 %>% arrange(ev.player.1) %>% group_by(ev.player.1) %>% filter(ev.player.1 > 1) %>% select(-(adjusted.distance:awaycapt.inevent))
# summary1415 <- allGames1415 %>% summarize(goals = sum(etype == "GOAL"),
#                                           shotsAtGoal = sum(etype == "SHOT" | etype == "MISS"),
#                                           shotsOnGoal = sum(etype == "SHOT"),
#                                           accuracy = shotsOnGoal / (shotsAtGoal + shotsOnGoal),
#                                           goalPerc = goals / shotsOnGoal,
#                                           hits = sum(etype == "HIT"),
#                                           blocks = sum(etype == "BLOCK"),
#                                           penalties = sum(etype == "PENL"),
#                                           season = getmode(season),
#                                           team = first(ev.team))
# 
# allGames1516 <- data.frame(gamesWithZones[[1]])
# for(i in 2:length(gamesWithZones)){
#   if(mean(gamesWithZones[[i]]$season[1:5], na.rm = T) == 20152016){
#     allGames1516 <- rbind(allGames1516, gamesWithZones[[i]])
#   }
# }
# allGames1516 <- allGames1516 %>% arrange(ev.player.1) %>% group_by(ev.player.1) %>% filter(ev.player.1 > 1) %>% select(-(adjusted.distance:awaycapt.inevent))
# summary1516 <- allGames1516 %>% summarize(goals = sum(etype == "GOAL"),
#                                           shotsAtGoal = sum(etype == "SHOT" | etype == "MISS"),
#                                           shotsOnGoal = sum(etype == "SHOT"),
#                                           accuracy = shotsOnGoal / (shotsAtGoal + shotsOnGoal),
#                                           goalPerc = goals / shotsOnGoal,
#                                           hits = sum(etype == "HIT"),
#                                           blocks = sum(etype == "BLOCK"),
#                                           penalties = sum(etype == "PENL"),
#                                           season = getmode(season),
#                                           team = first(ev.team))
# 
# allGames1617 <- data.frame(gamesWithZones[[1]])
# for(i in 2:length(gamesWithZones)){
#   if(mean(gamesWithZones[[i]]$season[1:5], na.rm = T) == 20162017){
#     allGames1617 <- rbind(allGames1617, gamesWithZones[[i]])
#   }
# }
# allGames1617 <- allGames1617 %>% arrange(ev.player.1) %>% group_by(ev.player.1) %>% filter(ev.player.1 > 1) %>% select(-(adjusted.distance:awaycapt.inevent))
# summary1617 <- allGames1617 %>% summarize(goals = sum(etype == "GOAL"),
#                                           shotsAtGoal = sum(etype == "SHOT" | etype == "MISS"),
#                                           shotsOnGoal = sum(etype == "SHOT"),
#                                           accuracy = shotsOnGoal / (shotsAtGoal + shotsOnGoal),
#                                           goalPerc = goals / shotsOnGoal,
#                                           hits = sum(etype == "HIT"),
#                                           blocks = sum(etype == "BLOCK"),
#                                           penalties = sum(etype == "PENL"),
#                                           season = getmode(season),
#                                           team = first(ev.team))
# 
# fullSummary <- rbind(summary1314, summary1415, summary1516, summary1617) %>% arrange(team, ev.player.1)
# View(fullSummary) # Jason Chimera is player ID 5
# 
# 
