# Base exploration of the data provided by Jack Davis

# Libraries
library(tidyr)
library(dplyr)

# Loading data example
# Requires the path ~\github\Goalie-Fatigue\Data
sampleData <- read.csv("~/GitHub/Goalie-Fatigue/Data/NHL 20001 20132014 timeout and capt 2017-09-01.csv")

# Way to iterate
# First value: Gets repeated 4 times, one for each of the year combos
fv <- rep(seq(20001, 21230), times = 1, each = 4) %>% as.character()

# Second value: Year combos
sv <- c("20132014", "20142015", "20152016", "20162017") %>% rep(times = length(fv) / 4)

# Combine fv and sv
yearIter <- paste0("~/GitHub/Goalie-Fatigue/Data/NHL ", fv, " ", sv, " timeout and capt 2017-09-01.csv")

# Validate
dataTest <- read.csv(yearIter[1]) # Works as intended

# Full data set, easier way like what Brad did
files <- list.files("Data", pattern = "*.csv", full.names = TRUE)
dataFull <- lapply(files, read.csv)

# Beginning of the data work
head(dataFull)
head(dataFull[[1]]$homezone)

# Choose an arbitrary game to test methods, then apply to all with lapply
game <- dataFull[[13]] %>% as_tibble() %>% mutate(homezone = as.character(homezone),
                                                  hometeam = as.character(hometeam),
                                                  awayteam = as.character(awayteam),
                                                  ev.team  = as.character(ev.team))

# Need to create a marker for zone continuous
  # homeTeam: Still needs tidying but currently checks prev event for diff zone, then curr ev for off zone for home team and their event
  homeTeam <- game %>% mutate(marker = case_when(
                                          (lag(homezone, default = "Neu") != "Off"
                                           | lag(ev.team) != hometeam)
                                          & homezone == "Off"
                                          & ev.team == hometeam
                                          ~ 1,
                                          TRUE ~ 0))

  # awayTeam: Should be play is in neutral or home team's offensive zone (away team's defence) then changes to away team action in home team's def zone
  awayTeam <- game %>% mutate(markerAway = case_when(
                                                     (lag(homezone, default = "Neu") != "Def" | lag(ev.team) == hometeam)
                                                     & homezone == "Def"
                                                     & ev.team != hometeam
                                                     ~ 1,
                                                     TRUE ~ 0))

  # Might be easier to make general marker and then use filter commands


# Now to create sequences of zone actions, based on either home or away
homeTeam <- homeTeam %>% mutate(zoneStart = marker,
                                marker2= case_when(homezone == "Off" & ev.team == hometeam ~ 1, TRUE ~ 0),
                                marker = case_when(
                                                       marker2 == 1 | # Are we at a zone entry
                                                       (lag(marker2, default = 0) == 1 & homezone == "Off") # or still in the off zone
                                                       ~ 1, # Then 1
                                                       TRUE ~ 0), # Else 0
                                zoneEnd = case_when(
                                                    marker == 1 & lead(marker, default = 1) == 0
                                                    ~ 1,
                                                    TRUE ~ 0),
                                zoneNum = case_when(
                                                    marker == 1 ~ cumsum(zoneStart),
                                                    TRUE ~ 0)
                                ) %>% select(-marker2)
                                
awayTeam <- awayTeam %>% mutate(zoneStartAway = markerAway,
                                marker2Away= case_when(homezone == "Def" & ev.team != hometeam ~ 1, TRUE ~ 0),
                                markerAway = case_when(
                                                        marker2Away == 1 | # Are we at a zone entry
                                                        (lag(marker2Away, default = 0) == 1 & homezone == "Def") # or still in home team's def zone
                                                        ~1, # Then 1
                                                        TRUE ~ 0), # Else 0
                                zoneEndAway = case_when(markerAway == 1 & lead(markerAway, default = 1) == 0
                                                        ~ 1,
                                                        TRUE ~0),
                                zoneNumAway = case_when(markerAway == 1 ~ cumsum(zoneStartAway),
                                                        TRUE ~ 0)) %>% select(-marker2Away)

# For each game (already separated because of data setup), enumerate the zone attempt we are on

