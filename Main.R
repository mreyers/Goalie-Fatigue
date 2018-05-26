# Base exploration of the data provided by Jack Davis

# Libraries
library(tidyr)

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


# Beginning of the data work
View(sampleData)

