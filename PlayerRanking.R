# JSM Project
# Reading in Data

library(tidyverse)
library(cluster)

setwd("C:/Users/Brad_/Desktop/SFU/JSM/Kevin Capt")

filenames = Sys.glob("*.csv")
data = lapply(filenames, read.csv)

# If want to make one massive dataframe
merged = Reduce(rbind, contents)


# Data Exploration
str(data[1])
data[1]
View(data[1])

setwd("C:/Users/Brad_/Desktop/SFU/JSM/Salary Data 1")

# Salary Data Cleaning

# Season 2013/14
Salaries_13_14 <- read.csv("Salaries_2013_2014.csv", header = TRUE)

# Use only players with compmplete data
Salaries_13_14 <- Salaries_13_14[complete.cases(Salaries_13_14), ]


# Rename the columns of the dataframe
colnames(Salaries_13_14) <- c("Name", "Pos", "Team.13", "GamesPlayed.13",
                              "Goals.13", "Assists.13", "Points.13", "SCH.13",
                              "GoalsPerSCH.13","AssistsPerSCH.13", "PointsPerSCH.13")

# Change salary cap to numeric, remove $ sign and commas
Salaries_13_14$SCH.13 = as.numeric(gsub("[\\$,]", "", Salaries_13_14$SCH.13))

# Season 2014/15
Salaries_14_15 <- read.csv("Salaries_2014_2015.csv", header = TRUE)
Salaries_14_15 <- Salaries_14_15[complete.cases(Salaries_14_15), ]

colnames(Salaries_14_15) <- c("Name", "Pos", "Team.14", "GamesPlayed.14",
                              "Goals.14", "Assists.14", "Points.14", "SCH.14",
                              "GoalsPerSCH.14","AssistsPerSCH.14", "PointsPerSCH.14")

Salaries_14_15$SCH.14 = as.numeric(gsub("[\\$,]", "", Salaries_14_15$SCH.14))

# Season 2015/16
Salaries_15_16 <- read.csv("Salaries_2015_2016.csv", header = TRUE)
Salaries_15_16 <- Salaries_15_16[complete.cases(Salaries_15_16), ]

colnames(Salaries_15_16) <- c("Name", "Pos", "Team.15", "GamesPlayed.15",
                              "Goals.15", "Assists.15", "Points.15", "SCH.15",
                              "GoalsPerSCH.15","AssistsPerSCH.15", "PointsPerSCH.15")

Salaries_15_16$SCH.15 = as.numeric(gsub("[\\$,]", "", Salaries_15_16$SCH.15))

# Season 2016/17
Salaries_16_17 <- read.csv("Salaries_2016_2017.csv", header = TRUE)
Salaries_16_17 <- Salaries_16_17[complete.cases(Salaries_16_17), ]


colnames(Salaries_16_17) <- c("Name", "Pos", "Team.16", "GamesPlayed.16",
                              "Goals.16", "Assists.16", "Points.16", "SCH.16",
                              "GoalsPerSCH.16","AssistsPerSCH.16", "PointsPerSCH.16")

Salaries_16_17$SCH.16 = as.numeric(gsub("[\\$,]", "", Salaries_16_17$SCH.16))


# Clustering
# Separate players out by position and year


# Defense players
Def_13_14 <- filter(Salaries_13_14, Salaries_13_14$Pos == "RD" | Salaries_13_14$Pos == "LD" | Salaries_13_14$Pos == "D")
Def_14_15 <- filter(Salaries_14_15, Salaries_14_15$Pos == "RD" | Salaries_14_15$Pos == "LD" | Salaries_14_15$Pos == "D")
Def_15_16 <- filter(Salaries_15_16, Salaries_15_16$Pos == "RD" | Salaries_15_16$Pos == "LD" | Salaries_15_16$Pos == "D")
Def_16_17 <- filter(Salaries_16_17, Salaries_16_17$Pos == "RD" | Salaries_16_17$Pos == "LD" | Salaries_16_17$Pos == "D")

# Offense players
Off_13_14 <- filter(Salaries_13_14, Salaries_13_14$Pos == "RW" | Salaries_13_14$Pos == "LW" | Salaries_13_14$Pos == "C")
Off_14_15 <- filter(Salaries_14_15, Salaries_14_15$Pos == "RW" | Salaries_14_15$Pos == "LW" | Salaries_14_15$Pos == "C")
Off_15_16 <- filter(Salaries_15_16, Salaries_15_16$Pos == "RW" | Salaries_15_16$Pos == "LW" | Salaries_15_16$Pos == "C")
Off_16_17 <- filter(Salaries_16_17, Salaries_16_17$Pos == "RW" | Salaries_16_17$Pos == "LW" | Salaries_16_17$Pos == "C")


?kmeans()
names(Off_13_14)
View(Off_13_14)
str(Off_13_14$SCH.13)

# Standardize Data


# Offense Clusters

set.seed(1234)
clusters_Def_13_14 <- kmeans(data.frame(scale(Def_13_14$Points.13), scale(Def_13_14$SCH.13)), centers = 4)
clusters_Def_14_15 <- kmeans(data.frame(scale(Def_14_15$Points.14), scale(Def_14_15$SCH.14)), centers = 4)
clusters_Def_15_16 <- kmeans(data.frame(scale(Def_15_16$Points.15), scale(Def_15_16$SCH.15)), centers = 4)
clusters_Def_16_17 <- kmeans(data.frame(scale(Def_16_17$Points.16), scale(Def_16_17$SCH.16)), centers = 4)

# Offense Clusters
clusters_Off_13_14 <- kmeans(data.frame(scale(Off_13_14$Points.13), scale(Off_13_14$SCH.13)), centers = 4)
clusters_Off_14_15 <- kmeans(data.frame(scale(Off_14_15$Points.14), scale(Off_14_15$SCH.14)), centers = 4)
clusters_Off_15_16 <- kmeans(data.frame(scale(Off_15_16$Points.15), scale(Off_15_16$SCH.15)), centers = 4)
clusters_Off_16_17 <- kmeans(data.frame(scale(Off_16_17$Points.16), scale(Off_16_17$SCH.16)), centers = 4)

# Recombining on the Dataframes

# Defense Players 

Def_13_14 <- cbind(Def_13_14, clusters_Def_13_14$cluster)
colnames(Def_13_14)[12] = c("Cluster")

Def_14_15 <- cbind(Def_14_15, clusters_Def_14_15$cluster)
colnames(Def_14_15)[12] = c("Cluster")

Def_15_16 <- cbind(Def_15_16, clusters_Def_15_16$cluster)
colnames(Def_15_16)[12] = c("Cluster")

Def_16_17 <- cbind(Def_16_17, clusters_Def_16_17$cluster)
colnames(Def_16_17)[12] = c("Cluster")

# Offense Players

Off_13_14 <- cbind(Off_13_14, clusters_Off_13_14$cluster)
colnames(Off_13_14)[12] = c("Cluster")

Off_14_15 <- cbind(Off_14_15, clusters_Off_14_15$cluster)
colnames(Off_14_15)[12] = c("Cluster")

Off_15_16 <- cbind(Off_15_16, clusters_Off_15_16$cluster)
colnames(Off_15_16)[12] = c("Cluster")

Off_16_17 <- cbind(Off_16_17, clusters_Off_16_17$cluster)
colnames(Off_16_17)[12] = c("Cluster")



str(Def_13_14)
str(Def_14_15)
str(Def_15_16)
str(Def_16_17)

str(Off_13_14)
str(Off_14_15)
str(Off_15_16)
str(Off_16_17)


####### Cross Cluster Ranking System

### Defensive Players

# Clusters names are not the same across all clusters.  Best players are not always cluster 1, etc.  
# Assigns correct rankings to each player based on their cluster.  
# Best players ranked A, worst players ranked D.
# 

# Season 2013/14

View(Def_13_14)
# Goes in order 1 2 4 3

Skill_Level_13_14 <- c()
for(i in 1:nrow(Def_13_14)){
  if(Def_13_14[i,12] == 1){
    Skill_Level_13_14[i] = "A"
  }else if(Def_13_14[i, 12] == 2){
    Skill_Level_13_14[i] = "B"
  }else if(Def_13_14[i, 12] == 4){
    Skill_Level_13_14[i] = "C"
  }else if(Def_13_14[i, 12] == 3){
    Skill_Level_13_14[i] = "D"
  }
}  
Skill_Level_13_14

# Season 2014/15

View(Def_14_15)
# Goes in order 3 2 4 1

Skill_Level_14_15 <- c()
for(i in 1:nrow(Def_14_15)){
  if(Def_14_15[i,12] == 3){
    Skill_Level_14_15[i] = "A"
  }else if(Def_14_15[i, 12] == 2){
    Skill_Level_14_15[i] = "B"
  }else if(Def_14_15[i, 12] == 4){
    Skill_Level_14_15[i] = "C"
  }else if(Def_14_15[i, 12] == 1){
    Skill_Level_14_15[i] = "D"
  }
}  
Skill_Level_14_15

# Season 2015/16

View(Def_15_16)
# Goes in order 2 4 3 1

Skill_Level_15_16 <- c()
for(i in 1:nrow(Def_15_16)){
  if(Def_15_16[i,12] == 2){
    Skill_Level_15_16[i] = "A"
  }else if(Def_15_16[i, 12] == 4){
    Skill_Level_15_16[i] = "B"
  }else if(Def_15_16[i, 12] == 3){
    Skill_Level_15_16[i] = "C"
  }else if(Def_15_16[i, 12] == 1){
    Skill_Level_15_16[i] = "D"
  }
}  
Skill_Level_15_16

# Season 2016/17

View(Def_16_17)
# Goes in order 3 1 2 4

Skill_Level_16_17 <- c()
for(i in 1:nrow(Def_16_17)){
  if(Def_16_17[i,12] == 3){
    Skill_Level_16_17[i] = "A"
  }else if(Def_16_17[i, 12] == 1){
    Skill_Level_16_17[i] = "B"
  }else if(Def_16_17[i, 12] == 2){
    Skill_Level_16_17[i] = "C"
  }else if(Def_16_17[i, 12] == 4){
    Skill_Level_16_17[i] = "D"
  }
}  
Skill_Level_16_17

### Offense Players Recombine and Ranking

# 2013 - 2014

View(Off_13_14)

# 4 3 2 1

Skill_Level_Off_13_14 <- c()
for(i in 1:nrow(Off_13_14)){
  if(Off_13_14[i,12] == 4){
    Skill_Level_Off_13_14[i] = "A"
  }else if(Off_13_14[i, 12] == 3){
    Skill_Level_Off_13_14[i] = "B"
  }else if(Off_13_14[i, 12] == 2){
    Skill_Level_Off_13_14[i] = "C"
  }else if(Off_13_14[i, 12] == 1){
    Skill_Level_Off_13_14[i] = "D"
  }
}  
Skill_Level_Off_13_14

# Season 2014/15

View(Off_14_15)
# Goes in order 4 3 2 1

Skill_Level_Off_14_15 <- c()
for(i in 1:nrow(Off_14_15)){
  if(Off_14_15[i,12] == 4){
    Skill_Level_Off_14_15[i] = "A"
  }else if(Off_14_15[i, 12] == 3){
    Skill_Level_Off_14_15[i] = "B"
  }else if(Off_14_15[i, 12] == 2){
    Skill_Level_Off_14_15[i] = "C"
  }else if(Off_14_15[i, 12] == 1){
    Skill_Level_Off_14_15[i] = "D"
  }
}  
Skill_Level_Off_14_15

# Season 2015/16

View(Off_15_16)
# Goes in order 3 1 4 2

Skill_Level_Off_15_16 <- c()
for(i in 1:nrow(Off_15_16)){
  if(Off_15_16[i,12] == 3){
    Skill_Level_Off_15_16[i] = "A"
  }else if(Off_15_16[i, 12] == 1){
    Skill_Level_Off_15_16[i] = "B"
  }else if(Off_15_16[i, 12] == 4){
    Skill_Level_Off_15_16[i] = "C"
  }else if(Off_15_16[i, 12] == 2){
    Skill_Level_Off_15_16[i] = "D"
  }
}  
Skill_Level_Off_15_16

# Season 2016/17

View(Off_16_17)
# Goes in order 1 2 4 3

Skill_Level_Off_16_17 <- c()
for(i in 1:nrow(Off_16_17)){
  if(Off_16_17[i,12] == 1){
    Skill_Level_Off_16_17[i] = "A"
  }else if(Off_16_17[i, 12] == 2){
    Skill_Level_Off_16_17[i] = "B"
  }else if(Off_16_17[i, 12] == 4){
    Skill_Level_Off_16_17[i] = "C"
  }else if(Off_16_17[i, 12] == 3){
    Skill_Level_Off_16_17[i] = "D"
  }
}  
Skill_Level_Off_16_17

# Recombine on Data


Def_13_14 <- cbind(Def_13_14, Skill_Level_13_14)
Def_14_15 <- cbind(Def_14_15, Skill_Level_14_15)
Def_15_16 <- cbind(Def_15_16, Skill_Level_15_16)
Def_16_17 <- cbind(Def_16_17, Skill_Level_16_17)

Off_13_14 <- cbind(Off_13_14, Skill_Level_Off_13_14)
Off_14_15 <- cbind(Off_14_15, Skill_Level_Off_14_15)
Off_15_16 <- cbind(Off_15_16, Skill_Level_Off_15_16)
Off_16_17 <- cbind(Off_16_17, Skill_Level_Off_16_17)

colnames(Def_13_14)[12] <- c("Skill Rank")
colnames(Def_14_15)[12] <- c("Skill Rank")
colnames(Def_15_16)[12] <- c("Skill Rank")
colnames(Def_16_17)[12] <- c("Skill Rank")
colnames(Off_13_14)[12] <- c("Skill Rank")
colnames(Off_14_15)[12] <- c("Skill Rank")
colnames(Off_15_16)[12] <- c("Skill Rank")
colnames(Off_16_17)[12] <- c("Skill Rank")


# Recombine Into Seasons

dim(Def_13_14)
dim(Off_13_14)

str(Def_13_14)
str(Off_13_14)

Def_13_14 <- Def_13_14[, -c(12)]
Def_14_15 <- Def_14_15[, -c(12)]
Def_15_16 <- Def_15_16[, -c(12)]
Def_16_17 <- Def_16_17[, -c(12)]

Off_13_14 <- Off_13_14[, -c(12, 13)]
Off_14_15 <- Off_14_15[, -c(12, 13)]
Off_15_16 <- Off_15_16[, -c(12, 13)]
Off_16_17 <- Off_16_17[, -c(12, 13)]

Players_13_14 <- rbind(Off_13_14, Def_13_14)
Players_14_15 <- rbind(Off_14_15, Def_14_15)
Players_15_16 <- rbind(Off_15_16, Def_15_16)
Players_16_17 <- rbind(Off_16_17, Def_16_17)

# Add Year and remove year indicators on other variable names

help("rep")
Season <- rep("2016/2017", nrow(Players_16_17)) 

Players_16_17 <- cbind(Players_16_17, Season)
names(Players_14_15)
colnames(Players_16_17) <- c("Name", "Pos", "Team", "GamesPlayed",
                              "Goals", "Assists", "Points", "SCH",
                              "GoalsPerSCH","AssistsPerSCH", "PointsPerSCH", "SkillRank", "Season")


PlayersCombined <- rbind(Players_13_14, Players_14_15, Players_15_16, Players_16_17)

write.csv(PlayersCombined, "Player Rankings.csv")


#################################################################################################################################
#####                                               Fuzzy Clustering                                                        #####
#################################################################################################################################


install.packages("fclust")
install.packages("ppclust")
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)


?Fclust()

Def_13_14 <- filter(Salaries_13_14, Salaries_13_14$Pos == "RD" | Salaries_13_14$Pos == "LD" | Salaries_13_14$Pos == "D")
Def_14_15 <- filter(Salaries_14_15, Salaries_14_15$Pos == "RD" | Salaries_14_15$Pos == "LD" | Salaries_14_15$Pos == "D")
Def_15_16 <- filter(Salaries_15_16, Salaries_15_16$Pos == "RD" | Salaries_15_16$Pos == "LD" | Salaries_15_16$Pos == "D")
Def_16_17 <- filter(Salaries_16_17, Salaries_16_17$Pos == "RD" | Salaries_16_17$Pos == "LD" | Salaries_16_17$Pos == "D")

Off_13_14 <- filter(Salaries_13_14, Salaries_13_14$Pos == "RW" | Salaries_13_14$Pos == "LW" | Salaries_13_14$Pos == "C")
Off_14_15 <- filter(Salaries_14_15, Salaries_14_15$Pos == "RW" | Salaries_14_15$Pos == "LW" | Salaries_14_15$Pos == "C")
Off_15_16 <- filter(Salaries_15_16, Salaries_15_16$Pos == "RW" | Salaries_15_16$Pos == "LW" | Salaries_15_16$Pos == "C")
Off_16_17 <- filter(Salaries_16_17, Salaries_16_17$Pos == "RW" | Salaries_16_17$Pos == "LW" | Salaries_16_17$Pos == "C")

str(Def_13_14)

test_cluster <- Fclust(data.matrix(Def_13_14[,c(4:11)]))


?fcm()
res.fcm <- fcm(data.matrix(Def_13_14[,c(4:11)]), centers=4)
res.fcm$u

plotcluster(res.fcm, cp=1, trans=TRUE)

res.fcm2 <- fcm(data.matrix(Def_13_14[,c(7,8,11)]), centers = 4)
res.fcm2$u

plotcluster(res.fcm2, cp = 1, trans=TRUE)



### Reading in Matt's Data


load("C:/Users/Brad_/Desktop/SFU/JSM/Jack Test/testCase/nhlscrapr-20132014.RData")
load("C:/Users/Brad_/Desktop/SFU/JSM/Jack Test/testCase/nhlscrapr-20142015.RData")
load("C:/Users/Brad_/Desktop/SFU/JSM/Jack Test/testCase/nhlscrapr-core.RData")

str(games)
str(grand.data)
str(roster.master)


# The Roster

View(roster.master)

