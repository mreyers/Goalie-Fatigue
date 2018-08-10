filenames = Sys.glob("*.csv")
data = lapply(filenames[1:50], read.csv, as.is = T, header = T, strip.white = T)
data2 <- load(filenames[1])
library(tidyverse)

# Loading in data
hockey1 <- data[[1]]

# Looking at data
str(hockey1)

head(hockey1)

unique(hockey1$type)

# Filtering tip ins
hockey2 <- hockey1 %>% filter(etype %in% c("SHOT", "GOAL"))

# Pulling out the shot
hockey3 <- hockey2 %>% dplyr::select(seconds, distance, etype, type)

# looking at plot
ggplot(data = hockey3, aes(x = seconds, y = distance, color = etype, shape = type)) + geom_point()

# looking at proportion of shots that were taken
table(hockey3$type)

# So, all the goals scored in this game were wrist shots but we had 55 so, need more data
# to figure out the best measure, then we can figure out how much weighting should be given to
# type of shot (need to further split somehow on clustering of players)



View(hockey1)

#### Action Plan ####

# 1. Find a way to cluster players
# 2. Once clustered, decide what determines the shot quality

### Loading in all games at once
allGames <- NULL
for (i in 1:length(data)) {
  allGames <- rbind(allGames, data[[i]])
}

str(allGames)

library(tidyverse)

# Filtering tip ins
allGames2 <- allGames %>% filter(etype %in% c("GOAL", "SHOT"))

# Pulling out the shot
allGames3 <- allGames2 %>% dplyr::select(seconds, distance, etype, type)

allgames_shots <- allGames2 %>% filter(etype == "SHOT")  %>% dplyr::select(seconds, distance, etype, type)
allgames_goals <- allGames2 %>% filter(etype == "GOAL")  %>% dplyr::select(seconds, distance, etype, type)

# Looking at all games 3 and seeing if regression means anything
str(allGames2)

allGames2$etype <- as.factor(allGames2$etype)
allGames2$period <- as.factor(allGames2$period)
allGames2$type <- as.factor(allGames2$type)
allGames2$etype

# Linear model
nhl_model1 <- glm(etype ~ seconds + as.factor(period) + distance + as.factor(type) + xcoord + ycoord, 
                  data = allGames2, family = "binomial")

# Predicting
pred1 <- predict(nhl_model1, newdata = allGames2, type = "response")

# Editing
pred1 <- ifelse(pred1 > 0.8, "SHOT", "GOAL")

# Comaparing
sum(as.vector(pred1)[-31] == as.character(allGames2$etype)[-31])/(length(pred1) - 1)

# Looking at plot
pred_data <- data.frame(real = as.numeric(allGames2$etype), pred = as.numeric(as.factor(pred1)))[-31,]
ggplot(data = pred_data, aes(x = 1:74, y = pred + real)) + geom_point()

# Looking at summary
summary(nhl_model1)

### Need another way to consider coefficients
angleMaker <- function(data){
  dataShots <- filter(data, data$etype == "SHOT" | data$etype == "GOAL")
  Radians <- c()
  dataShots <- dataShots[!is.na(dataShots$xcoord),]
  for(i in 1:nrow(dataShots)){
    #if(dataShots$xcoord <= 0 & dataShots$ycoord <= 0){
    #Distance from center to end, minus end to net, minus abs of xcoord
    xdist <- abs(100 - 11 - abs(dataShots$xcoord[i]))
    ydist <- abs(dataShots$ycoord[i])
    Radians[i] = atan(ydist/xdist)
    
  }
  data2 = data[!is.na(data$xcoord),]
  data2$Angle = (180*Radians)/3.14159
  return(data2)  
}

str(allGames2)
temp <- angleMaker(allGames2)

# Rerunning glm with angle instead
nhl_model2 <- glm(etype ~ distance + as.factor(type) + Angle, 
                  data = temp, family = "binomial")

# Looking at resylt
summary(nhl_model2)

# Predicting
pred1 <- predict(nhl_model2, newdata = temp, type = "response")

# Editing
pred1 <- ifelse(pred1 > 0.8, "SHOT", "GOAL")

# Comaparing
sum(as.vector(pred1) == as.character(temp$etype))/(length(pred1) - 1)

# Looking at plot
pred_data <- data.frame(real = as.numeric(temp$etype), pred = as.numeric(as.factor(pred1)))
ggplot(data = pred_data, aes(x = 1:nrow(pred_data), y = pred + real)) + geom_point()

plot(nhl_model2)

# Going over residuals
plot(predict(nhl_model2), residuals(nhl_model2), col=c("blue","red")[as.numeric(temp$etype)])
abline(h=0,lty=2,col="grey")
lines(lowess(predict(nhl_model2),residuals(nhl_model2)),col="black",lwd=2)

rl=lm(residuals(nhl_model2) ~ bs(predict(nhl_model2),8))
rl=loess(residuals(nhl_model2)~predict(nhl_model2))
y=predict(rl,se=TRUE)
segments(predict(nhl_model2), y$fit+2*y$se.fit, predict(nhl_model2), y$fit-2*y$se.fit, col="green")

# Figuring out variable importance
nhl_model3 <- glm(etype ~ distance + as.factor(type) + Angle, 
                  data = temp, family = "binomial")

weights(nhl_model2)

gbmImp <- varImp(nhl_model2, scale = FALSE)

##################### FOllowing Book ###########################

# Splitting data
library(caret)
set.seed(998)
inTraining <- createDataPartition(temp$etype, p = .60, list = FALSE)
training <- temp[ inTraining,]
testing  <- temp[-inTraining,]

# Creating CV
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Practice model
gbmFit1 <- train(etype ~ distance + Angle, data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

gbmFit3 <- train(etype ~ distance, data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

# Results: Gradient Boosting Machine
gbmFit1
gbmFit3

# Optimizing parameters
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(etype ~ distance + Angle, data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2

# Comparing tuning parameter results
trellis.par.set(caretTheme())
plot(gbmFit2) 

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(825)
gbmFit4 <- train(etype ~ distance + Angle, data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")
gbmFit4

trellis.par.set(caretTheme())
plot(gbmFit4) 

# making predictions
dist_pred <- predict(gbmFit3, newdata = testing)
new_pred <- predict(gbmFit1, newdata = testing)
new_pred2 <- predict(gbmFit4, newdata = testing)

# Comparing results
mean(dist_pred == testing$etype)
mean(new_pred == testing$etype)
mean(new_pred2 == testing$etype)

# Testing to see issue
gbmFit5 <- train(etype ~ event, data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

new_pred3 <- predict(gbmFit5, newdata = testing)
mean(new_pred3 == testing$etype)

########################## Trying another classifier

# Splitting data
inTraining <- createDataPartition(temp$etype, p = .65, list = FALSE)
training <- temp[ inTraining,]
testing  <- temp[-inTraining,]

# Creating CV
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Bagged ADABoost
baggedAdaFit1 <- train(etype ~ distance + Angle, data = training, 
                 method = "AdaBag", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

baggedAdaFit2 <- train(etype ~ distance, data = training, 
                       method = "AdaBag", 
                       trControl = fitControl,
                       ## This last option is actually one
                       ## for gbm() that passes through
                       verbose = FALSE)

# Predictions
ada_dist_pred <- predict(baggedAdaFit2, newdata = testing)
ada_angledist_pred <- predict(baggedAdaFit1, newdata = testing)

mean(ada_dist_pred == testing$etype)
mean(ada_angledist_pred == testing$etype)

########################## Trying another classifier again

# Splitting data
inTraining <- createDataPartition(temp$etype, p = .65, list = FALSE)
training <- temp[ inTraining,]
testing  <- temp[-inTraining,]

# Creating CV
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

grid <- expand.grid(n.trees = seq(5000,1000000,5000), 
                    interaction.depth = 2, shrinkage = .001, n.minobsinnode = 20)

# Bagged ADABoost
wrapperFit1 <- train(etype ~ distance + Angle, data = training, 
                       method = "bagEarth", 
                       trControl = fitControl,
                       ## This last option is actually one
                       ## for gbm() that passes through
                       verbose = FALSE)

wrapperFit2 <- train(etype ~ distance, data = training, 
                       method = "bagEarth", 
                       trControl = fitControl,
                       ## This last option is actually one
                       ## for gbm() that passes through
                       verbose = FALSE)

# Predictions
treebag_dist_pred <- predict(wrapperFit2, newdata = testing)
treebag_angledist_pred <- predict(wrapperFit1, newdata = testing, type = "prob")

mean(treebag_dist_pred == testing$etype)
mean(treebag_angledist_pred == testing$etype)

str(temp)

##########################

ggplot(data = testing, aes(x = testing$distance, y = treebag_angledist_pred$SHOT)) + geom_point()

ggplot(data = testing, aes(x = 1:nrow(testing), y = distance, color = etype)) + geom_point() + theme_minimal()
ggplot(data = testing, aes(x = 1:nrow(testing), y = Angle, color = etype)) + geom_point() + theme_minimal()

ao <- glm(etype ~ distance + Angle, data = training, family = "binomial")
summary(ao)

probs <- predict(ao, newdata = testing, type = "response")
head(probs)
testing$probs <- probs
ggplot(data = testing, aes(x = distance, y = probs, color = etype)) + geom_point() +theme_minimal() +
  xlab("Distance from the Net") + ylab("Probability of a Save\n Logistic Regression") +
  ggtitle("Distance vs. Probability of a Save") + theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Result")) + scale_color_manual(values = c("blue", "green"))

ggplot(data = testing, aes(x = Angle, y = probs, color = etype)) + geom_point() +theme_minimal() +
  xlab("Angle") + ylab("Probability of a Save\n Logistic Regression") +
  ggtitle("Angle vs. Probability of a Save") + theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Result")) + scale_color_manual(values = c("blue", "green"))

####################


 ###################

# looking at plot
ggplot(data = allGames3, aes(x = seconds, y = distance, color = type)) + geom_point()

# looking at proportion of shots that were taken, relative to the goals scored
table(allgames_goals$type)/(table(allgames_shots$type[allgames_shots$type != "Wrap"]) + table(allgames_goals$type))

plot(x = allGames2$xcoord[1:50], y = allGames2$ycoord[1:50])

ggplot(data = allGames2[1:33,], aes(x = xcoord, y = ycoord, color = ev.team)) + geom_point()


tail(allGames2[1:35,])

str(temp)

temp$time_cat <- cut(temp$seconds, seq(0, 3600, 60))
temp2 <- temp %>% filter(etype == "GOAL")


time_cat_count <- temp %>% group_by(time_cat) %>% summarise(prop_goals = length(time_cat))
hist(time_cat_count$prop_goals)

ggplot(data = time_cat_count, aes(x = time(prop_goals), y = prop_goals)) + geom_line(color = "green") + geom_smooth() +
  theme_minimal() + xlab("Time (Seconds Bin'd into Single Minutes)") + 
  ylab("Proportion of Goals/Shots") + ggtitle("Proportion of Goals Per Minute Across NHL Games") +
  theme(plot.title = element_text(hjust = 0.5))

################

#### Working on HeatMap
temp$dist_cat <- cut(temp$distance, seq(0, 70, 10))
temp$angle_cat <- cut(temp$Angle, seq(0, 90, 30))

heatmap_daa <- temp %>% mutate(goal_or_not = ifelse(etype == "GOAL", 1, 0)) %>% group_by(dist_cat, angle_cat) %>%
  summarise(prop_goals = sum(goal_or_not)/length(goal_or_not))

head(heatmap_daa)
heatmap2 <- na.omit(data.frame(heatmap_daa))

range(temp$Angle)

ggplot(data = heatmap2, aes(x = angle_cat, y = dist_cat)) + geom_tile(aes(alpha = prop_goals),
                                                                      color = "red") +
  theme_minimal() 

heatmap2

dist1 <- c(0.136, 0.178, 0.28)

heat_map_hockey <- ggplot(data = heatmap2, aes(dist_cat, angle_cat, fill = prop_goals)) +
  geom_tile(color = "black") + scale_fill_gradient2(low = "white", high = "red", 
                                                    midpoint = 0, limit = c(-1,1), space = "Lab", 
                                                    name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + 
  labs(x = "Distance Category", y = "Angle Category") +
  ggtitle("Hockey Goal Proportions") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = guide_colorbar(barwidth = 3, barheight = 7,
                               title.position = "top", title.hjust = 0.5))


################
allgames_goals %>% group_by(type) %>% summarise(count_of_goals = length(type)/nrow(allgames_goals))


################

# hm so, should we use these coefficients as the weights? And these will be like
# 1/0 variables in front of them that determine what they are?

# We need to do some kind of normalization along the way so that we can make sure all the units are
# properly measured
  
a %<>% select()


glm(as.factor(allGames3$etype) ~ as.factor(allGames3$type))


# Looking at all games
str(allGames2)

##### Setting up a single game

str(hockey1)

testing_hockey <- hockey1 %>% filter(etype %in% c("SHOT", "GOAL")) %>% 
                  select(etype, seconds, distance, type )

new_test_hockey <- data.frame(game.info[1])
str(new_test_hockey)

testing_hockey <- new_test_hockey %>% filter(playbyplay.etype %in% c("SHOT", "GOAL")) %>% 
  select(playbyplay.seconds, playbyplay.etype, playbyplay.ev.team, playbyplay.ev.player.1,
         playbyplay.ev.player.2, playbyplay.distance, playbyplay.type)

str(testing_hockey)

ggplot(data = testing_hockey, aes(x = playbyplay.seconds, 
                                  y = playbyplay.distance, color = playbyplay.ev.team, 
                                  shape = playbyplay.etype)) + geom_point()

### Checking james reimer's play
testing_hockey_reimer <- testing_hockey %>% filter(playbyplay.ev.team == "MTL")

table(testing_hockey_reimer$playbyplay.etype)
## So reimer let in 3 goals on 34 shots = 31/34 save%
save_percent_reimer <- 31/34

## now, lets look at his plot only

ggplot(data = testing_hockey_reimer, aes(x = playbyplay.seconds, 
                                  y = playbyplay.distance, 
                                  color = playbyplay.etype)) + geom_point() +
  geom_line()


## Creating basis for the shots taken
gameBasis65 = create.bspline.basis(c(0,3600), norder = 11)

## looking at plots
plot(gameBasis65)

## Creating time vector
time_vec <- testing_hockey_reimer$playbyplay.seconds

# evaluating basis
evalsplinebasis <- eval.basis(time_vec, gameBasis65)

## the above gives us our Phi matrix

# now let's figure out the coefficients using regression
chat <- (solve((t(evalsplinebasis))%*%evalsplinebasis))%*%(t(evalsplinebasis))%*%testing_hockey_reimer$playbyplay.distance

# now, we can create the functional object again
hockeyFD = fd(chat, gameBasis65,
            list("Seconds","", "Distance"))

# Looking at plot
plotfit.fd(testing_hockey_reimer$playbyplay.distance, 
           testing_hockey_reimer$playbyplay.seconds, hockeyFD, 
           lty=1, lwd=2, col=1)

plot(hockeyFD)

str(testing_hockey_reimer$playbyplay.seconds)
