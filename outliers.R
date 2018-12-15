lm.fit <- lm(usd_pledged_real ~ categoryconcat + usd_goal_real + other_active_projects + country + name.word_count + duration, data = projects)

cooksd <- cooks.distance(lm.fit)
summary(cooksd)
plot(cooksd) #initially there are two so extreme outliers that you can't even distinguish any others

cookCoef <- 1
outlierThreshold <- mean(cooksd) * cookCoef
abline(h = outlierThreshold, col = "red") #drawing the treshold in the graphic

dim(projects[cooksd > outlierThreshold,]) #The system only determines 4 projects as outliers

noOutliers <- projects[cooksd < outlierThreshold,]
lm.fit <- lm(usd_pledged_real ~ categoryconcat + usd_goal_real + other_active_projects + country + name.word_count + duration, data = noOutliers)
summary(lm.fit)
cooksd <- cooks.distance(lm.fit)
plot(cooksd) 
outlierThreshold <- mean(cooksd) * cookCoef
abline(h = outlierThreshold, col = "red") #drawing the treshold in the graphic. Pretty big outliers too

dim(noOutliers[cooksd > outlierThreshold,])


#By removing the outliers on the training data we obtain a better model with better predictions!

removeClassificationOutliers <- function(dataset, cookCoef){
  lm.fit <- glm(success ~ ., data = dataset)
  cooksd <- cooks.distance(lm.fit)
  threshold <- mean(cooksd) * cookCoef
  
  return(dataset[cooksd < threshold,])
}

removeRegressionOutliers <- function(dataset, cookCoef){
  lm.fit <- glm(usd_pledged_real ~ ., data = dataset)
  cooksd <- cooks.distance(lm.fit)
  threshold <- mean(cooksd) * cookCoef
  
  
  return(dataset[cooksd < threshold,])
}
