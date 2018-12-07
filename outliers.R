lm.fit <- lm(usd_pledged_real ~ categoryconcat + usd_goal_real + other_active_projects + country + name.word_count + duration, data = projects)

cooksd <- cooks.distance(lm.fit)
summary(cooksd)
plot(cooksd) #initially there are two so extreme outliers that you can't even distinguish any others

cookCoef <- 4
outlierTreshold <- mean(cooksd) * cookCoef
abline(h = outlierTreshold, col = "red") #drawing the treshold in the graphic

dim(projects[cooksd > outlierTreshold,]) #The system only determines 4 projects as outliers

noOutliers <- projects[cooksd < outlierTreshold,]
lm.fit <- lm(usd_pledged_real ~ categoryconcat + usd_goal_real + other_active_projects + country + name.word_count + duration, data = noOutliers)
summary(lm.fit)
cooksd <- cooks.distance(lm.fit)
plot(cooksd) 
outlierTreshold <- mean(cooksd) * cookCoef
abline(h = outlierTreshold, col = "red") #drawing the treshold in the graphic. Pretty big outliers too

dim(noOutliers[cooksd > outlierTreshold,])
