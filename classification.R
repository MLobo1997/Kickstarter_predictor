projects$success <- (projects$usd_pledged_real >= projects$usd_goal_real)
set   <-  projects[, c("categoryconcat2", "other_active_projects", "country", "name.length", "name.word_count", "duration", "usd_goal_real", "success")]
train <-  set[1:(size%/%2),]
test  <-  set[(size%/%2):size,]

glm.fit <- glm(success ~. , data = train)
summary(glm.fit)
pred <- ((predict.glm(glm.fit, newdata = test)) > 0.5)
accurate <- (test$success == pred)
accuracy <- (length(accurate[accurate == TRUE]) / length(accurate))
accuracy
