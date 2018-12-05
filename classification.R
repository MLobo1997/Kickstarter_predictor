projects$success <- (projects$usd_pledged_real >= projects$usd_goal_real)
set   <-  projects[, c("categoryconcat", "other_active_projects", "country", "name.word_count", "duration", "usd_goal_real", "success")]
train <-  set[1:(size%/%2),]
test  <-  set[(size%/%2):size,]

bestAccuracy <- -Inf
bestI <- 0


l <- 25
for (i in 1:l){
  glm.fit <- glm(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, i), data = train)
  summary(glm.fit)
  pred <- ((predict.glm(glm.fit, newdata = test)) > 0.5)
  accurate <- (test$success == pred)
  accuracy <- (length(accurate[accurate == TRUE]) / length(accurate))
  accuracy
  
  if (bestAccuracy < accuracy){
    print("Os melhores para já:")
    bestI <- i
    print(bestI)
    print(paste(accuracy * 100, "%"))
    bestAccuracy <- accuracy
  }
  else {
    print ("Meh:")
    print (i)
  }
}

glm.fit <- glm(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, 18), data = train)
summary(glm.fit)
pred <- ((predict.glm(glm.fit, newdata = test)) > 0.5)
accurate <- (test$success == pred)
accuracy <- (length(accurate[accurate == TRUE]) / length(accurate))
accuracy

#melhores poly 67.133%:
#usd_goal_real: 10
#other_active_projects: 7
#word_count: 15
#duration: 18

#Com todas as categorias concatenadas acabou por se revelar melhor aqui do que com as 35 filtradas