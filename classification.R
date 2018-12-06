projects$success <- (projects$usd_pledged_real >= projects$usd_goal_real)
set   <-  projects[, c("categoryconcat", "other_active_projects", "country", "name.word_count", "duration", "usd_goal_real", "usd_pledged_real", "success")]
train <-  set[1:(size%/%2),]
test  <-  set[(size%/%2):size,]

bestAccuracy <- -Inf
bestI <- 0


set$categoryconcat <- filterBestCategories(set, projects$categoryconcat, 111)
set$country <- projects$country
for (i in 1:22){
  
  set$country <- filterBestCategories(set, projects$country, i)
  train <-  set[1:(size%/%2),]
  test  <-  set[(size%/%2):size,]
  glm.fit <- glm(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, 18), data = train)
  summary(glm.fit)
  pred <- ((predict.glm(glm.fit, newdata = test)) > 0.5)
  accurate <- (test$success == pred)
  accuracy <- (length(accurate[accurate == TRUE]) / length(accurate))
  accuracy
  
  if (bestAccuracy < accuracy){
    print("O melhor!!!:")
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
#Categorias concatenadas também é melhor que só as categorias
#Fazendo filtragem de category concat o melhor foi com 111 = 67.14%
#Country idealmente deverá ter todos os valores