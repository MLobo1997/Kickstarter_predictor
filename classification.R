library(MASS)
size <- dim(projects)[1]
projects$success <- (projects$usd_pledged_real >= projects$usd_goal_real)
set   <-  projects[, c("categoryconcat", "other_active_projects", "country", "name.word_count", "duration", "usd_goal_real", "usd_pledged_real", "success")]
train <-  set[1:(size%/%2),]
test  <-  set[(size%/%2):size,]

bestAccuracy <- -Inf
bestI <- 0


set$categoryconcat <- filterBestCategories(set, projects$categoryconcat, 111) # The ideal value is 111
for (i in 1:22){
  
  set$country <- filterBestCategories(set, projects$country, i)
  train <-  set[1:(size%/%2),]
  test  <-  set[(size%/%2):size,]
  glm.fit <- glm(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, 18), data = train)
  summary(glm.fit)
  pred <- ((predict.glm(glm.fit, newdata = test)) > 0.5)
  accuracy <- mean(test$success == pred)
  
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
#Best subsetreg does not make a lot of sense since p is not really big
glm.fit <- glm(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, 18), data = train)
summary(glm.fit)
glm.fit <- step(glm.fit, direction = "backward")
pred <- ((predict.glm(glm.fit, newdata = test, type = "response")) > 0.5)
table(pred, test$success)
accuracy <- mean(test$success == pred)
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

#Experimentar brute force???? o resultado compativamente com glm tem uma diferença nao significativa
lda.fit <- lda(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, 18), data = train)
summary(lda.fit)
#plot(lda.fit)
lda.pred <- predict(lda.fit, newdata = test)
lda.class <- lda.pred$class
table(lda.class, test$success)
accuracy <- mean(lda.class == test$success)
accuracy

#QDA does not work since there are too few entries which are successful