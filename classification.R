library(MASS)
size <- dim(projects)[1]
set.seed(125)
indexes <- sample(1:dim(projects)[1], size)
projects <- projects[indexes,]  #Removeu-se também o atributo currency e country para reduzir a complexidade. Como tamos a usar o dataset todo isto apenas serve para baralhar os dados, mas se reduzirmos o size já exclui dados
projects$success <- (projects$usd_pledged_real >= projects$usd_goal_real)
set   <-  projects[, c("categoryconcat", "other_active_projects", "currencyCountry", "name.word_count", "duration", "usd_goal_real", "success")]
train <-  set[1:(size%/%2),]
test  <-  set[(size%/%2):size,]

bestAccuracy <- -Inf
bestI <- 0


set$categoryconcat <- filterBestCategories(set, projects$categoryconcat, 111) # The ideal value is 111
for (i in 1:25){
  
  glm.fit <- glm(success ~ poly(categoryconcat, 26) + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, 18), data = train)
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

#To remove outliers:
#train <- removeClassificationOutliers(train, 6)

glm.fit <- glm(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + currencyCountry + poly(name.word_count, 15) + poly(duration, 18), data = train)
summary(glm.fit)
#glm.fit <- step(glm.fit, direction = "backward")
pred <- ((predict.glm(glm.fit, newdata = test, type = "response")) > 0.5)
table(pred, test$success)
accuracy <- mean(test$success == pred)
accuracy

#melhores poly 67.433% (com remoção de outliers nos dados de treino):
#usd_goal_real: 10
#other_active_projects: 7
#word_count: 15
#duration: 18
#Desce um pouco qnd trocamos o filtro de categorias para success, mas não é significativo

#Tentamos muito tranformar category e country em numéricos, no entanto os resultados nunca são favoráveis relativamente a usar os factors filtrados. São no entanto, bastante mais rápidos

#Com todas as categorias concatenadas acabou por se revelar melhor aqui do que com as 35 filtradas
#Categorias concatenadas também é melhor que só as categorias
#Fazendo filtragem de category concat o melhor foi com 111 = 67.14%
#Country idealmente deverá ter todos os valores

lda.fit <- lda(success ~ categoryconcat + poly(usd_goal_real,10) + poly(other_active_projects, 7) + country + poly(name.word_count, 15) + poly(duration, 18), data = train)
summary(lda.fit)
#plot(lda.fit)
lda.pred <- predict(lda.fit, newdata = test)
lda.class <- lda.pred$class
table(lda.class, test$success)
accuracy <- mean(lda.class == test$success)
accuracy

#QDA does not work since there are too few entries which are successful