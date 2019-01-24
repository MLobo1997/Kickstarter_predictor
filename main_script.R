#library(boot)
setwd("~/Development/Kickstarter_predictor") # para o Lobo
#projects1 = read.csv("datasets/ks-projects-201612.csv",header=T,na.strings="?")
projects <- read.csv("datasets/ks-projects-201801_WithOtherActive.csv",header=T,na.strings="?") #Talvez seja melhor apenas analisarmos um dos datasets dado que ambos têm dimensões suficientemente grandes?
names(projects)
projects <- projects[projects$state %in% c("successful", "failed"),] #removing attributes which were not finished (at the time of the snapshot) yet or had wierd status
projects <- projects[,-c(7, 9, 10, 11, 13)] #são removidos os atributos que não faz sentido utilizarmos (não os que não sao úteis)
#Tendo nos apercebi que existem projetos com data de inicio em 1970 e que nenhum têm pledge, estes foram removidos
projects <- projects[substring(projects$launched,1, 4) != "1970",]
#Transformar as datas todas em strings (porque estão em factors)
projects$launched <- as.character(projects$launched)
projects$deadline <- as.character(projects$deadline)

#attach(projects)

dim(projects)
names(projects)


##########Tratamento de dados########
size <- dim(projects)[1]
set.seed(125)
indexes <- sample(1:dim(projects)[1], size)
projects <- projects[indexes,]  #Removeu-se também o atributo currency e country para reduzir a complexidade. Como tamos a usar o dataset todo isto apenas serve para baralhar os dados, mas se reduzirmos o size já exclui dados

names(projects)

######TEST######
set   <-  projects[, c("categoryconcat3", "other_active_projects", "currencyCountry2", "name.word_count", "duration", "usd_goal_real", "usd_pledged_real")]
train <-  set[1:(size%/%2),]
test  <-  set[(size%/%2):size,]
summary(train)
summary(test)


bestMae <- Inf
bestI <- 0

for (i in 1:25){
  lm.fit <- lm(usd_pledged_real~ poly(categoryconcat3,2) + poly(usd_goal_real,12) + other_active_projects + poly(country3,3) + poly(duration, 18), data = train)
  pred <- predict.lm(lm.fit, newdata = test, se.fit = T, interval = "prediction") #começamos por utilizar cross validation mas como sabiamos que tinhamos muitos dados não havia necessidade por questões de eficiencia
  mae <- mean((abs(pred$fit[,1] - test$usd_pledged_real)))
  
  if (bestMae > mae){
    print("O melhor!!!:")
    bestI <- i
    print(bestI)
    print(paste(mae))
    bestMae <- mae
  }
  else {
    print ("Meh:")
    print (i)
  }
}

#train <- removeRegressionOutliers(train, 10) Remover outliers em regressão torna o erro absolutamente ridiculo
#dim(train2)

lm.fit <- lm(usd_pledged_real~ poly(categoryconcat3,2) + poly(usd_goal_real,12) + other_active_projects + currencyCountry2 + poly(duration, 18), data = train)
summary(lm.fit)
pred <- predict.lm(lm.fit, newdata = test, se.fit = T, interval = "prediction") #começamos por utilizar cross validation mas como sabiamos que tinhamos muitos dados não havia necessidade por questões de eficiencia
mae <- mean((abs(pred$fit[,1] - test$usd_pledged_real)))
mae
#14003.04 com:
#lm.fit <- lm(usd_pledged_real~ poly(categoryconcat3,2) + poly(usd_goal_real,12) + other_active_projects + poly(country3,3) + poly(duration, 18), data = train)
#14002.55 com:

#Regsubsets 
regfit <- regsubsets(usd_pledged_real~ poly(categoryconcat3,2) + poly(usd_goal_real,12) + other_active_projects + poly(country3,3) + poly(duration, 18), data = train, nvmax =  36)
reg.summary <- summary(regfit)

predict(regfit, newdata = test, interval = "prediction")

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
p <- which.max(reg.summary$adjr2)
points(p,reg.summary$adjr2[p], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
p <- which.min(reg.summary$cp)
points(p,reg.summary$cp[p],col="red",cex=2,pch=20)
p <- which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(p,reg.summary$bic[p],col="red",cex=2,pch=20)
