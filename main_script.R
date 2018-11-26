library(boot)
setwd("~/Development/Kickstarter_predictor") # para o Lobo
#projects1 = read.csv("datasets/ks-projects-201612.csv",header=T,na.strings="?")
projects = read.csv("datasets/ks-projects-201801.csv",header=T,na.strings="?") #Talvez seja melhor apenas analisarmos um dos datasets dado que ambos têm dimensões suficientemente grandes?
names(projects)
projects <- projects[,-c(7, 9, 10, 11, 13)] #são removidos os atributos que não faz sentido utilizarmos (não os que não sao úteis)

#attach(projects)

dim(projects)
names(projects)

##########Tratamento de dados########
set.seed(125)
indexes <- sample(1:dim(projects)[1], 20000)
projects <- projects[indexes,]  #Removeu-se também o atributo currency e country para reduzir a complexidade
projects$duration = difftime(as.Date(projects[indexes,"deadline"]), as.Date(projects[indexes,"launched"]))
names(projects)
projects <- projects[, c("main_category", "usd_goal_real", "duration", "usd_pledged_real")]
summary(projects)
dim(projects)

attach(projects)

plot(duration, usd_pledged_real) #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática
plot(duration, log(usd_pledged_real)) #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática
lm.fit1 <- glm(usd_pledged_real~., data = projects)
summary(lm.fit1) #modelo continuar a ser muito fraco
cv.err <- cv.glm(projects, lm.fit1, K = 100)$delta[1]
cv.err


