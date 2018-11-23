library(boot)
#projects1 = read.csv("datasets/ks-projects-201612.csv",header=T,na.strings="?")
projects = read.csv("datasets/ks-projects-201801.csv",header=T,na.strings="?") #Talvez seja melhor apenas analisarmos um dos datasets dado que ambos têm dimensões suficientemente grandes?
names(projects)
projects <- projects[,-c(7, 9, 10, 11, 13)] #são removidos os atributos que não faz sentido utilizarmos (não os que não sao úteis)

#attach(projects)

dim(projects)
names(projects)

##########Tratamento de dados#########

dataset0 <- projects[,c(4, 5, 12, 15, 14)] #São removidos todos os atributos que não serão utilizados (id, goal, pledged, usd.pledged, state, backers,) ou não podem ser utilizados sem tratamento (name, category, deadline, launched)
names(dataset0)

attach(dataset0)

par(mfrow = c(2,2))
plot(usd_goal_real, usd_pledged_real); plot(main_category, usd_pledged_real); plot(currency, usd_pledged_real); plot(country, usd_pledged_real);

lm.fit0 <- lm(usd_pledged_real ~ usd_goal_real + main_category + currency + country)
summary(lm.fit0) #Modelo muito fraco. Apenas descreve os dados a 0.8%

rm("lm.fit0")
rm("dataset0")
detach(dataset0)

#########Agora a sério#########

set.seed(125)
indexes <- sample(1:dim(projects)[1], 5000)
projects <- projects[indexes,]  #Removeu-se também o atributo currency e country para reduzir a complexidade
projects$duration = difftime(as.Date(projects[indexes,"deadline"]), as.Date(projects[indexes,"launched"]))
names(projects)
projects <- projects[, c("main_category", "usd_goal_real", "duration", "usd_pledged_real")]
summary(projects)

attach(projects)

plot(duration, usd_pledged_real) #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática
plot(duration, log(usd_pledged_real)) #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática

lm.fit1 <- glm(usd_pledged_real~., data = projects)
summary(lm.fit1) #modelo continuar a ser muito fraco
cv.err <- cv.glm(projects, lm.fit1, K = 100)$delta[1]
cv.err


