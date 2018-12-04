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
projects$country2 <- filterBestCategories(projects, projects$country, 2)
set   <-  projects[, c("categoryconcat2", "other_active_projects", "country2", "name.length", "name.word_count", "duration", "usd_goal_real", "usd_pledged_real")]
train <-  set[1:(size%/%2),]
test  <-  set[(size%/%2):size,]
summary(train)
summary(test)



lm.fit <- lm(usd_pledged_real~., data = train)
summary(lm.fit) 
pred <- predict.lm(lm.fit, newdata = test, se.fit = T, interval = "prediction") #começamos por utilizar cross validation mas como sabiamos que tinhamos muitos dados não havia necessidade por questões de eficiencia
mae <- mean((abs(pred$fit[,1] - test$usd_pledged_real)))
mae #mean absolute error
mre <- 100 * mean((abs(pred$fit[,1] - test$usd_pledged_real) / (test$usd_pledged_real + 1))) #MEAN RELATIVE ERROR
print(paste(mre,"%"))
if (mae < maeMin) {
  print("Found a good one!")
  maeMin <- mae
  best <- i
}

#Resultados:
#c("main_category", "usd_goal_real", "usd_pledged_real"): 746.6837
#c("category", "usd_goal_real", "usd_pledged_real"): 1799.943
#c("categoryconcat", "usd_goal_real", "usd_pledged_real"): 1800.004
#c("category2"(COM FACTORNR 10), "usd_goal_real", "usd_pledged_real"): 235.9796
#c("category3", "usd_goal_real", "usd_pledged_real"): 278.1705

