

#projects1 = read.csv("datasets/ks-projects-201612.csv",header=T,na.strings="?")
projects = read.csv("datasets/ks-projects-201801.csv",header=T,na.strings="?") #Talvez seja melhor apenas analisarmos um dos datasets dado que ambos têm dimensões suficientemente grandes?

#attach(projects)

dim(projects)
names(projects)

#########Compreensão dos atributos#########

length(unique(projects$ID)) #Não útil

length(unique(projects$name)) #Não útil neste estado, talvez extraindo quantidades (e.g.: número de caracteres ou palavras) ou realizando nlp

length(unique(projects$deadline)) #É necessário arranjar outra forma de interpretar as datas para além de atributo nominativo. Hipóteses:
#-Ter vários atributos, um para cada componente relevante da data
#-Ter um valor que é a diferença da deadline com a epoch


length(unique(projects$category)) #demasiados valores únicos para ser utilizado como atributo nominativo, desnecessário a não ser que se faça algum tratamento. Talvez ver utilizar a frequencia da categoria?

unique(projects$main_category) #Provavelmente muito útil

unique(projects$currency) #Provavelmente útil

summary(projects$goal) #É importante utilizar o usd_goal_real em vez deste, por ser uniforme e estar convertido para USD
summary(projects$usd_goal_real) #Provavelmente muito útil


length(unique(projects$launched)) #Tal como com "deadline" é necessário arranjar outra forma de tratar as datas

unique(projects$state) #Este atributo está demasiado relacionado com o atributo objetivo para ser utilizado

unique(projects$country) #Possivelmente útil

summary(projects$backers) #Este também não seria um atributo objetivo?

summary(projects$pledged) #Para não ter valores em moedas diferentes é necessário que utilizemos apenas o atributo usd.pledged ou usd.pledged.real
summary(projects$usd.pledged) #Tendo em conta que a conversão do atributo goal só existe através da Fixer.io api, convém utilizarmos a mesma para o pledged.
summary(projects$usd_pledged_real) #Classe, atributo objetivo

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

dataset1 <- projects[,c(4,15,14)]  #Removeu-se também o atributo currency e country para reduzir a complexidade
dataset1$launch.deadline.diff = difftime(as.Date(projects[,"deadline"]), as.Date(projects[,"launched"]))

attach(dataset1)

plot(launch.deadline.diff, usd_pledged_real) #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática

lm.fit1 <- lm(usd_pledged_real ~ main_category + usd_goal_real + launch.deadline.diff)
summary(lm.fit1) #modelo continuar a ser muito fraco

rm("dataset1")
detach(dataset1)
