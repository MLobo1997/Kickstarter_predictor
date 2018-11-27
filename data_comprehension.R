########---ID---########

length(unique(projects$ID)) #Não útil

########---Name---########

length(unique(projects$name)) #Não útil neste estado, talvez extraindo quantidades (e.g.: número de caracteres ou palavras) ou realizando nlp

########---Deadline---########

length(unique(projects$deadline)) #É necessário arranjar outra forma de interpretar as datas para além de atributo nominativo. Hipóteses:
#-Ter vários atributos, um para cada componente relevante da data
#-Ter um valor que é a diferença da deadline com a epoch

########---Category---########


length(unique(projects$category)) #demasiados valores únicos para ser utilizado como atributo nominativo, desnecessário a não ser que se faça algum tratamento. Talvez ver utilizar a frequencia da categoria?



########---Main Category---########

unique(projects$main_category) #Provavelmente muito útil

########---Currency---########

unique(projects$currency) #Provavelmente útil

boxplot(formula = log(projects$usd_pledged_real+0.0001) ~ projects$currency)

########---Goal---########

summary(projects$goal) #É importante utilizar o usd_goal_real em vez deste, por ser uniforme e estar convertido para USD
summary(projects$usd_goal_real) #Provavelmente muito útil


########---Launched---########

length(unique(projects$launched)) #Tal como com "deadline" é necessário arranjar outra forma de tratar as datas

########---State---########

unique(projects$state) #Este atributo está demasiado relacionado com o atributo objetivo para ser utilizado

########---Country---########

unique(projects$country) #Possivelmente útil
boxplot(formula = log(projects$usd_pledged_real+0.0001) ~ projects$country)

########---Backers---########

summary(projects$backers) #Este também não seria um atributo objetivo?

########---Pledged---########

summary(projects$pledged) #Para não ter valores em moedas diferentes é necessário que utilizemos apenas o atributo usd.pledged ou usd.pledged.real

########---USD Pledged---########

summary(projects$usd.pledged) #Tendo em conta que a conversão do atributo goal só existe através da Fixer.io api, convém utilizarmos a mesma para o pledged.

########---USD Pledged Real---########

summary(projects$usd_pledged_real) #Classe, atributo objetivo

########---Duration---########

plot(duration, usd_pledged_real) #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática
plot(duration, log(usd_pledged_real)) #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática

########---name.word_count---########

summary(projects$name.word_count)
plot(projects$name.word_count, log(projects$usd_pledged_real + 0.01)) #não aparenta haver qualquer relação

########---name.length---########

summary(projects$name.length)
plot(projects$name.length, log(projects$usd_pledged_real + 0.01)) #não aparenta haver qualquer relação
