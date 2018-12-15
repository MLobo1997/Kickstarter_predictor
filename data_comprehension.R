attach(projects)
detach(projects)
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

#Se utilizarmos este bem, entao Main Category talvez se menos significativi, dado que cada subcategoria corresponde a PELO MENOS uma categoria. Com o script a seguir confere-se que há subcategorias iguais para diversas categorias
unique((projects[,c("category", "main_category")])[order(projects$category),])

########---Main Category---########

unique(projects$main_category) #Provavelmente muito útil

########---Currency---########
#By including currency and country some currencies get NA'd
unique(projects$currency) #Provavelmente útil

par(mfrow=c(1,1))
boxplot(formula = projects$usd_pledged_real ~ projects$currency, outline = F)

########---Goal---########

summary(projects$goal) #É importante utilizar o usd_goal_real em vez deste, por ser uniforme e estar convertido para USD
summary(projects$usd_goal_real) #Provavelmente muito útil

success_by_goal <- aggregate(success ~ (usd_goal_real %/% 1000), FUN = mean)
success_by_goal[success_by_goal$success > 0.02,]
barplot(success_by_goal$success)

########---Launched---########

par(mfrow=c(1,1))

length(unique(substring(projects$launched, 1, 10))) #Tal como com "deadline" é necessário arranjar outra forma de tratar as datas

boxplot(usd_pledged_real ~ substring(projects$launched,1, 4), data = projects, outline = F) #pledge em função do ano

sort(projects$launched)
sort(projects$deadline)

project_nr_by_month <- aggregate(usd_pledged_real ~ factor(substring(launched, 6, 7)), FUN = length)
names(project_nr_by_month) <- c("month", "length")
barplot(project_nr_by_month$length) 

boxplot(usd_pledged_real ~ substring(launched, 6, 7), outline = F) #Em função dos meses nao aparenta haver nenhum que a média de financiamentos seja superior

########---State---########

unique(projects$state) #Este atributo está demasiado relacionado com o atributo objetivo para ser utilizado

dim(projects[projects$state %in% c("successful", "failed"),])

########---Country---########

unique(projects$country) #Possivelmente útil
boxplot(formula = projects$usd_pledged_real ~ projects$country, outline = F)

########---Backers---########

summary(projects$backers) #Este também não seria um atributo objetivo?

########---Pledged---########

summary(projects$pledged) #Para não ter valores em moedas diferentes é necessário que utilizemos apenas o atributo usd.pledged ou usd.pledged.real


########---USD Pledged---########

summary(projects$usd.pledged) #Tendo em conta que a conversão do atributo goal só existe através da Fixer.io api, convém utilizarmos a mesma para o pledged.

########---USD Pledged Real---########

summary(projects$usd_pledged_real) #Classe, atributo objetivo
length(projects[projects$usd_pledged_real == 0,"usd_pledged_real"])

#Vamos verificar se houve alterações nos valores de pledging ao longo dos anos

########---Duration---########

summary(duration)
boxplot(usd_pledged_real ~ duration, outline = F, xlab = "Duration", ylab = "Pledge") #se se remover aquele outlier dos 1500 vê se talvez uma correlação quadrática

########---name.word_count---########

summary(name.word_count)
boxplot(usd_pledged_real ~ name.word_count, outline = F, xlab = "Number of Words of the name", ylab = "Pledge") #aparenta haver relação quadratica!!

########---name.length---########

summary(name.length)
boxplot(usd_pledged_real ~ name.length, outline = F, xlab = "Number of characters in the name", ylab = "Pledge")

########---category2---########
summary(projects$category2)

########---category3---########
summary(projects$category3)
boxplot(usd_pledged_real ~ category3, outline = F) #impossivel interpretar

########---categoryconcat---#####
length(unique(projects$categoryconcat))

boxplot(usd_pledged_real ~ categoryconcat, outline = F)

pledge_by_category3 <- aggregate(usd_pledged_real ~ categoryconcat3, FUN = mean)
pledge_by_category3 <- pledge_by_category3[order(pledge_by_category3$usd_pledged_real),]
barplot(pledge_by_category3$usd_pledged_real, names.arg = 1:170)

########---other_active_projects -- através de um programa em java foi criado um atributo que diz qual o número de projetos ativos na data launched
summary(other_active_projects)

par(mfrow=c(1,2))

pledge_by_other_actives <- aggregate(usd_pledged_real ~ (other_active_projects %/% 1000), FUN = mean)

barplot(pledge_by_other_actives$usd_pledged_real, width = 0.75, names.arg = pledge_by_other_actives$`other_active_projects%/%1000` * 1000, xlab = "Intervals of other_active_projects values", ylab = "Pledge mean")
lines(pledge_by_other_actives$`other_active_projects%/%1000`, pledge_by_other_actives$usd_pledged_real, col = "red")

pledge_by_other_actives <- aggregate(success ~ (other_active_projects %/% 1000), FUN = mean)

barplot(pledge_by_other_actives$success, width = 0.75, names.arg = pledge_by_other_actives$`other_active_projects%/%1000` * 1000, xlab = "Intervals of other_active_projects values", ylab = "Success")
lines(pledge_by_other_actives$`other_active_projects%/%1000`, pledge_by_other_actives$success, col = "red")
#Tentar com barplots? Anyway há claramente correlação. Muito interessante a comparação entra a média e mediana. É um bom argumento para dizer que, a longo prazo, seria melhor para o kickstarter reduzir o número de projetos

#Poderá o pledge estar a ser influenciado pelos anos e nao pelo nr de projetos ativos?
len_by_year <- aggregate(ID ~ substring(launched,1,4), FUN = length)
barplot(len_by_year$ID, names.arg = len_by_year$`substring(launched, 1, 4)`)
