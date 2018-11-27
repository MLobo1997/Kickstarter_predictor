###Duration###
projects$duration <- difftime(as.Date(projects[,"deadline"]), as.Date(projects[,"launched"]))
###Número de palavras do título###
projects$name.word_count <- lengths(strsplit(as.character(projects$name), "\\W+"))
###Número de caractéres do título###
projects$name.length <- nchar(as.character(projects$name))

###Descobrir quais as subcategorias com média de pledging superior e mudar as outras para "other"
categories <- unique(projects$category)
v <- rep(0, length(categories))
names(v) <- categories

for(i in 1:length(v)){
  v[i] <- median(projects[projects$category == categories[i], "usd_pledged_real"]) #mediana para o caso de haver outliers
}

bestCategories <- names(tail(sort(v),10))

projects$category2 <- rep("Other", dim(projects)[1])
for (i in 1:length(projects$category2)){
  if (as.character(projects$category[i]) %in% bestCategories){
    projects$category2[i] <- as.character(projects$category[i])
  }
}#TROCAR PARA FACTOR

###Transformação das subcategorias nas medianas dos seus pledges

projects$category3 <- v[projects$category]