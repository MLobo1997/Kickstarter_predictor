###Duration###
createDuration <- function(dataset){
  return(as.numeric(difftime(as.Date(dataset[,"deadline"]), as.Date(dataset[,"launched"]))))
}
projects$duration <- createDuration(projects)
#o##Número de palavras do título###
createName.wordcount <- function(dataset){
  return(lengths(strsplit(as.character(dataset$name), "\\W+")))
}
projects$name.word_count <- createName.wordcount(projects)

###Número de caractéres do título###
createNameLength <- function(dataset){
  return(nchar(as.character(dataset$name)))
}

projects$name.length <- createNameLength(projects)

###Descobrir quais as subcategorias com média de pledging superior e mudar as outras para "other". Isto, comparativamente a utilizar category traz uma eficiencia muito superior, no entanto apenas dá resultados ligeiramente melhores. A melhor tentativa foi com factorNr a 33
calculateCategoryValues <- function(dataset, category){ #função auxiliar
  categories <- unique(category)
  v <- rep(0, length(categories))
  names(v) <- categories
  
  for(i in 1:length(v)){
    v[i] <- median(dataset[category == categories[i], "usd_pledged_real"]) #mediana para não ser influenciado por outliers
  }
  
  return(v)
}

createCategory2 <- function(dataset, category, factorNr){
  v <- calculateCategoryValues(dataset, category) 
  
  bestCategories <- factor(names(tail(sort(v), factorNr))) #definir aqui o número de subcategorias
  
  r <- rep("Other", dim(dataset)[1])
  for (i in 1:length(r)){
    if (category[i] %in% bestCategories){
      r[i] <- as.character(category[i])
    }
  }
  
  return(factor(r))
}
projects$category2 <- createCategory2(projects, projects$category, 10)

###Transformação das subcategorias nas medianas dos seus pledges
createCategory3 <- function(dataset, category){
  v <- calculateCategoryValues(dataset, category) 
  
  return(v[dataset$category])
}

projects$category3 <- createCategory3(projects, projects$category)


###Concatenação das categorias com subcategorias. Porque cada subcategorias está associada a uma categoria e talvez não faça sentido separar essa informação. Este atributo acabou por se relevar uma necessidade. Ao fazer modelo linear tanto com category como com main_category resulta em valores de main_category com coeficiente nulos, porque alguns valores de main_category podem ser previstos através de outros de category (https://stackoverflow.com/questions/40774922/how-to-solve-rank-deficient-fit-may-be-misleading-error-on-my-linear-model-in)
createConcatenatedCategory <- function(dataset){
  return(factor(paste(dataset$main_category, dataset$category, sep = " - ")))
}
projects$categoryconcat <- createConcatenatedCategory(projects)

projects$categoryconcat2 <- createCategory2(projects, projects$categoryconcat, 10)

###Fazer um categoryconcat2



