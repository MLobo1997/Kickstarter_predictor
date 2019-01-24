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
    v[i] <- mean(dataset[category == categories[i], "success"]) #mediana para não ser influenciado por outliers
  }
  
  return(v)
}

filterBestCategories <- function(dataset, category, factorNr){
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
projects$category2 <- filterBestCategories(projects, projects$category, 10)

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

projects$categoryconcat2 <- filterBestCategories(projects, projects$categoryconcat, 111) #The ideal value for classification was 111


#Creating the sortable attribute of categories
calculateEnumeratedCategoryValues <- function(dataset, category, objective){ #função auxiliar
  categories <- unique(category)
  v <- rep(0, length(categories))
  names(v) <- categories
  
  for(i in 1:length(v)){
    v[i] <- mean(dataset[category == categories[i], objective]) 
  }
  
  v <- sort(v)
  
  for (i in 1:length(v)){
    v[i] = i
  }
  
  return(v)
}


createCategory3Classification <- function (dataset, values){
  v <- calculateEnumeratedCategoryValues(dataset, values, "success")
  
  return(v[values])
}

createCategory3Regression <- function (dataset, values){
  v <- calculateEnumeratedCategoryValues(dataset, values, "usd_pledged_real")
  
  return(v[values])
}

projects$categoryconcat3 <- createCategory3Regression(projects, projects$categoryconcat)

projects$country3 <- createCategory3Regression(projects, projects$country)

###CurrencyCountry###
projects$currencyCountry <- factor(paste(as.character(projects$currency), as.character(projects$country), sep = "-"))

projects$currencyCountry2 <- calculateEnumeratedCategoryValues(projects, projects$currencyCountry, "usd_pledged_real")[projects$currencyCountry]
