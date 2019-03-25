# plumber_ini.R

#' Afficher le message entre en parametre à la fonction
#' @param msg Le message a retourner
#' @get /message
function(msg=""){
  list(msg = paste0("Le message est : ", msg))
}

#' Graphique des donnees iris
#' @param spec If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png
function(spec){
  myData <- iris
  title <- "Toutes"
  
  # Filtre sur l'espèce
  if (!missing(spec)){
    title <- paste0("Espèce '", spec, "' seulement")
    myData <- subset(iris, Species == spec)
  }
  
  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
}
