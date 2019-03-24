# pred_xgboost.R

load("deploiement/model.plumber/sysdata.rda")
library(xgboost)

#' Classification from individually specified features
#' @param Sepal.Length Sepal.Length
#' @param Sepal.Width Sepal.Width
#' @param Petal.Length Petal.Length
#' @param Petal.Width Petal.Width
#' @get /xgbkwargs
#' @json
function(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 0, Petal.Width = 0) {
  data_pred <- c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  data_pred <- matrix(data_pred, nrow = 1)
  prob <- predict(object = model_iris, newdata = data_pred)
  label = iris_levels[which.max(prob)]
  return(list(label = label, prob = prob))
}

#* Classification from vector of features
#* @get /xgbvector
#* @json
function(input = c(0,0,0,0)) {
  data_pred <- matrix(input, nrow = 1)
  prob <- predict(object = model_iris, newdata = data_pred)
  label = iris_levels[which.max(prob)]
  return(list(label = label, prob = prob))
}

#* @get /test
#* @json
function(input = c(0,0,0,0)) {
  return(list(input = input, length = length(input), class = class(input)))
}

#* @get /allo
#* @json
function(input = 22) {
  return(list(input = input, length = length(input), class = class(input)))
}


#' @get /search
search <- function(q="", pretty=0){
  paste0("The q parameter is '", q, "'. ",
         "The pretty parameter is '", pretty, "'.")
}

