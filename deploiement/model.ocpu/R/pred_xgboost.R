#' Classification from individually specified features
#'
#' @export
pred_xgboost_indiv <- function(Sepal.Length = 0, Sepal.Width = 0, Petal.Length = 0, Petal.Width = 0) {
  data_pred <- c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  data_pred <- matrix(data_pred, nrow = 1)
  prob <- predict(object = model_iris, newdata = data_pred)
  label = iris_levels[which.max(prob)]
  return(list(label = label, prob = prob))
}

#' Classification from vector of features
#' @export
pred_xgboost_vector <- function(input = c(0,0,0,0)) {
  data_pred <- matrix(input, nrow = 1)
  prob <- predict(object = model_iris, newdata = data_pred)
  label = iris_levels[which.max(prob)]
  return(list(label = label, prob = prob))
}

#' Returns characteristics from input
#' @export
pred_dummy <- function(input) {
  print("input")
}
