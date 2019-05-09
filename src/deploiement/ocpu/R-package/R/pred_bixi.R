# pred_bixi.R

#' Classification from individually specified features
#'
#' @importFrom caret dummyVars
#' @importFrom glmnet cv.glmnet
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#' @importFrom magrittr %>%
#' @importFrom stringi stri_replace_all_regex
#'
#' @export
bixikwargs <- function(start_date, start_station_code, end_date, end_station_code, duration_sec, is_member) {

  # arranger en un data.table
  dt_pred <- data.table(start_date, start_station_code, end_date, end_station_code, duration_sec, is_member)

  dt_pred <- merge_data(dt_pred, init_objects$merging_data$data_stations, init_objects$merging_data$points_stations)
  data_pred <- preprocessing_main(copy(dt_pred), train_mode = FALSE, list_objects = init_objects)
  data_pred_regression <- data_pred$data_regression
  data_pred_classif <- data_pred$data_classif

  duree = predict(init_objects$model_glm, as.matrix(data_pred_regression), s = "lambda.min")
  meme_station = predict(init_objects$model_xgb, as.matrix(data_pred_classif)) > 0.5

  return(list(duree = duree, meme_station = meme_station))
}

#' Classification a partir d'un vecteur de variables
#'
#' @importFrom caret dummyVars
#' @importFrom glmnet cv.glmnet
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#' @importFrom magrittr %>%
#' @importFrom stringi stri_replace_all_regex
#'
#' @export
bixidata <- function(data) {

  # convertir en data.table
  dt_pred <- as.data.table(data)

  dt_pred <- merge_data(dt_pred, init_objects$merging_data$data_stations, init_objects$merging_data$points_stations)
  data_pred <- preprocessing_main(copy(dt_pred), train_mode = FALSE, list_objects = init_objects)
  data_pred_regression <- data_pred$data_regression
  data_pred_classif <- data_pred$data_classif

  duree = predict(init_objects$model_glm, as.matrix(data_pred_regression), s = "lambda.min")
  meme_station = predict(init_objects$model_xgb, as.matrix(data_pred_classif)) > 0.5

  return(list(duree = duree, meme_station = meme_station))
}
