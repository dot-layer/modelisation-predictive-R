# pred_bixi.R

load("src/deploiement/model.plumber/sysdata.rda")
library(xgboost)
library(fst)
library(jsonlite)

dt_pred_test <- read_fst("src/deploiement/data_test.fst", as.data.table = T)
# jsonlite::write_json(dt_pred_test, path = "src/deploiement/data_test.json")
# jsonlite::write_json(dt_pred_test, path = "src/deploiement/data_test_elements.json")

#' Classification from individually specified features
#' @param start_date start_date
#' @param start_station_code start_station_code
#' @param end_date end_date
#' @param end_station_code end_station_code
#' @param duration_sec duration_sec
#' @param is_member is_member
#' @param start_date_time start_date_time
#' @param end_date_time end_date_time
#' @param start_quartier start_quartier
#' @param end_quartier end_quartier
#' @get /bixikwargs
#' @json
function(start_date, start_station_code, end_date, end_station_code, duration_sec, is_member, 
         start_date_time, end_date_time, start_quartier, end_quartier) {
  
  # arranger en un data.table
  dt_pred <- data.table(start_date, start_station_code, end_date, end_station_code, duration_sec, is_member, 
                        start_date_time, end_date_time, start_quartier, end_quartier)
  
  # convertir les dates
  dt_pred[, start_date := as.Date(start_date)]
  dt_pred[, end_date := as.Date(end_date)]
  dt_pred[, start_date_time := as.POSIXct(start_date_time)]
  dt_pred[, end_date_time := as.POSIXct(end_date_time)]
  
  # preprocess
  dt_pred <- preprocessing(data = dt_pred, path_objects = "data/models/", train = FALSE)
  
  # duree
  duree = predict(model_glm, as.matrix(dt_pred), s = rev(model_glm$lambda)[1])
  # Gerer le threshold différement
  meme_station = predict(model_xgb, as.matrix(dt_pred)) > 0.5
  
  return(list(duree = duree, meme_station = meme_station))
}

#' Classification from a vector of features
#' @param data data au format json
#' @get /bixidata
#' @json
function(data) {
  
  # convertir en data.table
  dt_pred <- as.data.table(data)
  
  # convertir les dates
  dt_pred[, start_date := as.Date(start_date)]
  dt_pred[, end_date := as.Date(end_date)]
  dt_pred[, start_date_time := as.POSIXct(start_date_time)]
  dt_pred[, end_date_time := as.POSIXct(end_date_time)]
  
  # preprocess
  dt_pred <- preprocessing(data = dt_pred, path_objects = "data/models/", train = FALSE)
  
  # duree
  duree = predict(model_glm, as.matrix(dt_pred), s = rev(model_glm$lambda)[1])
  # Gerer le threshold différement
  meme_station = predict(model_xgb, as.matrix(dt_pred)) > 0.5
  
  return(list(duree = duree, meme_station = meme_station))
}
