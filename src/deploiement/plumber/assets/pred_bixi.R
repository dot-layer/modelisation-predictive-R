# pred_bixi.R

library(jsonlite)
library(data.table)
library(caret)
library(fst)
library(stringi)
library(lubridate)
library(xgboost)
library(glmnet)

# source("sysdata.rda")
# source("merge-data.R")
# source("preprocessing_base.R")
# source("preprocessing_classif.R")
# source("preprocessing_regression.R")
# source("preprocessing_main.R")

load("src/deploiement/plumber/assets/sysdata.rda")
source("src/deploiement/plumber/assets/merge-data.R")
source("src/deploiement/plumber/assets/preprocessing_base.R")
source("src/deploiement/plumber/assets/preprocessing_classif.R")
source("src/deploiement/plumber/assets/preprocessing_regression.R")
source("src/deploiement/plumber/assets/preprocessing_main.R")

# dt_pred_test <- read_fst("src/deploiement/sample_data.fst")
# dt_pred_test <- read_fst("data_test.fst", as.data.table = T)

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
function(start_date, start_station_code, end_date, end_station_code, duration_sec, is_member) {
  
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

#' Classification from a vector of features
#' @param data data au format json
#' @get /bixidata
#' @json
function(data) {
  
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
