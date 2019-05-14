# pred_bixi.R

library(jsonlite)
library(data.table)
library(caret)
library(fst)
library(stringi)
library(lubridate)
library(xgboost)
library(glmnet)

load("sysdata.rda")
source("merge-data.R")
source("preprocessing_base.R")
source("preprocessing_classif.R")
source("preprocessing_regression.R")
source("preprocessing_main.R")

# load("src/deploiement/plumber/assets/sysdata.rda")
# source("src/deploiement/plumber/assets/merge-data.R")
# source("src/deploiement/plumber/assets/preprocessing_base.R")
# source("src/deploiement/plumber/assets/preprocessing_classif.R")
# source("src/deploiement/plumber/assets/preprocessing_regression.R")
# source("src/deploiement/plumber/assets/preprocessing_main.R")

#' Classification from individually specified features
#' @param start_date start_date
#' @param start_station_code start_station_code
#' @param is_member is_member
#' @get /bixikwargs
#' @post /bixikwargs
#' @json
function(start_date="2017-04-15 00:48", start_station_code=6079, is_member=1) {
  
  # arranger en un data.table
  dt_pred <- data.table(start_date = as.character(start_date), 
                        start_station_code = as.integer(start_station_code), 
                        is_member = as.numeric(is_member))
  
  dt_pred <- merge_data(dt_pred, init_objects$merging_data$data_stations)
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
#' @post /bixidata
#' @json
function(data) {
  
  # convertir en data.table
  dt_pred <- as.data.table(data)
  
  dt_pred <- merge_data(dt_pred, init_objects$merging_data$data_stations)
  data_pred <- preprocessing_main(copy(dt_pred), train_mode = FALSE, list_objects = init_objects)
  data_pred_regression <- data_pred$data_regression
  data_pred_classif <- data_pred$data_classif
  
  duree = predict(init_objects$model_glm, as.matrix(data_pred_regression), s = "lambda.min")
  meme_station = predict(init_objects$model_xgb, as.matrix(data_pred_classif)) > 0.5
  
  return(list(duree = duree, meme_station = meme_station))
}
