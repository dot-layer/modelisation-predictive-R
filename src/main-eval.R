#' Programme est un exemple de code pour predire une nouvelle donnée
#' Ca replique le comportement du service lors de la prediction
#' 

# Load les packages -------------------------------------------------------

library(sf)
library(data.table)
library(caret)
library(fst)
library(stringi)
library(lubridate)
library(xgboost)
library(glmnet)


# Source les fonctions ----------------------------------------------------

source("src/extraction/load-historical-data.R")
source("src/extraction/load-merging-data.R")
source("src/extraction/merge-data.R")
source("src/preprocessing/preprocessing_main.R")


# A lancer au debut du server ---------------------------------------------

source("src/init.R")
init_objects <- init("data/models/")


# Importer les données ----------------------------------------------------

# Doit avoir le fichier 'LIMADMIN.shp' dans le repertoire passer en argument
# a la fonction load_data()
sample_data <- load_historical_data("data/")[sample(1:500, 1),]
data_bixi <- merge_data(sample_data, init_objects$merging_data$data_stations, init_objects$merging_data$points_stations)

# TEST POUR LE MOMENT: ON VA AVOIR LES DONNÉES EN ENTRÉE DANS L'API
data_test <- data_bixi


# Preprocessing -----------------------------------------------------------

data_pred <- preprocessing_main(copy(data_test), train_mode = FALSE, list_objects = init_objects)

data_pred_regression <- data_pred$data_regression
data_pred_classif <- data_pred$data_classif


# Prediction --------------------------------------------------------------

# Penser a avoir un fichier de configs maybe? (threshold, lambda dans glmnet, etc)
list(
  # Gerer le lambda dans glmnet
  duree = predict(init_objects$model_glm, as.matrix(data_pred_regression), s = "lambda.min"),
  # Gerer le threshold différement
  meme_station = predict(init_objects$model_xgb, as.matrix(data_pred_classif)) > 0.5
)
