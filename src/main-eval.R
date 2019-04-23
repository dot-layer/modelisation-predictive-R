

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

source("src/extraction/load-data.R")
source("src/preprocessing/preprocessing.R")


# A lancer au debut du server ---------------------------------------------

source("src/deploiement/init.R")
init_objects <- init("data/models/")


# Importer les données ----------------------------------------------------

# Doit avoir le fichier 'LIMADMIN.shp' dans le repertoire passer en argument
# a la fonction load_data()
data_bixi <- load_data("data/")
# TEST POUR LE MOMENT: ON VA AVOIR LES DONNÉES EN ENTRÉE DANS L'API
data_test <- data_bixi[sample(1:nrow(data_bixi), 1),]


# Preprocessing -----------------------------------------------------------

data_test <- preprocessing(data_test, train_mode = FALSE, list_objects = init_objects)


# Prediction --------------------------------------------------------------

# Penser a avoir un fichier de configs maybe? (threshold, lambda dans glmnet, etc)
list(
  # Gerer le lambda dans glmnet
  duree = predict(init_objects$model_glm, as.matrix(data_test), s = rev(init_objects$model_glm$lambda)[1]),
  # Gerer le threshold différement
  meme_station = predict(init_objects$model_xgb, as.matrix(data_test)) > 0.5
)
