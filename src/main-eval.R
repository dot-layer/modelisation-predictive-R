

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


# Importer les données ----------------------------------------------------

# Doit avoir le fichier 'LIMADMIN.shp' dans le repertoire passer en argument
# a la fonction load_data()
data_bixi <- load_data("data/")
# TEST POUR LE MOMENT: ON VA AVOIR LES DONNÉES EN ENTRÉE DANS L'API
data_test <- data_bixi[sample(1:nrow(data_bixi), 1),]


# Preprocessing -----------------------------------------------------------

data_test <- preprocessing(data_test, path_objects = "data/models/", train = FALSE)


# Prediction --------------------------------------------------------------

model_glm <- readRDS(file = "data/models/glm.rds")
model_xgb <- readRDS(file = "data/models/xgb.rds")

# Penser a avoir un fichier de configs maybe? (threshold, lambda dans glmnet, etc)

list(
  # Gerer le lambda dans glmnet
  duree = predict(model_glm, as.matrix(data_test), s = rev(model_glm$lambda)[1]),
  # Gerer le threshold différement
  meme_station = predict(model_xgb, as.matrix(data_test)) > 0.5
)
