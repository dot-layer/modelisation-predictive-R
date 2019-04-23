

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


# Split data --------------------------------------------------------------

ind_train <- c(caret::createDataPartition(y = data_bixi$start_station_code, times = 1, p = .75, list = FALSE))
saveRDS(ind_train, "data/models/ind_train.rds")


# Preprocessing -----------------------------------------------------------

X <- preprocessing(data_bixi[ind_train,], train_mode = TRUE)

y_duree <- X$target_duree
y_meme <- X$target_meme_station

X <- X[,-c("target_duree","target_meme_station")]


# Modeling ----------------------------------------------------------------

glm_full <- glmnet::glmnet(x = as.matrix(X), y = y_duree, family = "gaussian")

ratio <- mean(y_meme == 1)
xgb_full <- xgboost::xgboost(data = as.matrix(X), weight = (1-ratio)*y_meme + ratio*(1-y_meme), label = y_meme, booster = "gblinear", objective = "binary:logistic", eval.metric = "logloss", nrounds = 15)

saveRDS(glm_full, "data/models/glm.rds")
saveRDS(xgb_full, "data/models/xgb.rds")
