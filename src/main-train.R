#' Programme qui entraine les modeles de A a Z
#' On fait essentiellement les etapes suivantes:
#' - Load data
#' - Split
#' - Preprocessing
#' - Entrainement
#' 
#' Parametre a changer: path_objects dans la section output
#' Ce parametre indique ou est-ce que les objets calculés seront sauvergarder
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

source("src/extraction/load-data.R")
source("src/preprocessing/preprocessing_main.R")


# Output les objets -------------------------------------------------------

path_objects <- "data/models/"


# Importer les données ----------------------------------------------------

# Doit avoir le fichier 'LIMADMIN.shp' dans le repertoire passer en argument
# a la fonction load_data()
data_bixi <- load_data("data/")


# Split data --------------------------------------------------------------

ind_train <- c(caret::createDataPartition(y = data_bixi$start_station_code, times = 1, p = .75, list = FALSE))
saveRDS(ind_train, paste0(path_objects, "ind_train.rds"))


# Preprocessing -----------------------------------------------------------

preprocessed_objects <- preprocessing_main(data_bixi[ind_train,], train_mode = TRUE)

# Saver les objets
# Objects communs au 2 modeles
write(jsonlite::toJSON(preprocessed_objects$variables_a_imputer, pretty = TRUE), paste0(path_objects, "valeurs_imputations.json"))
# Objets du modele classif
saveRDS(preprocessed_objects$objet_un_chaud_classif, paste0(path_objects, "objet_un_chaud_classif.rds"))
# write(jsonlite::toJSON(preprocessed_objects$valeurs_normalisation, pretty = TRUE), paste0(path_objects, "valeurs_normalisation.json"))
write(jsonlite::toJSON(preprocessed_objects$vars_to_keep_classif, pretty = TRUE), paste0(path_objects, "variables_a_conserver_classif.json"))
# Objets du modele regression
write(jsonlite::toJSON(preprocessed_objects$vars_to_keep_regression, pretty = TRUE), paste0(path_objects, "variables_a_conserver_regression.json"))


# Setter les tables pour le modeling
X_classif <- copy(preprocessed_objects$data_classif)[, target_meme_station := NULL]
X_regression <- copy(preprocessed_objects$data_regression)[, target_duree := NULL]

y_classif <- preprocessed_objects$data_classif$target_meme_station
y_regression <- preprocessed_objects$data_regression$target_duree


# Modeling ----------------------------------------------------------------

glm_full <- glmnet::glmnet(x = as.matrix(X_regression), y = y_regression, family = "gaussian")

ratio <- mean(y_classif == 1)
xgb_full <- xgboost::xgboost(data = as.matrix(X_classif), weight = (1-ratio)*y_classif + ratio*(1-y_classif), label = y_classif, booster = "gbtree", objective = "binary:logistic", eval.metric = "logloss", nrounds = 15)

saveRDS(glm_full, paste0(path_objects, "glm.rds"))
saveRDS(xgb_full, paste0(path_objects, "xgb.rds"))
