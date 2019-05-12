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

source("src/collecte/load-historical-data.R")
source("src/collecte/load-merging-data.R")
source("src/collecte/merge-data.R")
source("src/preprocessing/preprocessing_main.R")


# Output les objets -------------------------------------------------------

path_objects <- "data/models/"


# Importer les données ----------------------------------------------------

# Doit avoir le fichier 'LIMADMIN.shp' dans le repertoire passer en argument
# a la fonction load_data()
historical_data <- load_historical_data("data/")
merging_data <- load_merging_data("data/")
merge_data(historical_data, merging_data$data_stations)

data_bixi <- copy(historical_data)

# Split data --------------------------------------------------------------

# ind_train <- c(caret::createDataPartition(y = data_bixi$start_station_code, times = 1, p = .75, list = FALSE))
# saveRDS(ind_train, "data/models/ind_train.rds")

ind_test <- sample(nrow(data_bixi), .25*nrow(data_bixi))
saveRDS(ind_test, paste0(path_objects, "ind_test.rds"))


# Preprocessing -----------------------------------------------------------

preprocessed_objects <- preprocessing_main(data_bixi[-ind_test,], train_mode = TRUE)

# Saver les objets
# Objects communs au 2 modeles
write(jsonlite::toJSON(preprocessed_objects$variables_a_imputer, pretty = TRUE), paste0(path_objects, "valeurs_imputations.json"))
# Objets du modele classif
saveRDS(preprocessed_objects$objet_un_chaud, paste0(path_objects, "objet_un_chaud.rds"))
# write(jsonlite::toJSON(preprocessed_objects$valeurs_normalisation, pretty = TRUE), paste0(path_objects, "valeurs_normalisation.json"))
write(jsonlite::toJSON(preprocessed_objects$vars_to_keep_classif, pretty = TRUE), paste0(path_objects, "variables_a_conserver_classif.json"))
# Objets du modele regression
write(jsonlite::toJSON(preprocessed_objects$vars_to_keep_regression, pretty = TRUE), paste0(path_objects, "variables_a_conserver_regression.json"))

# Setter les tables pour le modeling
X_regression <- copy(preprocessed_objects$data_regression)[, target_duree := NULL]
X_classif <- copy(preprocessed_objects$data_classif)[, target_meme_station := NULL]

y_regression <- preprocessed_objects$data_regression$target_duree
y_classif <- preprocessed_objects$data_classif$target_meme_station


# Modeling ----------------------------------------------------------------

sub_sample <- sample(nrow(X_regression), 1e+6)

glm_full <- glmnet::cv.glmnet(x = as.matrix(X_regression)[sub_sample,],
                              y = y_regression[sub_sample],
                              family = "gaussian",
                              nfolds = 5)

ratio <- mean(y_classif == 1)
xgb_full <- xgboost::xgboost(data = as.matrix(X_classif), weight = (1-ratio)*y_classif + ratio*(1-y_classif), label = y_classif, booster = "gbtree", objective = "binary:logistic", eval.metric = "logloss", nrounds = 20)


saveRDS(glm_full, paste0(path_objects, "glm.rds"))
saveRDS(xgb_full, paste0(path_objects, "xgb.rds"))
