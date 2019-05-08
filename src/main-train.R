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
source("src/preprocessing/preprocessing.R")
source("src/preprocessing/preprocessing_classif.R")
source("src/preprocessing/preprocessing_regression.R")


# Output les objets -------------------------------------------------------

path_objects <- "data/models/"


# Importer les données ----------------------------------------------------

# Doit avoir le fichier 'LIMADMIN.shp' dans le repertoire passer en argument
# a la fonction load_data()
data_bixi <- load_data("data/")


# Split data --------------------------------------------------------------

# ind_train <- c(caret::createDataPartition(y = data_bixi$start_station_code, times = 1, p = .75, list = FALSE))
# saveRDS(ind_train, "data/models/ind_train.rds")

ind_test <- sample(nrow(data_bixi), .25*nrow(data_bixi))
saveRDS(ind_test, paste0(path_objects, "ind_test.rds"))


# Preprocessing -----------------------------------------------------------

preprocessed_objects <- preprocessing(data_bixi[-ind_test,], train_mode = TRUE)
# preprocessed_objects <- preprocessing(data_bixi[ind_train,], train_mode = TRUE)
classif_objects <- preprocessing_classif(copy(preprocessed_objects$data_preprocess), train_mode = TRUE)
regression_objects <- preprocessing_regression(copy(preprocessed_objects$data_preprocess), train_mode = TRUE)

# Saver les objets
# Objects communs au 2 modeles
write(jsonlite::toJSON(preprocessed_objects$variables_a_imputer, pretty = TRUE), paste0(path_objects, "valeurs_imputations.json"))
# Objets du modele classif
saveRDS(classif_objects$objet_un_chaud, paste0(path_objects, "objet_un_chaud_classif.rds"))
# write(jsonlite::toJSON(preprocessed_objects$valeurs_normalisation, pretty = TRUE), paste0(path_objects, "valeurs_normalisation.json"))
write(jsonlite::toJSON(classif_objects$vars_to_keep, pretty = TRUE), paste0(path_objects, "variables_a_conserver_classif.json"))
# Objets du modele regression
write(jsonlite::toJSON(regression_objects$vars_to_keep, pretty = TRUE), paste0(path_objects, "variables_a_conserver_regression.json"))

# Setter les tables pour le modeling
X_classif <- copy(classif_objects$data_preprocess)
X_regression <- copy(regression_objects$data_preprocess)
# write.fst(X_classif, "data/X_classif.fst")
# write.fst(X_regression, "data/X_regression.fst")

# X_classif <- copy(classif_objects$data_preprocess)[, target_meme_station := NULL]
# X_regression <- copy(regression_objects$data_preprocess)[, target_duree := NULL]
# y_classif <- classif_objects$data_preprocess$target_meme_station
# y_regression <- regression_objects$data_preprocess$target_duree


# Modeling ----------------------------------------------------------------

# La formule pour créer notre régression.
# Elle ne fait que one-hot encode là, mais on pourrait ajouter des intéractions.
f <- as.formula(paste("y_regression", " ~ ", paste(names(X_regression), collapse = " + ")))
sub_sample <- sample(nrow(X_regression),1e+6)

glm_full <- glmnet::cv.glmnet(x = model.matrix(f, X_regression)[sub_sample,-1],
                              y = y_regression[sub_sample],
                              family = "gaussian",
                              nfolds = 5)

ratio <- mean(y_classif == 1)
xgb_full <- xgboost::xgboost(data = as.matrix(X_classif), weight = (1-ratio)*y_classif + ratio*(1-y_classif), label = y_classif, booster = "gbtree", objective = "binary:logistic", eval.metric = "logloss", nrounds = 15)


saveRDS(glm_full, paste0(path_objects, "glm.rds"))
saveRDS(xgb_full, paste0(path_objects, "xgb.rds"))
