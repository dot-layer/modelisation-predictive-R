library(sf)
library(data.table)
library(caret)
library(fst)
library(stringi)
library(lubridate)
library(xgboost)
library(glmnet)


# Source les fonctions ----------------------------------------------------

# À commenter
source("src/collecte/load-historical-data.R")
source("src/collecte/load-merging-data.R")
source("src/collecte/merge-data.R")
historical_data <- load_historical_data("data/")
merging_data <- load_merging_data("data/")
data_bixi <- merge_data(historical_data, merging_data$data_stations, merging_data$points_stations)


source("src/preprocessing/preprocessing_main.R")


# Split data --------------------------------------------------------------

# Questionnable : les données tests sont-elles vraiment " indépendantes" du train? 
ind_test <- sample(nrow(data_bixi), .4*nrow(data_bixi))


# Preprocessing -----------------------------------------------------------
preprocessed_objects <- preprocessing_main(data_bixi[-ind_test,], train_mode = TRUE)

# Setter les tables pour le modeling
X_reg_train <- copy(preprocessed_objects$data_regression)[, target_duree := NULL]
X_classif_train <- copy(preprocessed_objects$data_classif)[, target_meme_station := NULL]
y_reg_train <- preprocessed_objects$data_regression$target_duree
y_classif_train <- preprocessed_objects$data_classif$target_meme_station


preprocessed_objects_test <- preprocessing_main(data_bixi[ind_test,],
                                                train_mode = FALSE,
                                                list_objects = preprocessed_objects)


# On va créer un set validation pour le plaisir. (À partir du test!)
y_reg_test <- data_bixi[ind_test, duration_sec]
y_classif_test <- as.numeric(data_bixi[ind_test, start_station_code] == data_bixi[ind_test, end_station_code])
X_reg_test <- copy(preprocessed_objects_test$data_regression)
X_classif_test <- copy(preprocessed_objects_test$data_classif)
