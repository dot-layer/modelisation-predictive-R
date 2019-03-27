
#' Pretraitements pour le jeu de données à prédire
#' Importer les objets créer lors de l'entraînement


# Load packages -----------------------------------------------------------

library(data.table)
library(caret)
library(fst)
library(jsonlite)


# Load data ---------------------------------------------------------------

data_inference <- read_fst("data/data_bixi.fst", as.data.table = TRUE)


# Données manquantes ------------------------------------------------------

# apply(apply(data_bixi, 2, is.na), 2, sum)
# Aucune données manquantes (pour le moment)


# Nouveaux attributs et encodage ------------------------------------------

# week_start: 1=Lundi 7=Dimanche
data_inference[, `:=`(start_wday = wday(start_date, week_start = 1),
                      start_hour = hour(start_date_time))]

data_inference[, weekend_flag := as.integer(start_wday >= 6)]

data_inference[, moment_journee := 0L]
data_inference[start_hour >= 6 & start_hour < 23, moment_journee := 1L]
data_inference[start_hour >= 11 & start_hour < 16, moment_journee := 2L]
data_inference[start_hour >= 16 & start_hour < 23, moment_journee := 3L]

objet_un_chaud <- readRDS("data/objet_un-chaud.rds")
data_inference <- data.table(predict(objet_un_chaud, data_inference))


# Conserver les variables pertinentes à la modélisation

vars <- jsonlite::fromJSON("data/variables_a_conserver.json")
data_preprocess_inference <- data_inference[, ..vars]
write_fst(data_preprocess_inference, path = "data/data_preprocess_inference.fst")



