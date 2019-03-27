
#' Pretraitements pour le jeu de données d'entraînement
#' Sauvergarder les objets necessaires a lors de l'inférence


# Load packages -----------------------------------------------------------

library(data.table)
library(caret)
library(fst)


# Load data ---------------------------------------------------------------

data_bixi <- read_fst("data/data_bixi.fst", as.data.table = TRUE)
# Créer la variable réponse
setnames(data_bixi, "duration_sec", "target_duree")
data_bixi[, target_meme_station := as.integer(start_station_code == end_station_code)]


# Remove outliers ---------------------------------------------------------

# On utilise la methode avec plus/moins 3 écarts-types
# 3 écarts-type au dessus de la moyenne = 45 mins
mean(data_bixi$target_duree) + 3 * sd(data_bixi$target_duree)
data_bixi <- data_bixi[which(data_bixi$target_duree <= 2700 | data_bixi$target_duree >= 0)]


# Données manquantes ------------------------------------------------------

# apply(apply(data_bixi, 2, is.na), 2, sum)
# Aucune données manquantes (pour le moment)


# Nouveaux attributs et encodage ------------------------------------------

# week_start: 1=Lundi 7=Dimanche
data_bixi[, `:=`(start_wday = wday(start_date, week_start = 1),
                 start_hour = hour(start_date_time))]

data_bixi[, weekend_flag := as.integer(start_wday >= 6)]

data_bixi[, moment_journee := 0L]
data_bixi[start_hour >= 6 & start_hour < 23, moment_journee := 1L]
data_bixi[start_hour >= 11 & start_hour < 16, moment_journee := 2L]
data_bixi[start_hour >= 16 & start_hour < 23, moment_journee := 3L]

objet_un_chaud <- dummyVars(" ~ .", copy(data_bixi)[, (c("target_duree", "target_meme_station")) := NULL], 
                            fullRank = TRUE)
saveRDS(objet_un_chaud, "data/objet_un-chaud.rds")
data_bixi <- cbind(predict(objet_un_chaud, copy(data_bixi)[, (c("target_duree", "target_meme_station")) := NULL]),
                   copy(data_bixi)[, (c("target_duree", "target_meme_station")), with = F])


# Conserver les variables pertinentes à la modélisation

vars <- c("target_duree", "target_meme_station", "is_member", "weekend_flag", "moment_journee")
write(jsonlite::toJSON(vars[-which(vars %in% c("target_duree", "target_meme_station"))], pretty = TRUE), "data/variables_a_conserver.json")
data_preprocess <- data_bixi[, ..vars]
write_fst(data_preprocess, path = "data/data_preprocess.fst")



