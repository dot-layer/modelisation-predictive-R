


preprocessing <- function(data, path_objects, train=TRUE) {
  
  # Features engineering
  data[, `:=`(start_quartier = factor(tolower(stri_replace_all_regex(start_quartier, "-| ", ""))),
                        end_quartier = factor(tolower(stri_replace_all_regex(end_quartier, "-| ", ""))))]
  
  # week_start: 1=Lundi 7=Dimanche
  data[, `:=`(start_wday = wday(start_date, week_start = 1),
                        start_hour = hour(start_date_time))]
  
  data[, weekend_flag := as.integer(start_wday >= 6)]
  
  data[, moment_journee := 0L]
  data[start_hour >= 6 & start_hour < 23, moment_journee := 1L]
  data[start_hour >= 11 & start_hour < 16, moment_journee := 2L]
  data[start_hour >= 16 & start_hour < 23, moment_journee := 3L]
  
  
  if (train) {
    
    # On crée les variables réponses
    setnames(data, "duration_sec", "target_duree")
    data[, target_meme_station := as.integer(start_station_code == end_station_code)]
    
    # Enlever les outliers
    valeur_max_outlier <- mean(data$target_duree) + 3 * sd(data$target_duree)
    print(paste(length(which(data$target_duree >= valeur_max_outlier | data$target_duree <= 0)), 
                "outliers ont été enlevés du jeu de données."))
    data <- data[which(data$target_duree <= valeur_max_outlier | data$target_duree >= 0)]
    
    # Calculer les valeurs d'imputation
    variables_a_imputer <- list()
    variables_a_imputer["start_quartier"] <- "autre"
    variables_a_imputer["end_quartier"] <- "autre"
    write(jsonlite::toJSON(variables_a_imputer, pretty = TRUE), paste0(path_objects, "valeurs_imputations.json"))
    
    # Imputer les données manquantes
    for (col in names(variables_a_imputer)){
      data[is.na(get(col)), (col) := variables_a_imputer[[eval(col)]]]
    }
    
    # One-hot encoding
    objet_un_chaud <- dummyVars(" ~ .", copy(data)[, (c("target_duree", "target_meme_station")) := NULL], 
                                fullRank = TRUE)
    saveRDS(objet_un_chaud, paste0(path_objects, "objet_un-chaud.rds"))
    data <- cbind(
      predict(objet_un_chaud, copy(data)[, (c("target_duree", "target_meme_station")) := NULL]),
      copy(data)[, (c("target_duree", "target_meme_station")), with = F]
    )
                       
    # Normalisation 
    variables_a_normaliser <- c("moment_journee", "weekend_flag")
    moyennes <- apply(data[, (variables_a_normaliser), with = F], 2, mean)
    ecarts_types <- apply(data[, (variables_a_normaliser), with = F], 2, sd)
    valeurs_normalisation <- list(
      moyennes = as.list(moyennes),
      ecarts_types = as.list(ecarts_types)
    )
    write(jsonlite::toJSON(valeurs_normalisation, pretty = TRUE), paste0(path_objects, "valeurs_normalisation.json"))
    data <- lapply(names(valeurs_normalisation$moyennes), function(x) {
      data[, (x) := (get(x) - valeurs_normalisation$moyennes[[eval(x)]])/valeurs_normalisation$ecarts_types[[eval(x)]]]
    })[[2]]
    
    # Conserver les variables pertinentes à la modélisation
    vars <- c("target_duree", "target_meme_station", "is_member", "weekend_flag", "moment_journee")
    vars <- c(vars, grep("start_quartier", colnames(data), value = TRUE))
    write(jsonlite::toJSON(vars[-which(vars %in% c("target_duree", "target_meme_station"))], pretty = TRUE), paste0(path_objects, "variables_a_conserver.json"))
    
  } else {
    
    # Imputer les données manquantes
    variables_a_imputer <- jsonlite::fromJSON(paste0(path_objects, "valeurs_imputations.json"))
    for (col in names(variables_a_imputer)){
      data[is.na(get(col)), (col) := variables_a_imputer[[eval(col)]]]
    }
    
    # One-hot encoding
    objet_un_chaud <- readRDS(paste0(path_objects, "objet_un-chaud.rds"))
    data <- data.table(predict(objet_un_chaud, data))
    
    # Normaliser les données
    valeurs_normalisation <- jsonlite::fromJSON(paste0(path_objects, "valeurs_normalisation.json"))
    data <- lapply(names(valeurs_normalisation$moyennes), function(x) {
      data[, (x) := (get(x) - valeurs_normalisation$moyennes[[eval(x)]])/valeurs_normalisation$ecarts_types[[eval(x)]]]
    })[[2]]
    
    vars <- jsonlite::fromJSON("data/variables_a_conserver.json")
    
  }
  
  data_preprocess <- data[, ..vars]
  
}