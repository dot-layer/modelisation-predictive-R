


preprocessing <- function(data, train_mode=TRUE, list_objects=NULL) {
  
  # Features engineering
  data[, `:=`(start_quartier = tolower(stri_replace_all_regex(start_quartier, "-| ", "")),
                   end_quartier = tolower(stri_replace_all_regex(end_quartier, "-| ", "")))]
  
  data[, `:=`(start_quartier = factor(gsub("[[:punct:]]", "", iconv(start_quartier, from="UTF-8", to="ASCII//TRANSLIT"))),
                   end_quartier = factor(gsub("[[:punct:]]", "", iconv(end_quartier, from="UTF-8", to="ASCII//TRANSLIT"))))]
  
  # week_start: 1=Lundi 7=Dimanche
  data[, `:=`(start_wday = wday(start_date, week_start = 1),
                        start_hour = hour(start_date_time))]
  
  data[, weekend_flag := as.integer(start_wday >= 6)]
  
  data[, moment_journee := 0L]
  data[start_hour >= 6 & start_hour < 23, moment_journee := 1L]
  data[start_hour >= 11 & start_hour < 16, moment_journee := 2L]
  data[start_hour >= 16 & start_hour < 23, moment_journee := 3L]
  
  
  if (train_mode) {
    
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
    
    # Imputer les données manquantes
    for (col in names(variables_a_imputer)){
      data[is.na(get(col)), (col) := variables_a_imputer[[eval(col)]]]
    }
    
    # One-hot encoding
    objet_un_chaud <- dummyVars(" ~ .", copy(data)[, (c("target_duree", "target_meme_station")) := NULL], 
                                fullRank = TRUE)
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
    data <- lapply(names(valeurs_normalisation$moyennes), function(x) {
      data[, (x) := (get(x) - valeurs_normalisation$moyennes[[eval(x)]])/valeurs_normalisation$ecarts_types[[eval(x)]]]
    })[[2]]
    
    # Conserver les variables pertinentes à la modélisation
    vars <- c("target_duree", "target_meme_station", "is_member", "weekend_flag", "moment_journee")
    vars <- c(vars, grep("start_quartier", colnames(data), value = TRUE))
    
    list(
      data_preprocess = data[, ..vars],
      variables_a_imputer = variables_a_imputer,
      objet_un_chaud = objet_un_chaud,
      valeurs_normalisation = valeurs_normalisation,
      vars_to_keep = vars[-which(vars %in% c("target_duree", "target_meme_station"))]
    )
    
  } else {
    
    # Imputer les données manquantes
    variables_a_imputer <- list_objects$variables_a_imputer
    for (col in names(variables_a_imputer)){
      data[is.na(get(col)), (col) := variables_a_imputer[[eval(col)]]]
    }
    
    # One-hot encoding
    objet_un_chaud <- list_objects$objet_un_chaud
    data <- data.table(predict(objet_un_chaud, data))
    
    # Normaliser les données
    valeurs_normalisation <- list_objects$valeurs_normalisation
    data <- lapply(names(valeurs_normalisation$moyennes), function(x) {
      data[, (x) := (get(x) - valeurs_normalisation$moyennes[[eval(x)]])/valeurs_normalisation$ecarts_types[[eval(x)]]]
    })[[2]]
    
    vars <- list_objects$vars_to_keep
    data[, ..vars]
    
  }
  
  
}
