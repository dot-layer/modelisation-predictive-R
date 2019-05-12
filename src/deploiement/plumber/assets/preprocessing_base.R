#' Preprocessing de base
#' Ce programme a pour but de faire le preprocessing commun au 2 modeles

preprocessing <- function(data, train_mode=TRUE, list_objects=NULL) {
  
  # Features engineering
  data[, `:=`(start_quartier = tolower(stri_replace_all_regex(start_quartier, "-| ", "")))]
  
  data[, `:=`(start_quartier = factor(gsub("[[:punct:]]", "", iconv(start_quartier, from="UTF-8", to="ASCII//TRANSLIT"))))]
  
  # week_start: 1=Lundi 7=Dimanche
  data[, `:=`(start_wday = lubridate::wday(start_date, week_start = 1),
              start_hour = hour(start_date_time))]
  
  data[, weekend_flag := as.integer(start_wday >= 6)]
  
  data[, moment_journee := "nuit"]
  data[start_hour >= 6 & start_hour < 11, moment_journee := "matin"]
  data[start_hour >= 11 & start_hour < 16, moment_journee := "journee"]
  data[start_hour >= 16 & start_hour < 23, moment_journee := "soir"]
  data[, moment_journee := factor(moment_journee)]
  
  # Regrouper les quartiers (basé sur l'analyse de données)
  data[start_quartier == "leplateaumontroyal", start_quartier_group := "plateau_mont_royal"]
  data[start_quartier == "villemarie", start_quartier_group := "ville_marie"]
  data[start_quartier %in% c("ahuntsiccartierville", "villeraysaintmichelparcextension", "mercierhochelagamaisonneuve", "rosemontlapetitepatrie"), start_quartier_group := "nord_est"]
  data[start_quartier %in% c("outremont", "cotedesneigesnotredamedegrace", "westmount", "lesudouest", "verdun", "lasalle"), start_quartier_group := "sud_ouest"]
  data[, start_quartier_group := factor(start_quartier_group)]

  
  if (train_mode) {
    
    # Calculer les valeurs d'imputation
    variables_a_imputer <- list()
    variables_a_imputer["start_quartier"] <- "autre"
    variables_a_imputer["start_quartier_group"] <- "autre"
    
    # Imputer les données manquantes
    for (col in names(variables_a_imputer)){
      data[is.na(get(col)), (col) := variables_a_imputer[[eval(col)]]]
    }
    
    # One-hot encoding (pour xgboost on doit faire cette transfo)
    objet_un_chaud <- dummyVars(as.formula("~ start_quartier_group + moment_journee"), data, 
                                fullRank = TRUE)
    data <- cbind(
      predict(objet_un_chaud, data),
      copy(data)[, (c("start_quartier_group", "moment_journee")) := NULL]
    )

    # Normalisation 
    # variables_a_normaliser <- list()
    # moyennes <- apply(data[, (variables_a_normaliser), with = F], 2, mean)
    # ecarts_types <- apply(data[, (variables_a_normaliser), with = F], 2, sd)
    # valeurs_normalisation <- list(
    #   moyennes = as.list(moyennes),
    #   ecarts_types = as.list(ecarts_types)
    # )
    # data <- lapply(names(valeurs_normalisation$moyennes), function(x) {
    #   data[, (x) := (get(x) - valeurs_normalisation$moyennes[[eval(x)]])/valeurs_normalisation$ecarts_types[[eval(x)]]]
    # })[[2]]
    

    list(
      data_preprocess = data,
      variables_a_imputer = variables_a_imputer,
      objet_un_chaud = objet_un_chaud
      # valeurs_normalisation = valeurs_normalisation
    )
    
  } else {
    
    # Imputer les données manquantes
    variables_a_imputer <- list_objects$variables_a_imputer
    for (col in names(variables_a_imputer)){
      data[is.na(get(col)), (col) := variables_a_imputer[[eval(col)]]]
    }
    
    # One-hot encoding
    objet_un_chaud <- list_objects$objet_un_chaud
    data <- cbind(
      predict(objet_un_chaud, data),
      copy(data)[, (c("start_quartier_group", "moment_journee")) := NULL]
    )
    
    # Normaliser les données
    # valeurs_normalisation <- list_objects$valeurs_normalisation
    # data <- lapply(names(valeurs_normalisation$moyennes), function(x) {
    #   data[, (x) := (get(x) - valeurs_normalisation$moyennes[[eval(x)]])/valeurs_normalisation$ecarts_types[[eval(x)]]]
    # })[[2]]
    
    data
    
  }
  
  
}
