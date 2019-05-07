#' Preprocessing classification
#' Ce programme fait le preprocessing pour le modele de classification seulement
#' On assume ici que le preprocessing de base a été roulé

preprocessing_classif <- function(data, train_mode=TRUE, list_objects=NULL) {
  
  if (train_mode) {
    
    # On crée la variable réponse
    data[, target_meme_station := as.integer(start_station_code == end_station_code)]
    
    # One-hot encoding (pour xgboost on doit faire cette transfo)
    objet_un_chaud <- dummyVars(as.formula("~ start_quartier_group + moment_journee"), data, 
                                fullRank = TRUE)
    data <- cbind(
      predict(objet_un_chaud, data),
      copy(data)[, (c("start_quartier_group", "moment_journee")) := NULL]
    )
    
    # Conserver les variables pertinentes à la modélisation
    vars <- c("target_meme_station", "is_member", "weekend_flag")
    vars <- c(vars, grep("start_quartier_group|moment_journee", colnames(data), value = TRUE))
    
    list(
      data_preprocess = data[, ..vars],
      objet_un_chaud = objet_un_chaud,
      vars_to_keep = vars[-which(vars %in% c("target_meme_station"))]
    )
    
  } else {
    
    # One-hot encoding
    objet_un_chaud <- list_objects$objet_un_chaud
    data <- cbind(
      predict(objet_un_chaud, data),
      copy(data)[, (c("start_quartier_group", "moment_journee")) := NULL]
    )
    
    vars <- list_objects$vars_to_keep
    data[, ..vars]
    
  }
  
}