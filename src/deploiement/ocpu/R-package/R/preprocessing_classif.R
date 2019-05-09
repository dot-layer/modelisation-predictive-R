#' Preprocessing classification
#' Ce programme fait le preprocessing pour le modele de classification seulement
#' On assume ici que le preprocessing de base a été roulé

preprocessing_classif <- function(data, train_mode=TRUE, list_objects=NULL) {
  
  if (train_mode) {
    
    # Créer des facteurs avec les stations
    data[, `:=`(start_station_code = factor(start_station_code),
                end_station_code = factor(end_station_code))]
    
    # On crée la variable réponse
    data[, target_meme_station := as.integer(start_station_code == end_station_code)]
    
    # Conserver les variables pertinentes à la modélisation
    vars <- c("target_meme_station", "is_member", "weekend_flag")
    vars <- c(vars, grep("start_quartier_group|moment_journee", colnames(data), value = TRUE))
    
    list(
      data_preprocess = data[, ..vars],
      vars_to_keep = vars[-which(vars %in% c("target_meme_station"))]
    )
    
  } else {
    
    vars <- list_objects$vars_to_keep_classif
    data[, ..vars]
    
  }
  
}