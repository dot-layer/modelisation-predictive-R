#' Preprocessing regression
#' Ce programme fait le preprocessing pour le modele de regression seulement
#' On assume ici que le preprocessing de base a été roulé

preprocessing_regression <- function(data, train_mode=TRUE, list_objects=NULL) {
  
  if (train_mode){
    
    # On crée les variables réponses
    setnames(data, "duration_sec", "target_duree")
    
    # Enlever les outliers
    valeur_max_outlier <- mean(data$target_duree) + 3 * sd(data$target_duree)
    print(paste(length(which(data$target_duree >= valeur_max_outlier | data$target_duree <= 0)), 
                "outliers ont été enlevés du jeu de données de régression."))
    data <- data[which(data$target_duree <= valeur_max_outlier | data$target_duree >= 0)]
    
    # Conserver les variables pertinentes à la modélisation
    vars <- c("target_duree", "is_member", "weekend_flag", "moment_journee", "start_station_code", "start_quartier_group")

    list(
      data_preprocess = data[, ..vars],
      vars_to_keep = vars[-which(vars %in% c("target_duree"))]
    )
    
  } else {
    
    vars <- list_objects$vars_to_keep_regression
    data[, ..vars]
    
  }

}