#' Fonction main pour le preprocessing
#' Objectif : Partir des données brutes et produire les données prêtes à
#' l'entrainement ou a la prédiction
#' 
#' data: données brutes
#' train_mode: TRUE pour entrainer le modele .. produit une liste d'objets
#' list_objects: Lors de l'entrainement on le laisse a NULL, sinon on passe la liste 
#' pour l'inférence


preprocessing <- function(data, train_mode=TRUE, list_objects=NULL) {
  
  if (train_mode) {
    
    # TODO: Definir la variable reponse 
    # classif: data[, target := as.integer(start_station_code == end_station_code)]
    # regression: setnames(data, "duration_sec", "target")
    
    #TODO: Traitement des données aberrantes (ID: 12)
    # A decommenter pour la regression seulement
    # valeur_max_outlier <- mean(data$target) + 3 * sd(data$target)
    # print(paste(length(which(data$target >= valeur_max_outlier | data$target <= 0)), 
    #             "outliers ont été enlevés du jeu de données de régression."))
    # data <- data[which(data$target <= valeur_max_outlier | data$target >= 0)]
    
    # TODO: Transformations des données (ID: 14)
    # Faire les transformations selon les connaissances acquises dans l'exploration
    
    # TODO: Implémentation de l'imputation (ID: 11)
    # Calculer (ou définir) les valeurs les valeurs d'imputation à appliquer aux données manquantes
    # Stocker ces valeurs dans une liste, par exemple:
    # variables_a_imputer <- list()
    # variables_a_imputer["variable_a_imputer"] <- mean(data$variable_a_imputer, na.rm = TRUE)
    
    # TODO: Imputer les données manquantes (ID: 11)
    # Iterer sur la liste pour remplacer les données manquantes par la valeur stockée dans la liste
    
    

    # TODO: Faire le traitement des données catégoriques(ID: 13)
    # Creer un objet a partir de la fonction caret::dummyVars pour one-hot encoder les variables à transformer
    # Apres avoir calculer l'objet, faire le predict sur le data et merger ces données avec le reste des données (sans les variables one-hot encoder)
    # objet_un_chaud <- caret::dummyVars()
    
    
    # Definir les varibles a conserver lors de la prédiction
    vars <- c("target", "is_member", "weekend_flag")
    vars <- c(vars, grep("start_quartier_group|moment_journee", colnames(data), value = TRUE))
    
    # Output de la fonction doit etre une liste contenant ces objets
    list(
      data_preprocess = data[, ..vars], #garder les variables spécifiées
      variables_a_imputer = variables_a_imputer, #variables à imputer
      objet_un_chaud = objet_un_chaud, #object permettant de faire la transformation one-hot
      vars_to_keep = vars[-which(vars %in% c("target"))]
    )
    
  } else {
    
    variables_a_imputer <- list_objects$variables_a_imputer
    # TODO: Imputer les données manquantes (ID: 11)
    # Iterer sur la liste pour remplacer les données manquantes par la valeur stockée dans la liste
    # Meme chose que plus haut
    
    objet_un_chaud <- list_objects$objet_un_chaud
    # TODO: Faire le traitement des données catégoriques(ID: 13)
    # Meme chose que plus haut
    
    
    # Retourner les données pretraitées
    vars <- list_objects$vars_to_keep_classif
    data[, ..vars]
    
  }
  
}