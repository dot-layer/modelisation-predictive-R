#' Fonction main pour le preprocessing
#' Objectif : Partir des données brutes et produire les données prêtes à
#' l'entrainement ou a la prédiction
#' 
#' data: données brutes
#' train_mode: TRUE pour entrainer le modele .. produit une liste d'objets
#' list_objects: Lors de l'entrainement on le laisse a NULL, sinon on passe la liste 
#' pour l'inférence


source("src/preprocessing/preprocessing_base.R")
source("src/preprocessing/preprocessing_classif.R")
source("src/preprocessing/preprocessing_regression.R")

preprocessing_main <- function(data, train_mode=TRUE, list_objects=NULL) {
  
  if (train_mode) {
    
    # Preprocessing de base 
    preprocessed_objects <- preprocessing(data, train_mode = TRUE)
    # Preprocessing pour les données de classif
    classif_objects <- preprocessing_classif(copy(preprocessed_objects$data_preprocess), train_mode = TRUE)
    # Preprocessing pour les données de regression
    regression_objects <- preprocessing_regression(copy(preprocessed_objects$data_preprocess), train_mode = TRUE)
    
    list_output = list(
      variables_a_imputer = preprocessed_objects$variables_a_imputer,
      objet_un_chaud_classif = classif_objects$objet_un_chaud,
      vars_to_keep_classif = classif_objects$vars_to_keep,
      vars_to_keep_regression = regression_objects$vars_to_keep,
      data_classif = classif_objects$data_preprocess,
      data_regression = regression_objects$data_preprocess
    )
    
  } else {
    
    # Preprocessing de base 
    preprocessed_objects <- preprocessing(data, train_mode = FALSE, list_objects = list_objects)
    # Preprocessing pour les données de classif
    classif_objects <- preprocessing_classif(copy(preprocessed_objects), train_mode = FALSE, list_objects = list_objects)
    # Preprocessing pour les données de regression
    regression_objects <- preprocessing_regression(copy(preprocessed_objects), train_mode = FALSE, list_objects = list_objects)
    
    list_output = list(
      data_classif = classif_objects,
      data_regression = regression_objects
    )
    
  }
  
  list_output
  
}