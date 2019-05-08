

#' Fonction qui permet l'initialisation de l'env pour l'inférence
#' Cette fonction recoit en argument un path pour loader les différents objets/tables
#' 
#' 


init <- function(path) {
  
  variables_a_imputer <- jsonlite::fromJSON(paste0(path, "valeurs_imputations.json"))
  objet_un_chaud <- readRDS(paste0(path, "objet_un_chaud.rds"))
  # valeurs_normalisation <- jsonlite::fromJSON(paste0(path, "valeurs_normalisation.json"))
  vars_classif <- jsonlite::fromJSON(paste0(path, "variables_a_conserver_classif.json"))
  vars_regression <- jsonlite::fromJSON(paste0(path, "variables_a_conserver_regression.json"))
  
  model_glm <- readRDS(file = paste0(path, "glm.rds"))
  model_xgb <- readRDS(file = paste0(path, "xgb.rds"))
  
  list(
    variables_a_imputer = variables_a_imputer,
    objet_un_chaud = objet_un_chaud,
    # valeurs_normalisation = valeurs_normalisation,
    vars_to_keep_classif = vars_classif,
    vars_to_keep_regression = vars_regression,
    model_glm = model_glm,
    model_xgb = model_xgb
  )
  
}