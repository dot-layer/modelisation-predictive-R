

#' Fonction qui permet l'initialisation de l'env pour l'inférence
#' Cette fonction recoit en argument un path pour loader les différents objets/tables
#' 
#' 

source("src/collecte/load-merging-data.R")


init <- function(path_data, path_objects) {
  
  merging_data <- load_merging_data(path_data)
  
  variables_a_imputer <- jsonlite::fromJSON(paste0(path_objects, "valeurs_imputations.json"))
  objet_un_chaud <- readRDS(paste0(path_objects, "objet_un_chaud.rds"))
  vars_classif <- jsonlite::fromJSON(paste0(path_objects, "variables_a_conserver_classif.json"))
  vars_regression <- jsonlite::fromJSON(paste0(path_objects, "variables_a_conserver_regression.json"))
  
  model_glm <- readRDS(file = paste0(path_objects, "glm.rds"))
  model_xgb <- readRDS(file = paste0(path_objects, "xgb.rds"))
  
  list(
    merging_data = merging_data,
    variables_a_imputer = variables_a_imputer,
    objet_un_chaud = objet_un_chaud,
    vars_to_keep_classif = vars_classif,
    vars_to_keep_regression = vars_regression,
    model_glm = model_glm,
    model_xgb = model_xgb
  )
  
}