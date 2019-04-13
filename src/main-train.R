

# Load les packages -------------------------------------------------------

library(data.table)
library(caret)
library(fst)
library(stringi)
library(lubridate)
library(sf)


# Source les fonctions ----------------------------------------------------

source("src/extraction/load-data.R")
source("src/preprocessing/preprocessing.R")


# Importer les données ----------------------------------------------------

# Doit avoir le fichier 'LIMADMIN.shp' dans le repertoire passer en argument
# a la fonction load_data()
data_bixi <- load_data("data/")


# Split data --------------------------------------------------------------


# Preprocessing -----------------------------------------------------------

data_preprocess <- preprocessing(data_bixi, path_objects = "data/", train = TRUE)


# Modeling ----------------------------------------------------------------


