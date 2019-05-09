# Generate data for inference
# Models should be imported from previous steps of the tutorial

library(usethis)
library(xgboost)
library(data.table)
library(sf)

########################
# demo Iris
########################

data("iris")
target_raw <- iris$Species
iris_levels <- levels(target_raw)
target_int <- as.integer(target_raw) - 1

total_matrix <- model.matrix(~.-1-Species, data = iris)

dtrain <- xgboost::xgb.DMatrix(data = total_matrix, label = target_int)
watchlist = list(train = dtrain)
param <- list(max_depth = 3, num_class = 3, eta = 0.05,
              min_child_weight = 2, subsample = 0.5, colsample=1, silent = 1, nthread = 2,
              objective = "multi:softprob", eval_metric = "merror")
model_iris <- xgboost::xgb.train(params = param, data = dtrain, nrounds = 400, watchlist = watchlist, print_every_n = 25)

########################
# Bixi
########################
source("src/collecte/load-merging-data.R")
source("src/collecte/merge-data.R")
source("src/preprocessing/preprocessing_main.R")
source("src/init.R")
init_objects <- init("data/models/")

usethis::use_data(model_iris, iris_levels, preprocessing_main, model_glm, model_xgb, internal = T, overwrite = T)
