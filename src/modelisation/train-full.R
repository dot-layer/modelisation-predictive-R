#library(fst)
#library(caret)
#library(glmnet)
#library(xgboost)


# Importation des données -------------------------------------------------


data <- fst::read.fst(path = "data/data_preprocess.fst", as.data.table = T)




# Division du jeu de données (train/test) ---------------------------------

train_ind <- c(caret::createDataPartition(y = data$target_duree, times = 1, p = .8, list = FALSE))

length(train_ind)/nrow(data) # On a bien 80% du jeu de données

saveRDS(train_ind, "src/modelisation/train_ind.rds") # On garde en mémoire au cas...
saveRDS((1:nrow(data))[-train_ind], "src/modelisation/test_ind.rds") # Pour construire le jeu de données test en temps et lieu

X <- data[train_ind,-c("target_duree","target_meme_station")]
y_duree <- data$target_duree[train_ind]
y_meme <- data$target_meme_station[train_ind]
rm(data)


# Fit model ---------------------------------------------------------------

# vignette: https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
glm_full <- glmnet::glmnet(x = as.matrix(X), y = y_duree, family = "gaussian")


# vignette: https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
xgb_full <- xgboost::xgboost(data = X, label = y_meme, booster = "gblinear", objective = "binary:logistic")


# Model selection (cross-validation) --------------------------------------





# Model assessment --------------------------------------------------------


