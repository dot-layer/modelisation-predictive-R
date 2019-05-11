library(data.table)
library(ggplot2)
library(fst)

library(glmnet)
library(xgboost)
library(mvtnorm)
library(class)

path_objects <- "src/modelisation/"

X_classif <- read.fst("src/modelisation/X_classif.fst", as.data.table = T)
y_classif <- readRDS("src/modelisation/y_classif.rds")

ind_val <- sample(nrow(X_classif),nrow(X_classif)/2)

X_val <- X_classif[ind_val,]
y_val <- y_classif[ind_val]

X_classif <- X_classif[-ind_val,]
y_classif <- y_classif[-ind_val]

xgbs <- xgboost(data = as.matrix(X_classif),
                label = y_classif,
                objective = "binary:logistic",
                eval.metric = "logloss",
                nrounds = 20,
                verbose = T)


prop_1 <- mean(X_classif == 1)
xgb_w <- xgboost(data = as.matrix(X_classif),
                 label = y_classif,
                 objective = "binary:logistic",
                 eval.metric = "logloss",
                 nrounds = 20,
                 verbose = T,
                 weight = (1-prop_1)*y_classif + prop_1*(1-y_classif))


param_grid <- expand.grid(nrounds = seq(5,20,5), maxdepth = seq(4,8,2))
xgbs <- lapply(1:nrow(param_grid), function(k){
  xgb <- xgboost(data = as.matrix(X_classif),
                 label = y_classif,
                 objective = "binary:logistic",
                 eval.metric = "logloss",
                 nrounds = param_grid[k,"nrounds"],
                 maxdepth = param_grid[k,"maxdepth"],
                 verbose = F) 
})



xgb <- xgboost(data = as.matrix(X_classif),
                 label = y_classif,
                 objective = "binary:logistic",
                 eval.metric = "logloss",
                 nrounds = 15,
                 verbose = T)

predd <- predict(xxgb, newdata = as.matrix(X_val))

hist(predd)

cm <- caret::confusionMatrix(as.factor(as.numeric(predd > .1)), as.factor(y_val))
cm$table

