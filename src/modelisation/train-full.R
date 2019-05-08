# library(fst)
# library(caret)
# library(glmnet)
# library(xgboost)


# Importation des données -------------------------------------------------

data <- fst::read.fst(path = "data/data_preprocess.fst", as.data.table = T)


# Division du jeu de données (train/test) ---------------------------------

ind_test <- sample(nrow(data), .25*nrow(data))
length(ind_test)/nrow(data) # On a bien 75% du jeu de données

saveRDS(ind_test, "src/modelisation/ind_test.rds") # On garde en mémoire au cas...

# X <- data[train_ind,-c("target_duree","target_meme_station")]
# y_duree <- data$target_duree[train_ind]
# y_meme <- data$target_meme_station[train_ind]
# rm(data)




# *** Modifications *** ---------------------------------------------------

data$weekend_flag <- as.numeric(data$weekend_flag == data$weekend_flag[3])
data$moment_journee <- factor(data$moment_journee, labels = LETTERS[1:4])
my_cols <- grep("start_q", colnames(data)) # on note les colonnes de quartier
colnames(data)[my_cols] <- paste0("start_quartier_", 1:length(my_cols))



# Fit model ---------------------------------------------------------------

# vignette: https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
rhs1 <- paste0(colnames(data)[3:ncol(data)], collapse=" + ") # sans interactions
rhs2 <- paste0(rep(colnames(data)[3:5], each=length(my_cols)),":",
               colnames(data)[my_cols], collapse=" + ")  # interactions
rhs <- paste(rhs1, rhs2, sep = " + ")
rm(rhs1,rhs2)

f <- as.formula(paste("y_duree", rhs, sep=" ~ "))
X <- model.matrix(f, data)

glm <- glmnet::glmnet(x = X, y = y_duree, family = "gaussian", lambda=0)
coef(glm, s = 0)


plot(predict(glm,X[1:5000,]),data$target_duree[1:5000], xlim=c(1,7000))
hist(data$target_duree[1:5000])
hist(predict(glm,X[1:5000,]))

mean((predict(object = glm, newx = X, lambda = 0)-data$target_duree)^2)
sqrt(368400)/60




ratio <- mean(data$target_meme_station == 1)
# vignette: https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
xgb <- xgboost::xgboost(data = as.matrix(data[,-c(1:2)]), label = data$target_meme_station,
                        weight = (1-ratio)*y_meme + ratio*(1-y_meme),
                        booster = "gblinear", objective = "reg:logistic",
                        nrounds = 50, max_depth=10, alpha = .02, lambda = .02)



summary(xgb)
xgboost::xgb.ggplot.importance(importance_matrix = xgb.importance(model = xgb))

y_predict <- as.numeric(predict(xgb, newdata = as.matrix(data[,-c(1:2)])) >= .5)
mean(y_predict)
mean(data$target_meme_station)
#hist(y_predict)

library(caret)
cM <- confusionMatrix(as.factor(y_predict),as.factor(data$target_meme_station))


# Model selection (cross-validation) --------------------------------------





# Model assessment --------------------------------------------------------


