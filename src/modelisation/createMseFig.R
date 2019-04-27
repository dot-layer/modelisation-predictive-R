createMseFig <- function(path = "static-files/"){
  
  x0 <- .75
  Ex0 <- dnorm(x0,1,1/2)
  
  preds <- sapply(1:50, function(k){
    
    replicate(2000, {
      train_dummy <- data.table(y = sample(0:1, 50, replace = T))
      train_dummy[, x := rnorm(n=50, mean=y, sd=1/2)]
      
      new_y_pred <- class::knn(train = matrix(train_dummy$x,ncol=1),
                               test = matrix(x0,ncol=1),
                               cl = train_dummy$y,
                               k = k)
      as.numeric(as.vector(new_y_pred))
    })
  })
  
  erreur_gen_x0 <- apply(preds, 2, function(y) mean((y - Ex0)^2))
  bias2_x0 <- apply(preds, 2, function(y) mean(y - Ex0)^2)
  var_x0 <- apply(preds, 2, function(y) var(y))
  
  dt <- data.table(val = c(erreur_gen_x0, bias2_x0, var_x0),
                   quantite = factor(rep(c("Gen","Biais2","Var"),
                                         each = length(var_x0)), labels = c("Biais au carré","Erreur de gen.","Variance")),
                   k = rep(1:length(var_x0), times = 3))
  
  g <- ggplot(dt, aes(x = k, y = val, col = quantite)) + geom_line(size=1.25) +
    theme_minimal() + ylab("")
  
  ggsave("static-files/dummy-mse.png", g, width = 5, height = 3.5)
  return(NULL)
}