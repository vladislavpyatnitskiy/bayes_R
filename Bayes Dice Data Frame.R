bayes_df_dice <- function(x, y){ # Data Frame for Bayes stuff with dice
  
  # x is number of dice
  # y types of dices we use
  
  y = y[y>x]
  
  D = data.frame(rep(1/length(y), length(y)), 1/y)
  
  D[,3] = D[,1] * D[,2]
  
  d <- NULL
  
  for (n in 1:nrow(D)){ d <- c(d, D[n,3]/sum(D[,3])) }
  
  D[,4] <- d
  
  colnames(D) <- c("Prior", "Likelihood", "Unnorm", "Posterior")
  rownames(D) <- y
    
  D
}
bayes_df_dice(1, c(6, 8, 12))
