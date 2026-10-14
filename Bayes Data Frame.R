bayes_df <- function(x, y){ # Data Frame for Bayes stuff
  
  if (x != length(y)) 
    
    message("Number of options should be equal to probabilities")
  
  D = data.frame(rep(1/3, 3), y)
  
  D[,3] = D[,1] * D[,2]
  
  d <- NULL
  
  for (n in 1:nrow(D)){ d <- c(d, D[n,3]/sum(D[,3])) }
  
  D[,4] <- d
  
  colnames(D) <- c("Prior", "Likelihood", "Unnorm", "Posterior")
  
  D
}
bayes_df(3, c(1/6, 1/8, 1/12))
