errLavaan = function(x, obj){
  require(mvtnorm)
  require(lavaan)
  est = lavTech(obj, "sigma.hat")
  Sigma = est[[1]]
  -mean(mvtnorm::dmvnorm(x, mean = rep(0, NCOL(Sigma)), sigma = Sigma, log = TRUE))
}
