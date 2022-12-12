lavaan_checkConvergence = function(fitted_model=NULL){
  require(lavaan); require(matrixcalc)
  pbs = c("Parameters estimation","Negative residual error variances","Theta_delta not positive defined","Phi not positive defined")
  c1 = as.numeric(!fitted_model@optim$converged)
  PT = parTable(fitted_model)
  c2 = sum(PT[PT$op=="~~","est"]<0)
  out = lavInspect(fitted_model,what = "est")
  c3 = as.numeric(!matrixcalc::is.positive.definite(out$theta))
  c4 = as.numeric(!matrixcalc::is.positive.definite(out$psi))
  iid = which(c(c1,c2,c3,c4)==1)
  if(length(iid)==0){
    message("Convergence: Ok.")
  }else{
    if(c1==TRUE | c2==TRUE | c3==TRUE | c4==TRUE){
      message("Error: The current model did not get convergence. 
              Problems detected: ", length(iid), ". Type: ",
              paste(pbs[iid],collapse = ", "))
    }
  }
}
