lavaan_error = function(fitted_model = NULL, new_data = NULL){
  require(lavaan)
  PT = parTable(fitted_model)
  PT$free = 0L # fix all parameters
  PT$start = PT$ustart = PT$est # use estimates as starting values
  PT$est = PT$se = NULL # remove point and standard-error estimates
  
  test_model = update(fitted_model, model = PT,data = new_data,optim.force.converged = TRUE)
  
  Y0 = lavPredict(object = fitted_model,type = "ov",method = "ML")
  Y1 = lavPredict(object = test_model,type = "ov",method = "ML")
  err = norm(cov(Y0)-cov(Y1))
  return(err)
}

errLavaan = function(x, obj){
  require(mvtnorm)
  require(lavaan)
  est = lavTech(obj, "sigma.hat")
  Sigma = est[[1]]
  -mean(mvtnorm::dmvnorm(x, mean = rep(0, NCOL(Sigma)), sigma = Sigma, log = TRUE))
}

lavaan_error_MC = function(fitted_model = NULL, new_data = NULL, B = 500){
  require(lavaan); require(mvtnorm)
  n = fitted_model@SampleStats@nobs[[1]]
  Sigma = lavTech(fitted_model, "sigma.hat")[[1]]
  err = rep(NA,B)
  for(b in 1:B){
    Y_pred = mvtnorm::rmvnorm(n,sigma = Sigma)
    err[b] = norm(cov(Y_pred)-cov(new_data))  
  }
  return(err)
}


kFold_validation = function(model_definition=NULL,data=NULL,dwls=FALSE,nfold=2,error=c("mplus","zago","montecarlo"),force_crossValid=FALSE,B=100){
  error = match.arg(error)
  message(paste0("Note: Computing prediction error using ",error, " approach."))
  options(warn=-1)
  require(lavaan)
  n = dim(data)[1]
  noConv = 0
  folds = cut(seq(1,n), breaks=nfold, labels=FALSE)
  if(error=="montecarlo"){
    output = matrix(NA, nrow = nfold, ncol = B)
  }else{
    output = matrix(NA, nrow = nfold, ncol = 1)
  }
  
  for(b in 1:nfold){
    testIndexes = which(folds==b)
    testData = data[testIndexes, ]
    trainData = data[-testIndexes, ]
    
    if(dwls==FALSE){
      train_model = cfa(model = model_definition,data = trainData)
    }else{
      train_model = cfa(model = model_definition,data = trainData,ordered = colnames(trainData),estimator="DWLS")
    }
    conv = train_model@optim$converged
    noConv = noConv + as.numeric(!conv)
    if(conv==FALSE && force_crossValid==FALSE){
      message("ERROR: kFold cross validation cannot be computed if model gets no convergence. Please, modify the model in order to get convergence in the parameter estimation.")
      break
    }
    if(error=="zago"){
      output[b,] = lavaan_error(fitted_model = train_model,new_data = testData)  
    } else if(error=="mplus"){
      output[b,] = errLavaan(testData,train_model) 
    } else{
      output[b,] = lavaan_error_MC(fitted_model = train_model,new_data = testData, B=B)
    }
  }
  if(error=="montecarlo"){output = apply(output,1,mean)}
  message("Done. Number of failures to convergence for training model: ",noConv, "/",nfold)
  return(output)
}









