split_dataset = function(data=NULL,prop=0.50,seedx=NULL){
  if(!is.null(seedx)){set.seed(seedx)}
  
  n=NROW(data)
  iid = sample(x = 1:n,size = floor(n*prop),replace = FALSE)
  iidC = setdiff(1:n,iid)
  data_A = data[iid,]
  data_B = data[iidC,]
  
  return(list(A=data_A,B=data_B))
}
