

plot_lavaan_model = function(fitted_model=NULL,what=c("std.all","est"),cex_label=1.2,ndigits=2,...){
  require(semPlot)
  wm = match.arg(what)
  semPaths(fitted_model,nCharNodes = 3,what = "model", whatLabels = wm,
           edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = ndigits,...)
}
