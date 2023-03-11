source('Utilities-20221124/utilities.R')
library(lavaan)
library(semPlot)
library(mvtnorm)
load('data_exam.Rdata')

datax <- Sy
#1
D <- dist(datax, method = 'euclidean')
hc <- hclust(d = D, method = 'ward.D2')
plot(hc)
mod_1 <-hclust2lavaan(tree = hc, ngroups = 2)

#2
mod_1_fit <- cfa( model = mod_1, sample.cov = datax, sample.nobs = 267, std.lv = TRUE) #variabili ordinali
# se non hai il dataset ma matrice di cor utilizzare sample.cov=X, sample.nobs=100,
# per ortogonale o orthogonal=TRUE o lat1 ~~ 0*lat2
# per UVI std.lv=TRUE
# non devono esserci variabili categoriali non ordinali nel dataset che si passa
lavaan_checkConvergence(mod_1_fit)
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax)
100*(21/(p*(p+1)/2))

#3
mod_2= "eta1 =~ l1*GESCOM.A+l1*GESCOM.B+l1*CONWOR.A+l1*CONWOR.B+l1*HIDPAT.A+l1*HIDPAT.B \n eta2 =~ l2*THI.ROUND+l2*THI.BLUE+l2*VOCABU.A+l2*VOCABU.B"
mod_2_fit= cfa(model = mod_2, sample.cov = datax, sample.nobs = 267, std.lv = TRUE)
lavaan_checkConvergence(mod_2_fit)
round(fitmeasures(object = mod_2_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )


# 4
mod_3 <- "eta1 =~ GESCOM.A+GESCOM.B+CONWOR.A+CONWOR.B+HIDPAT.A+HIDPAT.B \n eta2 =~ THI.ROUND+THI.BLUE+VOCABU.A+VOCABU.B \n etabi =~ GESCOM.A+GESCOM.B+CONWOR.A+CONWOR.B+HIDPAT.A+HIDPAT.B+THI.ROUND+THI.BLUE+VOCABU.A+VOCABU.B \n etabi ~~ 0*eta1 \n etabi ~~ 0*eta2"
mod_3_fit= cfa(model = mod_3, sample.cov = datax, sample.nobs = 267, std.lv = TRUE)
lavaan_checkConvergence(mod_3_fit)
round(fitmeasures(object = mod_3_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
100*(31/(p*(p+1)/2))

#5
reliability(mod_1_fit,return.total = TRUE)
