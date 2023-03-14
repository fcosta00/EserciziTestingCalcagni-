source('utilities.R')
library(lavaan)
library(semPlot)
library(mvtnorm)
load('data_exam.Rdata')


data_bkp <- datax
datax <- data_bkp

str(datax)
head(datax)
summary(datax)
datax1 = scale(x = data_bkp[,1:8], center = TRUE, scale = FALSE)
datax[,1:8] = datax1 

# cfa multigruppo in tutti i modelli
# group = 'group'

#1

mod_1 = 'eta1 =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 +X8' 
mod_1_fit <- cfa( model = mod_1, data = datax, group = 'group', meanstructure = FALSE)
lavaan_checkConvergence(mod_1_fit)
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax)
100*(32/(p*(p+1)/2))
#2
mod_2 = 'eta1 =~ X1 + X2 + X3 + X4 \n eta2 =~ X5 + X6 + X7 +X8' 
mod_2_fit <- cfa( model = mod_2, data = datax, group = 'group')
lavaan_checkConvergence(mod_2_fit)
round(fitmeasures(object = mod_2_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_2_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax)
100*(50/(p*(p+1)/2))

mod_3 = 'eta1 =~ l1*X1 + l1*X2 +  l1*X3 +  l1*X4 \n eta2 =~  l2*X5 + l2*X6 + l2*X7+ l2*X8' 
mod_3_fit <- cfa( model = mod_3, data = datax, group = 'group')
lavaan_checkConvergence(mod_3_fit)
round(fitmeasures(object = mod_3_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_3_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax)
100*(38/(p*(p+1)/2))


#3
mod_4 = 'eta1 =~ l1*X1 + l1*X2 +  l1*X3 +  l1*X4 \n eta2 =~  l2*X5 + l2*X6 + l2*X7+ l2*X8 \n etabi =~ X1 + X2 +  X3 +  X4 \n etabi ~~ 0*eta1 \n etabi ~~ 0*eta2' 

err_m4 = kFold_validation(model_definition= mod_4, data= datax[,1:8], nfold= 7, error= 'montecarlo', force_crossValid= TRUE, B= 500)
err_m2 = kFold_validation(model_definition= mod_2, data= datax[,1:8], nfold= 7, error= 'montecarlo', force_crossValid= TRUE, B= 500)

boxplot(err_m2, err_m4)
t.test(err_m2, err_m4)

apply(cbind(err_m2,err_m4),2,sd) / apply(cbind(err_m2,err_m4),2,mean) # coefficiente di variazione


#4
# Il modello configurale è la baseline della procedura incrementale

# 2) invarianza debole
mod_1_weak = cfa(model = mod_2, data = datax,group = "group",group.equal="loadings")
anova(mod_2_fit,mod_1_weak)



#5

datag1 <- datax[datax$group == 'g1',]
mod_5 = 'eta1 =~ X1 + X2 + X3 + X4 \n eta2 =~ X5 + X6 + X7 +X8' 
mod_5_fit <- cfa( model = mod_5, data = datag1, group = 'group')

round(fitmeasures(object = mod_2_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_2_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax)
100*(50/(p*(p+1)/2))



mod_6 = 'eta1 =~ X1 + X2 + X3 + X4 \n eta2 =~ X5 + X6 + X7 +X8 \n etabi =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 +X8 \n etabi ~~ 0*eta1 \n etabi ~~ 0*eta2' 

err_m5 = kFold_validation(model_definition= mod_5, data= datax[,1:8], nfold= 5, error= 'montecarlo', force_crossValid= TRUE, B= 500)
err_m6 = kFold_validation(model_definition= mod_6, data= datax[,1:8], nfold= 5, error= 'montecarlo', force_crossValid= TRUE, B= 500)

boxplot(err_m5, err_m6)
t.test(err_m2, err_m4)

apply(cbind(err_m5,err_m6),2,sd) / apply(cbind(err_m5,err_m6),2,mean) # coefficiente di variazione


mod_6_fit <- cfa( model = mod_6, data = datag1, group = 'group')

round(fitmeasures(object = mod_6_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_2_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax)
100*(33/(p*(p+1)/2))
