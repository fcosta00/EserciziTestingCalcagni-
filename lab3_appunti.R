load('Datasets-20221124/covariance_wisc.Rda')
source('Utilities-20221124/plot_lavaan_model.R')

library(lavaan)
library(semPlot)

S
colnames(S) = rownames(S) = c('DC', 'SO', 'MC', 'CI', 'CR', 'VC', 'LN', 'RM', 'CO', 'RS')

mod_M1_def = ' g =~ DC+SO+MC+CI+CR+VC+LN+RM+CO+RS ' 

mod_M1_fit = cfa(model = mod_M1_def, sample.cov = S, sample.nobs = 2200)

plot_lavaan_model(mod_M1_fit) #x11();

mod_M2_def =  ' verbal =~ SO+VC+CO \n
                percep =~ DC+CI+RM \n
                elab =~ MC+LN+CR+RS '

mod_M2_fit = cfa(model = mod_M2_def, sample.cov = S, sample.nobs = 2200)
plot_lavaan_model(mod_M2_fit) #x11();

mod_M3_def =  ' VCI =~ SO+VC+CO \n
                PRI =~ DC+CI+RM \n
                WMI =~ MC+LN \n
                PSI =~ CR+RS \n'

mod_M3_fit = cfa(model = mod_M3_def, sample.cov = S, sample.nobs = 2200)
plot_lavaan_model(mod_M3_fit) #x11();


#confronto modelli, hanno la stessa matrice di covarianza ma cambia tra i fattori latenti (se così non fosse sarebbero annidati)
cfa_fits = matrix(NA, 3,6)  #creo mattice nuovo
cfa_fits[1,] = fitmeasures(object = mod_M1_fit,
                           fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
cfa_fits[2,] = fitmeasures(object = mod_M2_fit,
                           fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
cfa_fits[3,] = fitmeasures(object = mod_M3_fit,
                           fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 

colnames(cfa_fits) = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')
rownames(cfa_fits) = c('M1', 'M2', 'M3')

#ranking indici
#RMSEA -> +basso 
#CFI
#AIC
#DF

#chi quadro non lo guardo perchè ci sono troppe campinamenti (2200)


modificationindices(object = mod_M3_fit, sort. = TRUE)

mod_M3_1_def =  'VCI =~ SO+VC+CO \n
                PRI =~ DC+CI+RM+SO \n
                WMI =~ MC+LN \n
                PSI =~ CR+RS \n'
mod_M3_1_fit = cfa(model = mod_M3_1_def, sample.cov = S, sample.nobs = 2200)
plot_lavaan_model(mod_M3_1_fit) #x11();

modificationindices(object = mod_M3_1_fit, sort. = TRUE)


mod_M3_2_def =  'VCI =~ SO+VC+CO \n
                PRI =~ DC+CI+RM+SO \n
                WMI =~ MC+LN \n
                PSI =~ DC+CR+RS \n'
mod_M3_2_fit = cfa(model = mod_M3_2_def, sample.cov = S, sample.nobs = 2200)
plot_lavaan_model(mod_M3_2_fit) #x11();

modificationindices(object = mod_M3_2_fit, sort. = TRUE)


cfa_fits = matrix(NA, 3,6)  #creo mattice nuovo
cfa_fits[1,] = fitmeasures(object = mod_M3_1_fit,
                           fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
cfa_fits[2,] = fitmeasures(object = mod_M3_2_fit,
                           fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
cfa_fits[3,] = fitmeasures(object = mod_M3_fit,
                           fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 

colnames(cfa_fits) = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')
rownames(cfa_fits) = c('M3_1', 'M3_2', 'M3')


#modello secondo ordine
mod_quarto = '  VCI =~ SO+VC+CO \n
                PRI =~ DC+CI+RM \n
                WMI =~ MC+LN \n
                PSI =~ CR+RS \n
                g =~ VCI + PRI + WMI + PSI'

mod_quarto_fit = cfa(model = mod_quarto, sample.cov = S, sample.nobs = 2200)

#phi ha zero tra le covarianze perccè i modelli sovraordinate azzera le covarianza tra latenti e lascia solo la diagonale

#matrice beta ha lambda delle latenti per le sovralatenti

x11();plot_lavaan_model(fitted_model = mod_quarto_fit)




