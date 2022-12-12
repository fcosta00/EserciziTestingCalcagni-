library(lavaan)
library(psych)

load('Datasets-20221124/ISI.Rdata')
source('Utilities-20221124/kfold_crossvalidation.R')
source('Utilities-20221124/plot_lavaan_model.R')
source('Utilities-20221124/lavaan_checkConvergence.R')
library(semPlot)

#modelli bifactor, c'èm una latente che è legata a tutte le osservate ma non correla nessun'altra latente.
#Solitamente questa si chiama grouping factor come procedura

head(Y)
str(Y)

summary(Y)

#per standardizzare i dati (ora sono già standardizzati)
Y = scale(Y, center = TRUE, scale = FALSE) # per cfa i dati devono essere centrati

#cfa -> analisi della dimensionalità -> per studiare quante cose quel test sta misurando (quanti misurandi sta cogliendo il test)


#ISI: 2 misurandi -> difficoltà di addormentarsi
#                 -> distress da deprivazione di sonno

mod1 <- ' diff =~ wakeup_early + staying_asleep + falling_asleep + satisfied \n
          distress =~ worried + noticeable + inference \n'
# con il secondo eta stanno dicendo che l'insonnia non è legata solo all'insonnia ma anche a fattori di "personalità"


mod1_fit <- cfa(model = mod1, data = Y)
plot_lavaan_model(fitted_model = mo1_fit, what='std.all')


#modello 2 m

# i problemi di convergenza possono essere o segnalati da lavaan (standard error non calcolati)
#potrebbero pure non essere segnalati (std err molto elevau 20 in su circa)

# o si cambiano i dati o si cambia il modello

mod2 = "diff =~ wakeup_early + staying_asleep + falling_asleep\n
        distress =~ worried + noticeable + inference \n 
        dissatisfaction =~ satisfied + worried + wakeup_early"

mod2_fit = cfa(model = mod2, data=Y)

plot_lavaan_model(fitted_model = mod2_fit, what = "std.all")





mod3 = "sleep_diff =~ wakeup_early+staying_asleep+falling_asleep
        dissatisf =~ satisfied+worried+wakeup_early
        impact =~ worried+noticeable+inference
        eta =~ sleep_diff+dissatisf+impact"

mod3_fit = cfa(model = mod3,data = Y)
summary(mod3_fit, standardize=TRUE)
plot_lavaan_model(fitted_model = mod3_fit, what = "std.all")

fitmeasures(object = mod3_fit,fit.measures = c("chisq","df","pvalue","RMSEA","CFI")) #modello non converge

kf1 = kFold_validation(model_definition =  mod1,data =  Y, nfold = 10, dwls = FALSE, error = 'montecarlo', B = 100, force_crossValid = TRUE) 
#error: zago errore viene calcolato un'unica volta, montecarlo la procedura è simulativae avremo tanti errori (B) per ciascun fold
#force cross validation ignora i problemi di convergenza

kf2 = kFold_validation(model_definition =  mod2,data =  Y, nfold = 10, dwls = FALSE, error = 'montecarlo', B = 100, force_crossValid = TRUE) 
kf3 = kFold_validation(model_definition =  mod3,data =  Y, nfold = 10, dwls = FALSE, error = 'montecarlo', B = 100, force_crossValid = TRUE) 

boxplot(kf1,kf2,kf3)
#si analizza sia la media dell'errore sia la variabilità di essi. Con alta variabilità nessun modello è buono

#indici è grado di adattamento
# + alto è l'adattamento e - è la capacità predittiva
#con kfold valutiamo la capacità predittiva
#sempre tenere conto di capacità predittiva

#utilizziamo sempre montecarlo tranne quando specificato



#???? parametro ortogonal = true


mod4 <- ' diff =~ wakeup_early + staying_asleep + falling_asleep + satisfied \n
          distress =~ worried + noticeable + inference 
          csi =~ wakeup_early + staying_asleep + falling_asleep + satisfied + worried + noticeable + inference \n
          csi ~~ 0*diff \n
          csi ~~ 0*distress \n'
mod4_fit <- cfa(model = mod4, data = Y)
plot_lavaan_model(fitted_model = mod4_fit, what='std.all')

fitmeasures(object = mod4_fit, fit.measures = c("chisq","df","pvalue","RMSEA","CFI",'aic')) #modello non converge
fitmeasures(object = mod1_fit, fit.measures = c("chisq","df","pvalue","RMSEA","CFI",'aic')) #modello non converge
summary(object = mod4_fit, standardized = TRUE)
#si interpreti estensivamente -> perchè è buono? in che senso? cosa vuol dire? giustificare le motivazioni
kf4 = kFold_validation(model_definition =  mod4, data =  Y, nfold = 10, dwls = FALSE, error = 'montecarlo', B = 100, force_crossValid = TRUE)
boxplot(kf1,kf2,kf3,kf4)

#il modello bifactor come valore di adattamento ai dati sono sempre meglio rispetto a non bifactor

#idealmente se una cfa ha risultati buoni non ha senso migliorarlo, per logica della cfa. In questo caso seglieremo il non bifactor in quanto ha più gradi di libertà, è più facile interpretarlo e comunque si adatta bene
# CRITERIO DI OCCAM
#cecchare sempre previdibilità


#numero di nfold deve essere sensato per la numerosità del dataset 300 circa 3/4 fold
#B dovrebbe essere molto alto
