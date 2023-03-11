#1 
source('Utilities-20221124/utilities.R')
library(lavaan)
library(semPlot)
library(mvtnorm)

datax <- dataex9
datax <- datax[, 2:8]

str(datax)
head(datax)
summary(datax)

#analisi esplorativa
m_cor <- cor( datax[,1:6], method = 'pearson') # method='spearman' se ci sono variabili categoriali (default o pearson altrimenti)
D <- dist(m_cor, method = 'euclidean')
hc <- hclust(d = D, method = 'ward.D2')
plot(hc)
mod_2 <-hclust2lavaan(tree = hc, ngroups = 2)
# il clustering gerarchico mostra, tramite lanalisi delle altezze nel dendrogramma, come ci siano due gruppi principali


mod_1 <- "eta1 =~ y1+y2+y3+y4+y5+y6"
mod_2 <- "eta1 =~ y1+y2+y3+y4 \n eta2 =~ y5+y6"
mod_3 <- "eta1 =~ y1+y2+y3+y4 \n eta2 =~ y5+y6 \n etabi =~ y1+y2+y3+y4+y5+y6 \n etabi ~~ 0*eta1 \n etabi ~~ 0*eta2"

mod_1_fit <- cfa( model = mod_1, data = datax[,1:6]) #varibili continue
lavaan_checkConvergence(mod_1_fit)
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax[,1:6])
100*(12/(p*(p+1)/2))
#*il modello non è molto parsimonioso, usa 12 parametri, rispetto al 57% dei casi. 
#*Gli indici di adattamento indicano una adattamento non buono.
#*soprattutto il RMSEA è totalmente fuori range il cfi è più vicino a valori accettabili ma comunque fuori range
#*Questo potrebbe essere dovuto alle variabili y4 e y6 che riespetto alle altre rispecchiano molto meno la latente. Questo lo si può notare anche dalla elevata magnitudine delle varianze di errore

mod_2_fit <- cfa( model = mod_2, data = datax) #varibili continue
lavaan_checkConvergence(mod_2_fit)
round(fitmeasures(object = mod_2_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_2_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax[,1:6])
100*(13/(p*(p+1)/2))
#*il modello è meno parsimonioso del precedente. utilizza infatti il 13 parametri, il 61% del totale
#*il valore di rmsea è ottimo 0.016 e anche il valore di cfi è ottimo 0.99. analizzando i valori della lambda è tutto ottimo, l'unico sotto rispetto agli altri è y4 e infatti ha anche una varianza d'errore maggiore
#*l'unia possibile problematica del modello è la alta covarianza tra le due latenti che a livello di interpretazione è più complessa

mod_3_fit <- cfa( model = mod_3, data = datax) #varibili continue
lavaan_checkConvergence(mod_3_fit)
#*il modello presenta problemi di convergenza

#*scelgo il modello 2
err_m1 = kFold_validation(model_definition= mod_1, dwls = TRUE, data= datax[,1:6], nfold= 10, error= 'zago', force_crossValid= TRUE, B= 100)
err_m2 = kFold_validation(model_definition= mod_2, dwls = TRUE, data= datax[,1:6], nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
err_m3 = kFold_validation(model_definition= mod_3, dwls = TRUE, data= datax[,1:6], nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)

boxplot(err_m1, err_m2, err_m3)
t.test(err_m1, err_m2)

apply(cbind(err_m1,err_m2),2,sd) / apply(cbind(err_m1,err_m2),2,mean) # coefficiente di variazione



mod_4 <- "eta1 =~ y1+y2+y3+y4 \n eta2 =~ y5+y6 \n eta3 =~ eta1 + eta2"
mod_4_fit <- cfa( model = mod_4, data = datax[,1:6]) #varibili continue
lavaan_checkConvergence(mod_4_fit)
round(fitmeasures(object = mod_4_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_4_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax[,1:6])
100*(12/(p*(p+1)/2))
#* non c'è convergenza


reliability(mod_2_fit,return.total = TRUE)
# l'attendibilità è ottima. Sia per la prima che per la seconda latente. il valore totale è anch'esso ottimo

pr_ftt = lavPredict(object = mod_2_fit,type = "lv",method = "regression")
matrix2 = inspect(object = mod_2_fit, what="std.all")
zeta <- (pr_ftt %*% matrix2$psi) %*% c(1,1)

hist(bfi.eta[,1],main="eta1",ylab="",xlab="")
hist(bfi.eta[,2],main="eta2",ylab="",xlab="")




mod_1_conf = cfa(model = mod_1, data = datax,group = "z")

mod_1_weak = cfa(model = mod_1, data = datax,group = "z",group.equal="loadings")
anova(mod_1_conf,mod_1_weak)


mod_1_strong = cfa(model = mod_1, data = datax,group = "z",group.equal=c("loadings","intercepts"))
anova(mod_1_weak,mod_1_strong)

mod_1_strict = cfa(model = mod_1, data = datax,group = "z",group.equal=c("loadings","intercepts","residuals"))
anova(mod_1_strong,mod_1_strict)



