#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) WISC-IV: modelli e stime
# (B) WISC-IV: confronto tra modelli
##########################################################



# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2022_2023/testing_psicologico/")
library(lavaan); library(semPlot)


# Dati WISC-IV (matrice di covarianza/correlazione, variabili standardizzate)
source("laboratorio/data/wisc_simdata.Rdata")
rownames(S) = colnames(S) = c("DC","SO","MC","CI","CR","VC","LN","RM","CO","RS")
# Nota: numero di osservazioni (unità statistiche) = 2200


# (A) WISC-IV: modelli e stime --------------------------------------------

# Primo modello
model.vci1 = "g=~SO+VC+CO+DC+CI+RM+MC+LN+CR+RS"
cfa.vci1 = cfa(model = model.vci1,sample.cov = S,sample.nobs = 2200)
semPaths(cfa.vci1,nCharNodes = 2,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)

# Secondo modello
model.vci2 = "verbal=~SO+VC+CO \n percep=~DC+CI+RM \n elab=~MC+LN+CR+RS" 
cfa.vci2 = cfa(model = model.vci2,sample.cov = S,sample.nobs = 2200)
semPaths(cfa.vci2,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)

# Terzo modello
model.vci3 = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM \n WMI=~MC+LN \n PSI=~CR+RS" 
cfa.vci3 = cfa(model = model.vci3,sample.cov = S,sample.nobs = 2200)
semPaths(cfa.vci3,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)

# Quarto modello
model.vci4 = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM \n WMI=~MC+LN \n PSI=~CR+RS \n g=~VCI+PRI+WMI+PSI" 
cfa.vci4 = cfa(model = model.vci4,sample.cov = S,sample.nobs = 2200)
semPaths(cfa.vci4,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)



# (B) WISC-IV: confronto tra modelli --------------------------------------

cfa.fit = matrix(NA,4,6) #matrice che conterrà gli indici di fit del modello
cfa.fit[1,] = fitmeasures(object = cfa.vci1,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))
cfa.fit[2,] = fitmeasures(object = cfa.vci2,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))
cfa.fit[3,] = fitmeasures(object = cfa.vci3,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))
cfa.fit[4,] = fitmeasures(object = cfa.vci4,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))
colnames(cfa.fit) = c("RMSEA","CFI","chisq","df","npar","AIC")
rownames(cfa.fit) = c("model1","model2","model3","model4")
print(cfa.fit)

# I modelli non sono annidati e non condividono tutti la stessa matrice di covarianza S. Ragione per cui conviene procedere guardando agli 
# indici RMSEA, CFI, AIC (soprattutto quest'ultimo): proseguiamo dunque scegliendo il modello 3.
semPaths(cfa.vci3,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)

# Valutiamo il modello 3 con gli indici di modifica:
modificationindices(object = cfa.vci3,sort. = TRUE)

# ..aggiungiamo il legame PRI=~SO (occorrerebbe farlo sulla base di ragioni teoriche!)
model.vci3_1 = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM+SO \n WMI=~MC+LN \n PSI=~CR+RS" 
cfa.vci3_1 = cfa(model = model.vci3_1,sample.cov = S,sample.nobs = 2200)
semPaths(cfa.vci3_1,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)

modificationindices(object = cfa.vci3_1,sort. = TRUE)

# ..aggiungiamo il legame PSI=~DC (occorrerebbe farlo sulla base di ragioni teoriche!)
model.vci3_2 = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM+SO \n WMI=~MC+LN \n PSI=~CR+RS+DC" 
cfa.vci3_2 = cfa(model = model.vci3_2,sample.cov = S,sample.nobs = 2200)
semPaths(cfa.vci3_2,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)

modificationindices(object = cfa.vci3_2,sort. = TRUE)
# ..ci fermiamo qui.

# Modello finale
summary(cfa.vci3_2,standardized=TRUE)
fitmeasures(object = cfa.vci3_2,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))
fitmeasures(object = cfa.vci3,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))
# Il fit del modello si è alzato ma occorrerebbe valutare questo miglioramento a partire da considerazioni teoriche sulla struttura fattoriale ottenuta
# chiedendosi, ad esempio, se ha senso includere un legame tra PSI e DC (es.: Perché? Quali ragioni possono esserci?)

# Valutiamo l'attendibilità delle scale dei due modelli
semTools::reliability(cfa.vci3)[1:2,] #selezioniamo solo indici alpha e omega
semTools::reliability(cfa.vci3_2)[1:2,]
# Notiamo come l'indice alpha è invariante nei due modelli mentre omega varia. Ciò è dovuto al fatto che alpha si calcola sulla matrice di covarianza S osservata
# mentre omega utilizza la matrice Lambda stimata dal modello CFA.

######################################################### NOTA BENE ##################################################################################################################
# Considerazione finale: abbiamo condotto la stima e la valutazione del modello sullo stesso insieme di dati ed abbiamo così utilizzato due volte i dati.
# Occorrerebbe invece, quando le dimensioni campionarie lo consentono, dividere in due porzioni il dataset ed effettuare sulla prima parte le valutazioni e il miglioramento del 
# modello mentre sulla seconda parte la stima finale dei parametri che possono essere usati per l'interpretazione dei risultati e per eventuali previsioni.
# Si veda a questo riguardo: https://people.bath.ac.uk/jjf23/papers/interface98.pdf
######################################################################################################################################################################################
