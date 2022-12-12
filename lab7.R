#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Dati
# (B) Modelli
# (C) Valutazione finale
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2022_2023/testing_psicologico/")
library(lavaan); library(semPlot)



# (A) Dati ----------------------------------------------------------------
## Source: Chen, P. Y., Yang, C. M., & Morin, C. M. (2015). Validating the cross-cultural factor structure and invariance property of the Insomnia Severity Index: evidence based on ordinal EFA and CFA. Sleep medicine, 16(5), 598-603.
#The Insomnia Severity Index (ISI) is one of the most well-known instruments for assessing insomnia. 
#It contains seven items, all scored on a five-point scale, to measure the symptoms of insomnia.
load("laboratorio/data/ISI.Rdata")
head(Y)
Y = psych::rescale(x = Y,mean = 0,sd = 1) #standardizziamo i dati iniziali

#Obiettivo di questo laboratorio è quello di valutare psicometricamente quattro modelli alternativi per il test ISI, utilizzato solitamente nella valutazione clinica dell'insonnia.
#Procederemo all'inizio valutando due modelli noti in letteratura, quello a due e a tre fattori. Successivamente verranno valutati due modelli più complessi, 
#uno utilizza una struttura fattoriale di secondo ordine mentre il secondo utilizza una struttura fattoriale nota in letteratura come struttura bi-fattoriale (bifactor model).
#Una volta individuato il modello ottimale, si procederà calcolando gli indici di attendibilità (reliability) sulle scale così ottenute.

#Nota: In questo laboratorio vedremo alcuni problemi che si possono spesso incontrare nell'adattamento di modelli CFA ai dati.


# (B) Modelli -------------------------------------------------------------

# Two-factor model:
## Cit. Sierra, J. C., Guillén-Serrano, V., & Santos-Iglesias, P. (2008). Insomnia Severity Index: some indicators about its reliability and validity on an older adults sample. Revista de neurologia, 47(11), 566-570.
# The first factor included items assessing severity of sleeping difficulties. The items covered difficulties in initiating and maintaining sleep, as well as satisfaction with current sleeping pattern.  
# The second factor focused more on the impact of insomnia, and included items assessing day time interference and distress associated with insomnia, as well as how noticeable the sleeping problem was.
isi.twof = "
sleep_diff =~ wakeup_early+staying_asleep+falling_asleep+satisfied
impact =~ worried+noticeable+inference
"
isi.twof_fit = cfa(model = isi.twof,data = Y)
summary(isi.twof_fit,standardize=TRUE)
x11(); semPaths(object = isi.twof_fit,what="model",whatLabels = "std.all",style="lisrel",nCharNodes = 3,edge.label.cex = 1.25,sizeMan=5,sizeLat = 6,nDigits = 2,title = TRUE)

# Three-factor model:
## Fernandez-Mendoza, J., Rodriguez-Muñoz, A., Vela-Bueno, A., Olavarrieta-Bernardino, S., Calhoun, S. L., Bixler, E. O., & Vgontzas, A. N. (2012). The Spanish version of the Insomnia Severity Index: a confirmatory factor analysis. Sleep medicine, 13(2), 207-210.
#The first factor was termed the impact component and defined by items referring to distress, interference, and noticeability of insomnia. 
#The second factor was named the severity component, as it contained three items describing the primary symptoms of insomnia at different time points of the night (initial, middle, and terminal). 
#The third factor was called the (dis)satisfaction component and was defined by a single item about overall satisfaction with sleep. 
#According to Bastien et al., these three factors captured the main diagnostic criteria for insomnia.
isi.threef = "
sleep_diff =~ wakeup_early+staying_asleep+falling_asleep
dissatisf =~ satisfied+worried+wakeup_early
impact =~ worried+noticeable+inference
"
isi.threef_fit = cfa(model = isi.threef,data = Y)
summary(isi.threef_fit,standardize=TRUE)
x11(); semPaths(object = isi.threef_fit,what="model",whatLabels = "std.all",style="lisrel",nCharNodes = 3,edge.label.cex = 1.25,sizeMan=5,sizeLat = 6,nDigits = 2)

# Valutiamo quale tra i due modelli utilizzare per costruire il modello di secondo ordine
fitmeasures(object = isi.twof_fit,fit.measures = c("chisq","df","pvalue","RMSEA","CFI"))
fitmeasures(object = isi.threef_fit,fit.measures = c("chisq","df","pvalue","RMSEA","CFI"))
# ..scegliamo il modello con struttura a tre fattori latenti


# High-order factor model (second-level measurement model):
## More info at: https://www.frontiersin.org/articles/10.3389/fpsyg.2020.01357/full#h4
# The higher-order model (Thurstone, 1944) incorporates at least one superordinate (higher-order) factor and a series of subordinate factors upon which specified sub-group of items load.
# The higher-order model estimates two sets of loadings: those showing the relationships between the observed variables and the relevant grouping, or subordinate, factor, plus those showing the relationship between the higher-order factor and each of the subordinate factors. 
# Higher-order models are often used for theory testing (Brown, 2015), and they enable the researcher to explore theoretical understandings of the relationship between a series of sub-tests as distinct from one another, but also united by a common factor, which attempts to explain the scores in the higher-order factor.
# If the loadings between the higher-order and subordinate factors are satisfactorily high, it can be concluded that there is enough commonality between the sub-skills to justify this reporting both sub-scores and an overall score.
# It is important to note that in this model the observed variables act as indicators of the subordinate factors, and therefore, the commonality modeled by the higher-order factor is between the scales already established for each sub-group. This mediating role for the subordinate factors means that the higher-order factor, therefore, represents a “distilled” estimate of general ability rather than a more direct estimate, which accounts for commonalities between all observed variables as per the unidimensional model.
isi.so = "
sleep_diff =~ wakeup_early+staying_asleep+falling_asleep
dissatisf =~ satisfied+worried+wakeup_early
impact =~ worried+noticeable+inference
eta =~ sleep_diff+dissatisf+impact
"
isi.so_fit = cfa(model = isi.so,data = Y)
summary(isi.so_fit,standardize=TRUE)
#La stima dei parametri non è ottimale, nel sendo che l'algoritmo di ML non ha trovato convergenza (si veda il warning all'inizio del summary del modello).
#Proviamo a cambiare l'algoritmo di stima, utilizziamo ad esempio il metodo dei minimi quadrati generalizzati (GLS)
isi.so_fit = cfa(model = isi.so,data = Y,estimator="GLS")
summary(isi.so_fit,standardize=TRUE)
x11(); semPaths(object = isi.so_fit,what="model",whatLabels = "std.all",style="lisrel",nCharNodes = 3,edge.label.cex = 1.25,sizeMan=5,sizeLat = 6,nDigits = 2)

fitmeasures(object = isi.so_fit,fit.measures = c("chisq","df","pvalue","RMSEA","CFI"))
#L'introduzione del fattore si secondo ordine ha migliorato in generale l'adattamento del modello ai dati.
#A partire dal modello a tre fattori, come fatto ora, definiamo ed adattiamo ai dati un modello c.d. bi-fattoriale.

out1 = kFold_validation(model_definition = isi.twof,data = Y,nfold = 7,error = 2,force_crossValid = TRUE)
out2 = kFold_validation(model_definition = isi.threef,data = Y,nfold = 7,error = 2,force_crossValid = TRUE)
out3 = kFold_validation(model_definition = isi.so,data = Y,nfold = 7,error = 2,force_crossValid = TRUE)
summary(cbind(out1,out2,out3))


# Bifactor structure
## More info at: https://www.frontiersin.org/articles/10.3389/fpsyg.2020.01357/full#h4
# The bifactor model (Holzinger and Swineford, 1937), also described as a nested-factor (NF) model (Gustafsson and Åberg-Bengtsson, 2010; Brunner et al., 2012), incorporates a general factor, which loads directly onto all of the observed variables in the model.
# One of the defining features of the bifactor model is that the grouping factors in the model are hypothesized to be orthogonal (uncorrelated) with the general factor.
# The bifactor model does not offer a “simple structure” solution in which each observed variable only loads onto a single factor (Gustafsson and Åberg-Bengtsson, 2010). Observed variables, by design, in this model load onto more than one factor, meaning that the variance explanation is split between (at least) two latencies. Each observed variable in the bifactor model is an indicator of both the general factor and one grouping factor. This means that each observed variable has two loading estimates in the model; the first will show its relationship with the general factor and the second with its allocated grouping factor.
# A key distinguishing feature between the bifactor model and the higher-order model is that the general factor is hypothesized to load directly on each of the observed variables.
isi.bi = "
sleep_diff =~ wakeup_early+staying_asleep+falling_asleep
dissatisf =~ satisfied+worried+wakeup_early
impact =~ worried+noticeable+inference
eta =~ inference+noticeable+worried+satisfied+falling_asleep+staying_asleep+wakeup_early
"
isi.bi_fit = cfa(model = isi.bi,data = Y,estimator="GLS")
summary(isi.bi_fit,standardize=TRUE)
#Il summary del modello indica che questo non converge: notiamo che i gradi di liberta e la statistica test Chisq non è stata calcolata.
#Ciò è dovuto al fatto che il modello è non sovra-parametrizzato: il numero di parametri liberi (=29) è superiore al numero di parametri ammessi (=28).
#Il numero di parametri ammessi dal modello è pari a: 0.5*(NCOL(Y)*(NCOL(Y)+1))=28
# I gradi di libertà sono negativi!
# Non possiamo procedere oltre con questo modello.


# (C) Valutazione finale --------------------------------------------------
# Valutiamo quale tra i tre modelli adattati risulta migliore in termini di adattamento ai dati:

fitmeasures(object = isi.twof_fit,fit.measures = c("chisq","df","pvalue","RMSEA","CFI","AIC"))
fitmeasures(object = isi.threef_fit,fit.measures = c("chisq","df","pvalue","RMSEA","CFI","AIC"))
fitmeasures(object = isi.so_fit,fit.measures = c("chisq","df","pvalue","RMSEA","CFI","AIC")) #AIC: non può essere calcolato se ..estimator="GLS"

# Il modello di secondo ordine a tre fattori si adatta meglio ai dati. Provvediamo ad analizzarlo nel dettaglio:
summary(isi.so_fit,standardize=TRUE)
lavInspect(object = isi.so_fit,what = "est")
# Notiamo anzitutto due lambda negativi: ciò indica che le variabili originali presentano alcune correlazioni negative oppure che la matrice di correlazione presenta problemi di mal-condizoinamento (es.: matrice semidefinita positiva "al limite")
# Il warning ottenuto quando ..estimator="ML" difatti non doveva essere ignorato (cambiandolo con ..estimator="GLS").
# Inoltre ci sono varianze negative nella matrice Phi (=psi in lavaan sintax): altro elemento che suggerisce come il modello non sia idoneo sebbene gli indici complessivi suggeriscono di sceglierlo.
# Notiamo inoltre come il problema dei lambda negativi sia anche presente nel modello: isi.threef_fit. Ciò può altresì essere dovuto a problemi nella raccolta dei dati (problemi/errori presenti nel campione originario).
# Il modello isi.so_fit dunque non può essere considerato per ulteriori analisi (es.: analisi della reliability).
# Suggerimento: provare a migliorare il modello isi.twof_fit mediante modificationIndices().




