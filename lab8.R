#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) WISC-IV: dati
# (B) WISC-IV: valutazione invarianza 
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2022_2023/testing_psicologico/")
library(lavaan); library(semPlot)


# (A) WISC-IV: dati -------------------------------------------------------
load("laboratorio/data/wisc_simdata.Rdata")
str(wisc)
head(wisc)

# I dati si riferiscono a due campioni di 250 unità ciascuno simulati da una popolazione di bambini a sviluppo tipico (group="tipici") e da una popolazione di 
# bambini a sviluppo atipico (group="atipici"). I dati simulati comprendono i 10 items della WISC-IV (modulo II-B, slides 65-66) che sottendono, come da teoria,
# quattro fattori latenti (VCI, PRI, WMI, PSI). 
# Obiettivo del laboratorio è quello di valutare se i bambini a sviluppo atipico presentano medesima dimensione fattoriale dell'intelligenza. Le ragioni sono almeno due:
# a) se condividono la stessa struttura fattoriale dell'intelligenza, è possibile dunque valutare quest'ultima anche in situazioni per le quali la WISC-IV non è stata sviluppata (ragione psicometrica)
# b) se condividono la stessa struttura fattoriale dell'intelligenza, la ricerca clinica sulla popolazione atipica deve muovere i passi a partire dai risultati già noti
# ed ottenuti su popolazioni a sviluppo tipico. In questo senso, le due popolazioni non differirebbero per quanto riguarda la struttura dell'intelligenza (ragione clinica/di ricerca).
# Per far ciò occorre valutare se i due gruppi, tipici vs. atipici, condividono la medesima struttura fattoriale, risultato raggiungibile mediante l'analisi dell'invarianza.
# Per la parte teorica si vedano le slides 78-94 del modulo II-B.

# Prima di cominciare lo studio dell'invarianza, trasformiamo la variabile "group" da fattore a 2 livelli in variabile stringa (come vuole lavaan).
wisc$group = as.character(wisc$group)

# Scriviamo anche il modello generale della WISC-IV secondo la sintassi di lavaan:
model.wisc = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM \n WMI=~MC+LN \n PSI=~CR+RS" 


# (B) WISC-IV: valutazione dell'invarianza --------------------------------
# L'invarianza ha quattro livelli annidati, ciascuno dei quali è valutato in relazione al precedente seguendo una procedura incrementale:
# 1) invarianza configurale, 2) invarianza debole, 3) invarianza forte, 4) invarianza esatta
# Per semplicità procederemo adattando i quattro modelli di CFA per l'invarianza senza esplorare le stime ottenute e/o senza rappresentare graficamente i modelli ottenuti.
# Una volta valutato il livello di invarianza esistente nei dati, procederemo all'esplorazione grafica del modello finale insieme alla valutazione dei parametri del modello stimati.

# 1) invarianza configurale
cfa.wisc.configural = cfa(model = model.wisc,data = wisc,group = "group")
# Il modello configurale è la baseline della procedura incrementale

# 2) invarianza debole
cfa.wisc.weak = cfa(model = model.wisc,data = wisc,group = "group",group.equal="loadings")
anova(cfa.wisc.configural,cfa.wisc.weak)
# L'invarianza debole è stabilita, il test del chi-quadrato non consente di rigettare l'ipotesi H0 dell'equivalenza dei due modelli. Possiamo procedere oltre.

# 3) invarianza forte
cfa.wisc.strong = cfa(model = model.wisc,data = wisc,group = "group",group.equal=c("loadings","intercepts"))
anova(cfa.wisc.weak,cfa.wisc.strong)
# Anche l'invarianza forte è stabilita, procediamo oltre.

# 4) invarianza esatta
cfa.wisc.strict = cfa(model = model.wisc,data = wisc,group = "group",group.equal=c("loadings","intercepts","residuals"))
anova(cfa.wisc.strong,cfa.wisc.strict)
# L'invarianza esatta non è stabilita, come spesso accade nelle applicazioni reali. I due campioni condividono un modello di CFA ad invarianza forte con le medesime
# strutture fattoriali (matrice Lambda) e le medesime intercette delle variabili osservate (gli items, dunque, assumono lo stesso significato in entrambi i gruppi).
# I due gruppi possono essere dunque confrontati utilizzando i punteggi fattoriali.
# Note: Strong invariance, also called "scalar invariance", implies that the meaning of the construct (the factor loadings), and the levels of the underlying items (intercepts) 
# are equal in both groups. Consequently, groups can be compared on their scores on the latent variables. 

fitmeasures(object = cfa.wisc.strong,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))
semTools::reliability(cfa.wisc.strong)

summary(cfa.wisc.strong,standardized=TRUE)
# Guardiamo alla sezione "Intercepts" di entrambi i gruppi, in particolare le intercette (medie) dei fattori latenti VCI, PRI, WMI e PSI.
# Nel gruppo 1 (bambini tipici) sono fissate a zero: in questo modo il gruppo 1 funge da baseline per il confronto rispetto al secondo gruppo.
# Nel gruppo 2 (bambini atipici) sono libere e sono state per questo stimate: in termini numerici sono diverse da zero (rispetto al gruppo 1) ma statisticamente 
# non lo sono; infatti il test statistico Z non permette di rigettare l'ipotesi che le medie dei fattori WISC siano diversi tra tipici e atipici. 
# In conclusione, i due gruppi presentano valori medi di VCI,PRI,WMI,PSI non statisticamente differenti.


