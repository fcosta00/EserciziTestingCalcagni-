#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) BFI: dati e loro struttura
# (B) BFI: esplorazione grafica struttura di correlazione
# (C) BFI: contenuto dgli items e soluzioni di clustering
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2022_2023/testing_psicologico/")
library(lavaan); library(semPlot)

# (A) BFI: dati e loro struttura ------------------------------------------
# 25 personality self report items taken from the International Personality Item Pool (ipip.ori.org). 
# Three additional demographic variables (sex, education, and age) are also included.
# The first 25 items are organized by five putative factors: Agreeableness, Conscientiousness, Extraversion, Neuroticism, and Opennness.
# The item data were collected using a 6 point response scale: 1 Very Inaccurate 2 Moderately Inaccurate 3 Slightly Inaccurate 4 Slightly Accurate 5 Moderately Accurate 6 Very Accurate.
# The items are from the ipip (Goldberg, 1999). The data are from the SAPA project (Revelle, Wilt and Rosenthal, 2010), collected Spring, 2010 ( https://sapa-project.org).

bfi = read.csv(file = "Datasets-20221124/bfi_reversed.csv",header = TRUE,sep = ",")
str(bfi)
head(bfi)

summary(bfi)
x11();par(mfrow=c(5,5))
for(j in 1:25){
  barplot(table(bfi[,j]),main=colnames(bfi)[j])  
}


# Divisione del dataset in due parti: 50% per la stima+valutazione, 50% per successive analisi
# vedi lab9.R, considerazione finale.
# set.seed(121)
# iid = 1:NROW(bfi)
# jjd1 = sample(x = iid,size = round(NROW(bfi)*0.50))
# jjd2 = setdiff(iid,jjd1)
# bfi_A = bfi[jjd1,] #prima parte 50%
# bfi_B = bfi[jjd2,] #seconda parte 50%
source("laboratorio/utilities/split_dataset.R")
out = split_dataset(data = bfi,prop = 0.50,seedx = 121)
bfi_A = out$A
bfi_B = out$B

# (B) BFI: esplorazione grafica struttura di correlazione -----------------
# Prima di scrivere e adattare ai dati un modello CFA confermativo per il bfi, esploriamo i dati alla ricerca di strutture di agglomerazione che ci possano essere utili
# per formulare modelli CFA (ipotesi) alternative da valutare comparativamente con quella prevista (teorica) del bfi. 
# Utilizzeremo anche metodi di clustering utili a tale scopo. Per una introduzione ai metodi di clustering:
# https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/

# Iniziamo dalla matrice di correlazione osservata:
bfi.cor = cor(bfi_A[,1:25],method = "spearman")

corrplot::corrplot(bfi.cor, method = "color") 
# Il grafico delle correlazioni ci mostra come le variabili siano naturalmente raggruppate a piccoli gruppi. Come atteso dalla struttura dei dati (sappiamo infatti che
# le variabili sono espressione di 5 fattori latenti), gli items si raggruppano in 5 gruppi, dove quello N1-N5 esprime correlazioni più forti mentre quello 01-05 sembra essere
# abbastanza debole e sovrapposto a quello N1-N5. Ci sono anche delle correlazioni tra il gruppo E1-E5 e A1-A5, indice del fatto che le variabili latenti sottostanti siano correlate.

heatmap(x = bfi.cor,symm = TRUE,hclustfun = function(x){hclust(x,method="ward.D2")}) #visualizziamo la struttura di clustering
# Il grafico delle correlazioni mediante heatmap e clustering gerarchico ci mostrano i gruppi di variabili che, a partire dalla loro distanza osservata nei dati,
# si raggruppano a formare clusters/gruppi (metodo di Ward: clusters hanno minima varianza). Notiamo, ad esempio, il gruppo N1-N5 mentre rispetto all'ispezione grafica precedente, il gruppo A1-A5 è stato
# sparso in altri gruppi. 

# Adattiamo lo stesso modello di clustering gerarchico alla matrice delle distanze (ottenute per trasformazione dalla matrice di correlazione):
bfi.hclust = hclust(d = dist(bfi.cor),method = "ward.D2")
plot(bfi.hclust) #dendrogramma 
rect.hclust(tree=bfi.hclust,k = 3) # 3 gruppi individuati in precedenza
bfi.grp = cutree(bfi.hclust,k = 3) #raggruppamento degli items secondo hclust
print(bfi.grp)
#L'ispezione mediante clustering alla Ward dunque suggerisce il seguente raggruppamento (soluzione a tre cluster)
names(bfi.grp[bfi.grp==1]) # primo cluster
names(bfi.grp[bfi.grp==2]) # secondo cluster
names(bfi.grp[bfi.grp==3]) # terzo cluster
# che risulta complessivamente differente dal raggruppamento previsto dal bfi a cinque fattori.
# E' interessante notare, comunque, che all'interno dei tre clusters ritroviamo alcuni dei fattori teorici del bfi.

# Dalla soluzione ottenuta per clustering gerarchico alla Ward notiamo come i 5 fattori bfi sono contenuti all'interno di 3 categorie sovraordinate. Questo potrebbe farci 
# esplorare, dopo aver stimato il modello CFA, la matrice delle covarianze Phi: è possibile che i fattori latenti siano correlati tra loro o che, in alternativa, ci siano 
# modelli con variabili latenti di secondo ordine da esplorare.

# Procediamo utilizzando un altro metodo per il clustering: il metodo del diametro massimo (complete linkage)
bfi.hclust = hclust(d = dist(bfi.cor),method = "complete")
plot(bfi.hclust) #dendrogramma 
rect.hclust(tree=bfi.hclust,k = 4) # 4 gruppi individuati in precedenza
bfi.grp = cutree(bfi.hclust,k = 4) #raggruppamento degli items secondo hclust
# L'ispezione grafica suggerisce il seguente raggruppamento (soluzione a quattro gruppi):
names(bfi.grp[bfi.grp==1]) # primo cluster
names(bfi.grp[bfi.grp==2]) # secondo cluster
names(bfi.grp[bfi.grp==3]) # terzo cluster
names(bfi.grp[bfi.grp==4]) # quarto cluster

# L'ispezione grafica suggerisce il seguente raggruppamento (soluzione a tre gruppi):
rect.hclust(tree=bfi.hclust,k = 3,border = "blue")
bfi.grp = cutree(bfi.hclust,k = 3)
names(bfi.grp[bfi.grp==1]) # primo cluster
names(bfi.grp[bfi.grp==2]) # secondo cluster
names(bfi.grp[bfi.grp==3]) # terzo cluster


# (C) BFI: contenuto dgli items e soluzioni di clustering -----------------
# Sulla base del contenuto degli items provare a comprendere se, da un punto di vista teorico, i raggruppamenti ottenuti mediante hclust
# possono avere un senso interpretativo:

# Soluzione 1 (Ward method):
# "A1" "A2" "A3" "A4" "A5" "E1" "E2" "E3" "E4" "E5"
# "C1" "C2" "C3" "C4" "C5" "O1" "O2" "O3" "O4" "O5"
# "N1" "N2" "N3" "N4" "N5"

# Soluzione 2 (Complete method 1):
# "A1" "A2" "A3" "A4" "A5" "C1" "C2" "C3" "C4" "C5" "E1" "E2" "E4"
# "E3" "E5" "O1" "O3"
# "N1" "N2" "N3" "N4" "N5"
# "O2" "O4" "O5"

# Soluzione 3 (Complete method 2):
# "A1" "A2" "A3" "A4" "A5" "C1" "C2" "C3" "C4" "C5" "E1" "E2" "E3" "E4" "E5" "O1" "O3"
# "N1" "N2" "N3" "N4" "N5"
# "O2" "O4" "O5"

# Soluzione secondo la struttura semantica del BFI:
# A1-A5
# C1-C5
# E1-E5
# N1-N5
# O1-O5



