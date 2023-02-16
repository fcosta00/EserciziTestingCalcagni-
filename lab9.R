#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) BIG5: preparazione dei dati
# (B) BIG5: analisi dell'invarianza parziale
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2020_2021/testing_psicologico/")
library(lavaan); library(semPlot)


# (A) BIG5: preparazione dei dati -----------------------------------------
# Il dataset appartiene ad una ricerca più ampia condotta per valutare la capacità predittiva di frammenti di testo derivanti da profili social (es.: Facebook).
# Maggiori dettagli sono disponibili qui: https://github.com/jcl132/personality-prediction-from-text
# This data was collected (c. 2012) through on interactive online personality test. Participants were informed that their responses would be recorded and used for research at the begining of the test and asked to confirm their consent at the end of the test.
# The following items were rated on a five point scale where 1=Disagree, 3=Neutral, 5=Agree (0=missed). All were presented on one page in the order E1, N2, A1, C1, O1, E2...... 

# La struttura del BIG5 contiene 5 fattori ognuno dei quali è definito mediante 10 item su scala Likert:
#(A) Agreeableness: A1,..,A10
#(E) Extraversion: E1,..,E10
#(C) Conscientiousness: C1,..,C10
#(O) Openness to experience: O1,..,O10
#(N) Neuroticism: N1,..,N10

# Oltre agli item del big5 inventory sono disponibili alcune variabili categoriali:
# 1) race:	Chosen from a drop down menu. 1=Mixed Race, 2=Arctic (Siberian, Eskimo), 3=Caucasian (European), 4=Caucasian (Indian), 5=Caucasian (Middle East), 6=Caucasian (North African, Other), 7=Indigenous Australian, 8=Native American, 9=North East Asian (Mongol, Tibetan, Korean Japanese, etc), 10=Pacific (Polynesian, Micronesian, etc), 11=South East Asian (Chinese, Thai, Malay, Filipino, etc), 12=West African, Bushmen, Ethiopian, 13=Other (0=missed)
# 2) age: 	Entered as text (individuals reporting age < 13 were not recorded)
# 3) engnat:	Response to "is English your native language?". 1=yes, 2=no (0=missed)
# 4) gender:	Chosen from a drop down menu. 1=Male, 2=Female, 3=Other (0=missed)
# 5) hand:	"What hand do you use to write with?". 1=Right, 2=Left, 3=Both (0=missed)
# 6) country:	The participant's technical location. ISO country code.
# 7) source:	How the participant came to the test. Based on HTTP Referer. 1=from another page on the test website, 2=from google, 3=from facebook, 4=from any url with ".edu" in its domain name (e.g. xxx.edu, xxx.edu.au), 6=other source, or HTTP Referer not provided.
# Maggiori dettagli sono disponibili all'interno del file: BIG5_codebook.txt


# Carichiamo i dati disponbili nel formato comma-separated-values (csv)
big5data = read.csv(file = "laboratorio/data/BIG5_data.csv",header = TRUE,sep = "\t",dec = ".",stringsAsFactors = FALSE,na.strings = "0") 
#Note: 
# nel file originale i dati mancanti sono codificati con il numero 0 (zero)
# in questo casoa, il separatore di campo (tra una colonna ed un'altra) non è la virgola (valore di default) ma la tabulazione ("\t")
str(big5data)
# notiamo come le variabili categoriali non sono codificate correttamente (sono tipo integer) e dovrebbero essere convertite (mediante la funzione as.factor()) prima del loro utilizzo.
# In questo laboratorio useremo solo la variabile "country" che è già codificata come tipo character in maniera corretta.
# Prima di analizzare i dati, facciamo il reverse degli item che sono codificati a rovescio:
itms_toreverse = readLines(con = "laboratorio/data/BIG5_toreverse.txt") #leggiamo il file testuale contenente gli item da rovesciare
print(itms_toreverse)
# Effettuiamo l'operazione di reversing usando la funzione reverse.code() della libreria psych 
big5data[,itms_toreverse] = psych::reverse.code(keys = rep(-1,length(itms_toreverse)),items = big5data[,itms_toreverse])

# Eliminiamo i missing data mediante esclusione row-wise
big5data = big5data[complete.cases(big5data),]

# In questo laboratorio ci concentremo solo su due sottoinsiemi di risposte al BIG5, quelle relative a rispondenti danesi (country=="DK") e svedesi (country=="SE")
big5data_dk = big5data[big5data$country=="DK",8:57] #..le colonne da 8 a 57 sono quelle che contengono effettivamente gli item del BIG5 (ad esclusione delle variabili categoriali)
big5data_se = big5data[big5data$country=="SE",8:57]

head(big5data_dk)
# Le risposte agli item sono rilevate mediante scala a 5 punti con categorie ordinate. Per adattare un modello CFA su tali tipologie di dati,
# possiamo procedere in due modi: 
# (i) usando un metodo appropriato per stimare i parametri del modello (es.: DWLS)
# (ii) usando un metodo di quantificazione della scala prima di adattare il modello ai dati
# In questo laboratorio procederemo utilizzando una quantificazione basata sul calcolo della matrice di correlazione policorica. 
# Questa produce in output una statistica dei dati (la matrice di correlazione tra le variabili osservate del BIG5) e non il set di dati tipico partecipanti x variabili 
# che useremo come input della funzione cfa() di lavaan per adattare il modello ai dati.

# Calcoliamo la statistica dei dati per entrambi i sottoinsiemi:
big5data_dk_cormat = psych::polychoric(x = big5data_dk)$rho #gruppo danese
big5data_se_cormat = psych::polychoric(x = big5data_se)$rho #gruppo svedese

# Salviamo in una lista i dati che ci serviranno per le analisi successive
big5data_dkse = list(datacov=list(big5data_dk_cormat,big5data_se_cormat), #lista con le due matrici di correlazione
                     nobs=c(NROW(big5data_dk),NROW(big5data_se)), #numero di unità stats (osservazioni)
                     groupLabels=c("DK","SE"), #nomi dei due gruppi
                     itemMeans=list(apply(big5data_dk,2,mean),apply(big5data_se,2,mean)) #lista con le medie osservate per i due gruppi
                     )

str(big5data_dkse)

## ATTENZIONE: l'uso dello stimatore di massima verosimiglianza (ML) su matrici di correlazione policorica (come input)
## produce stime dei parametri del modello corrette ma standard errors (e misure che si basano sugli standard errors, come il chi2) scorretti!
## In caso di dati ordinali occorre perciò utilizzare stimatori tipo DWLS (come visto nei laboratori precedenti).
## Questo laboratorio, dunque, ha solo uno scopo didattico nel far vedere come si implementa la valutazione dell'invarianza parziale.


# (B) BIG5: analisi dell'invarianza parziale ------------------------------
# Breve tutorial: https://users.ugent.be/~yrosseel/lavaan/multiplegroup6Dec2012.pdf 
# A differenza di quanto fatto nel laboratorio in cui si è visto come procedere per l'analisi dell'invarianza completa, in questo laboratorio vedremo come effettuare
# l'analisi dell'invarianza quando uno degli step dell'invarianza completa fallisce. Questa procedura, nota come invarianza parziale, ci permette di analizzare fino a che punto
# le strutture fattoriali tra due o più gruppi sono invarianti almeno per un sottoinsieme (non piccolo) di parametri. Questa procedura è la prassi in molti casi di ricerca applicata.

# I due gruppi, in questo caso, sono rappresentati dal paese di provenienza dei rispondenti al test: danesi vs. svedesi.
# Obiettivo è capire se è possibile stabilire una qualche forma di invarianza tra danesi e svedesi per la struttura di personalità rappresentata dal BIG5.

# Definiamo dapprima il modello del BIG5 (variabile tipo character) usando la notazione di lavaan.
# Per velocizzare la scrittura possiamo usare una serie annidata di funzioni paste() -- maggiori info ?paste
big5_model = paste(
  paste("A",paste(paste0("A",1:10),collapse = "+"),sep = " =~ "), #(A) Agreeableness
  paste("E",paste(paste0("E",1:10),collapse = "+"),sep = " =~ "), #(E) Extraversion
  paste("C",paste(paste0("C",1:10),collapse = "+"),sep = " =~ "), #(C) Conscientiousness
  sep = " \n ") 
print(big5_model) # lavoriamo solo con un modello a tre fattori per semplicità

# 1) invarianza configurale
big5_configural = cfa(model = big5_model,sample.cov = big5data_dkse$datacov,sample.nobs = big5data_dkse$nobs,group = big5data_dkse$groupLabels,sample.mean = big5data_dkse$itemMeans)
print(big5_configural)

# 2) invarianza debole
big5_weak = cfa(model = big5_model,sample.cov = big5data_dkse$datacov,sample.nobs = big5data_dkse$nobs,group = big5data_dkse$groupLabels,sample.mean = big5data_dkse$itemMeans,
                group.equal=c("loadings"))
print(big5_weak)
anova(big5_configural,big5_weak)
# L'invarianza debole è stabilita, il test del chi-quadrato non consente di rigettare l'ipotesi H0 dell'equivalenza dei due modelli. Possiamo procedere oltre.

# 3) invarianza forte
big5_strong = cfa(model = big5_model,sample.cov = big5data_dkse$datacov,sample.nobs = big5data_dkse$nobs,group = big5data_dkse$groupLabels,sample.mean = big5data_dkse$itemMeans,
                  group.equal=c("loadings","intercepts"))
print(big5_strong)
anova(big5_weak,big5_strong)
# L'invarianza forte non è stabilita e dunque i due modelli differiscono rispetto alle intercette del modello (tau).
# In termini generali, non è possibile affermare che i due modelli siano invarianti in senso completo. Tuttavia potremmo chiederci se lo sono
# per un sottoinsieme ristretto di parametri tau, ossia potremmo provare ad escludere quegli item che non sono invarianti e tenere nell'analisi sono quelli che sono invarianti per i due gruppi.
# Cioè potremmo chiederci se i due gruppi sono parzialmente invarianti rispetto alla struttura fattoriale del BIG5.
# Come primo step, andiamo ad individuare quei parametri che non sono invarianti. In questo caso, valutando l'invarianza forte, andremo a cercare quelle
# intercette (tau) che non sono invarianti. Se invece valutassimo l'invarianza debole, andremmo a cercare quei loadings (lambda) che non sono invarianti.
# I parametri non invarianti sono quelli che non devono essere vincolati nella procedura di stima, ossia sono quelli che non devono essere uguali in entrambi i gruppi.
# Usiamo la procedura degli indici di modifica per valutare quale parametro liberare:
X = modificationIndices(object = big5_strong,free.remove = FALSE,sort. = TRUE)
print(X) 
# Poiché l'output della funzione modificationIndices() è troppo esteso in questo caso, per agevolare la ricerca delle intercette
# utilizziamo il parametro op="~1" nella funzione modificationIndices():
X = modificationIndices(object = big5_strong,free.remove = FALSE,sort. = TRUE, op = "~1")
print(X) #..ora abbiamo solo le intercette in output
# Inviduiamo le coppie di item (stessi item) che hanno un alto indice di modifica (mi) in entrambi i gruppi (group):
# possiamo notare come gli item A1 e O3 presentino, in entrambi i gruppi, valori di mi maggiori (se confrontanti con i restanti item).
# Proviamo dunque a liberare le intercette (tau) relative agli item appena individuati e ri-adattiamo il modello ai dati.

big5_strong_b = cfa(model = big5_model,sample.cov = big5data_dkse$datacov,sample.nobs = big5data_dkse$nobs,group = big5data_dkse$groupLabels,sample.mean = big5data_dkse$itemMeans,
                  group.equal=c("loadings","intercepts"), group.partial=c("A1~1"))
print(big5_strong_b)
anova(big5_weak,big5_strong_b)
# Notiamo come i due gruppi siano ancora non invarianti in senso forte. 
# Procediamo liberando altre intercette usando il modello appena adattato.
X = modificationIndices(object = big5_strong_b,free.remove = FALSE,sort. = TRUE, op = "~1")
print(X) 

big5_strong_c = cfa(model = big5_model,sample.cov = big5data_dkse$datacov,sample.nobs = big5data_dkse$nobs,group = big5data_dkse$groupLabels,sample.mean = big5data_dkse$itemMeans,
                    group.equal=c("loadings","intercepts"), group.partial=c("A1~1","E3~1","C4~1","C2~1","A9~1"))
anova(big5_weak,big5_strong_c)
# Il test del chi-quadrato non consente ora di rigettare l'ipotesi H0 dell'equivalenza dei due modelli. Abbiamo stabilito che i due modelli sono invarianti parzialmente in senso forte.

summary(big5_strong_c)
# Osservando la sezione "Intercepts:" per entrambi i gruppi notiamo come gli item liberati presentino ora intercette stimate differenti.
# Ad esempio, l'item A1 nel gruppo 1 presenta tau = 2.115 mentre nel gruppo 2 presenta tau = 2.421.

# I due campioni possono essere confrontati in termini di medie latenti (ossia le medie dei fattori latenti):
# nella sezione "Intercepts:" troviamo anche le medie latenti per ciascun fattore dei BIG5 (A,E,C,O,N) per entrambi i gruppi
# dove il gruppo 1 fa da baseline rispetto al gruppo 2 (in questo caso stiamo valutando la variazione delle medie latenti nei due gruppi).
# Nel gruppo 2, per ciascun fattore latente e la relativa media abbiamo anche un test Z per la differenza rispetto al gruppo 1 (baseline):
# possiamo notare come i due gruppi differiscano in termini di A,E,C,N (soglia: |z|>2.0 o p-value < 0.05), con A e N maggiormente presenti nel gruppo SE.

# Possiamo anche fare un grafico delle variazioni delle medie latenti:
X = lavInspect(object = big5_strong_c,what = "est") # estrazione dei parametri stimati del modello 
str(X) #i campi di nostro interesse sono alpha nel gruppo 1 e 2
y = X[[2]]$alpha #medie latenti gruppo 2 (gruppo 1 sono zero)
x11();barplot(beside = TRUE,space = 0.5,height = y,bty="n",col = c(10,3,4,5,8));abline(h = 0,lty=2,col=1)
legend("bottomright",legend = rownames(y),fill = c(10,3,4,5,8))

# 4) Invarianza esatta
big5_exact = cfa(model = big5_model,sample.cov = big5data_dkse$datacov,sample.nobs = big5data_dkse$nobs,group = big5data_dkse$groupLabels,sample.mean = big5data_dkse$itemMeans,
                    group.equal=c("loadings","intercepts","residuals"), group.partial=c("A1~1","E3~1","C4~1","C2~1","A9~1"))
anova(big5_strong_c,big5_exact)
# L'invarianza parziale esatta non è stabilita: i punteggi latenti (factor scores) tra danesi e svedesi non possono essere confrontati tra loro. 
# Se ci fosse stata invarianza esatta, i punteggi totali osservati (tramite somma) del BIG5 sarebbero potuti essere calcolati ed interpretati (in quel caso i due gruppi avrebbero condiviso le stesse dimensioni latenti).
# Nota: il calcolo dei punteggi totali osservati (tramite somma) ha senso in quanto entrambi i modelli condividono la stessa parte unica e residua.

# Potremmo in questo caso procedere come fatto finora, ossia cercando di stabilire l'invarianza esatta parziale. 
# Ci fermiamo tuttavia qui poiché questo laboratorio ha carattere meramente didattico ed esemplificativo sulla procedura di valutazione dell'invarianza parziale.








