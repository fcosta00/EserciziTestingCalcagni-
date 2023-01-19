#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Sintassi di lavaan: modello e stime
# (B) Sintassi lavaan: valutazione del modello
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2022_2023/testing_psicologico/")
library(lavaan); library(semPlot)


# (A) Sintassi di lavaan: modello e stime ---------------------------------
data("HolzingerSwineford1939") #carichiamo il dataset HolzingerSwineford1939. Ulteriori info: ?HolzingerSwineford1939
str(HolzingerSwineford1939)
head(HolzingerSwineford1939)

# CFA unidimensionale: scala Visual

model.visual = "visual =~ x1+x2+x3" 
# Oggetto tipo character contenente il modello: il simbolo =~ indica relazione fattore-item, + indica la componente additiva della scala, a destra di =~ scriviamo il nome del
# misurando/fattore latente mentre a sinistra di =~ scriviamo le variabili (stesso nome che troviamo nel dataset) che compongono il fattore

cfa.visual = cfa(model = model.visual,data = HolzingerSwineford1939)
# L'adattamento del modello CFA ai dati avviene mediante la funzione cfa() che richiede in input i dati su cui adattare il modello. 
# In alternativa, avremmo potuto usare la matrice di correlazione (se non ci fossero stati i dati) specificando anche il numero di osservazioni:
S = cov(HolzingerSwineford1939[,7:9])
cfa.visual = cfa(model = model.visual,sample.cov = S,sample.nobs = 301)

# Per visualizzare l'output di cfa() utilizziamo summary():
summary(cfa.visual,fit.measures=TRUE)
# L'output presenta tutto quanto è necessario per valutare il modello:
# la sezione iniziale riporta le informazioni relative a parametri, gradi di libertà, algoritmo di stima, test del chi-quadrato
# la sezione "Latent Variables" riporta le informazioni relative alla scala (Nota: Estimate = lambda) nel solito formato visto anche per lm()
# la sezione "Variances" riporta le informazioni relative alle varianze delle osservabili e delle latenti (con i corrispondenti standard errors)
# Note: 
# 1) lambda_x1 = 1 sempre a causa della parametrizzazione ULI (default). 
# 2) Il calcolo degli standard errors (Std.Err) dei parametri in questo caso avviene utilizzando le proprietà asintotiche dello stimatore Lambda: Estimate (che sono i lambda del modello) si distribuiscono asintoticamente come Normali con media uguale a Estimate e Varianza pari a Std.Err^2.
# 3) L'inferenza su Estimate (i lambda del modello) è fatta sfruttando la dist. asintotica normale standardizzata (z-value) sotto H0: lambda_jk=0. Il valore P(>|z|) è il p-value associato a ciascuna ipotesi sui lambda.

# ..visualizzazione output usando l'opzione standardized=TRUE
summary(cfa.visual,standardized=TRUE)
# Vengono aggiunte due colonne relative alla standardizzazione dei parametri del modello (Std.lv, Std.all). Quest'opzione permette di scalare i parametri nell'intervallo
# [0,1] per ragioni di comparabilità. Ci sono due metodi per la standardizzazione, noi utilizzeremo e valuteremo "Std.all".

# Per ottenere il grafico del modello CFA:
semPaths(object = cfa.visual,nCharNodes = 3,what = "model", whatLabels = "est",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1) #con stime grezze
semPaths(object = cfa.visual,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1) #con stime standardizzate
# E' possibile personalizzare in molti modi il grafico. Maggiori info su: ?semPaths

# La funzione cfa() utilizza in automatico la parametrizzazione ULI. Per utilizzare in alternativa la parametrizzazione UVI occorre specificare:
cfa.visual = cfa(model = model.visual,data = HolzingerSwineford1939,std.lv=TRUE)
summary(cfa.visual,standardized=TRUE)
# Notiamo come lambda_x1 = 0.724, diverso da 1, mentre VAR[visual] = 1.00 a causa della parametrizzazione ULI.

# Per estrarre le matrici dei parametri del modello {Lambda, Theta_delta, Phi} usiamo la funzione inspect():
cfa.matrices = inspect(object = cfa.visual,what="est") #stime grezze non standardizzate
cfa.matrices = inspect(object = cfa.visual,what="std.all") #stime standardizzate secondo std.all
print(cfa.matrices)
# cfa.matrices è una lista contenente le matrici stimate del modello

# Per calcolare la matrice di covarianza riprodotta dal modello (implied covariance matrix) Sigma_hat:
Sigma_hat = fitted(cfa.visual)
Sigma_hat = inspect(cfa.visual,what = "fitted") #comando simile

# Per stimare i valori eta delle variabili latenti per ciascun individuo (usando il metodo standard "regression"):
eta = lavPredict(object = cfa.visual,newdata = HolzingerSwineford1939[,c(7:9)],type = "lv",method = "regression")

# Possiamo anche fissare alcuni dei parametri del modello, imponendo vincoli nella definizione di quest'ultimo, come segue:
model.visual = "visual =~ x1+x2+0.9*x3" #fissando lambda_x3 ad un valore fisso (non stimato)
model.visual = "visual =~ x1+x2+0*x3" #fissando lambda_x3 ad un valore fisso (non stimato) pari a zero

model.visual = "visual =~ x1+x2+x3 \n x1~~x2" #correlando gli errori delle variabili x1,x2
# Il simbolo ~~ indica correlazione tra variabili mentre il simbolo \n indica riga-a-capo ed è indispensabile per scrivere il modello utilizzando più di una linea

# La stima è fatta mediante cfa() come in precedenza.

# Per includere le intercette tau nella stima possiamo utilizzare il parametro
cfa.visual = cfa(model = model.visual,data = HolzingerSwineford1939,meanstructure=TRUE)
summary(cfa.visual)
# Il summary del modello contiene ora il campo "Intercepts" con le stime delle medie degli items. 
# Nota: le intercette delle variabili latenti sono fissate a zero di default.


# CFA bidimensionale: scala Visual e Textual

model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6" # il simbolo \n indica riga-a-capo ed è indispensabile per scrivere il modello utilizzando più di una linea
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939)
summary(cfa.visual.text,standardized=TRUE)
# Il summary del modello ora contiene un altro campo "Covariances", la matriche Phi del modello CFA contenente le covarianze tra variabili latenti.

# Possiamo fissare alcuni dei parametri della matrice Phi scrivendo opportunamente il modello:
model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6 \n visual ~~ 0*text" 
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939)
summary(cfa.visual.text,standardized=TRUE)

# ..in alternativa si può usare il parametro orthogonal=TRUE direttamente in cfa()
model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6" 
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939,orthogonal=TRUE) # variabili latenti sono incorrelate
summary(cfa.visual.text,standardized=TRUE)

# ..in maniera analoga, fissando il parametro di covarianza ad un valore noto:
model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6 \n visual ~~ 0.8*text" 

# ..per aggiungere items ad una scala, ad esempio:
model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6+x1+x3" # la variabile latente "text" ora è composta anche dagli items x1,x3
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939)
semPaths(object = cfa.visual.text,nCharNodes = 3,what = "model", whatLabels = "est",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1) #con stime grezze

# Un modello con fattore sovraordinato (esempio modello M4, slide 74 del modulo II-B) è un modello CFA con l'aggiunta di 
# una variabile latente di secondo ordine che predice quelle di primo ordine. Ad esempio:
model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6 \n f0 =~ visual+text" # f0 è la variabile di secondo ordine
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939)
semPaths(object = cfa.visual.text,nCharNodes = 3,what = "model", whatLabels = "est",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1) #con stime grezze

# Il messaggio di warning: 
# "Could not compute standard errors! The information matrix could
# not be inverted. This may be a symptom that the model is not
# identified."
# indica che il modello non è identificato (ad es. a causa della sovraparametrizzazione) e segnala che le stime (soprattutto quelle degli std.err) potrebbero non essere accurate.
# Questo warning talvolta può essere ignorato ma il più delle volte indica che il modello CFA è definito in malo modo.

cfa.matrices = inspect(object = cfa.visual.text)
# I coefficienti che legano il fattore di secondo ordine f0 a quelli di primo ordine Visual e Text sono contenuti nel campo "beta" della lista di output


# (B) Sintassi lavaan: valutazione del modello ----------------------------

model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6" # il simbolo \n indica riga-a-capo ed è indispensabile per scrivere il modello utilizzando più di una linea
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939)

# Per calcolare gli indici di fit del modello (AIC, RMSEA, CFI, chi2):
fitmeasures(object = cfa.visual.text)
# la funzione calcola molti indici di fit per la valutazione del modello. E' possibile selezionarne alcuni tramite il parametro fit.measures
fitmeasures(object = cfa.visual.text,fit.measures = c("RMSEA","CFI","chisq","df","npar","AIC"))

# Per comparare modelli CFA annidati (nested)
# Nota: un modello CFA M0 è nested in M1 se condividono la stessa matrice di covarianza di input S e se M1 (modello "più grande") contiene M0 (modello "più piccolo") nel senso
# che M1 contiene più parametri liberi di M0. L'annidamento, dunque, lo si ottiene quando a parità di strutture di parametri, in un modello (es.: M1) 
# si libera un parametro in più rispetto all'altro modello (es.: M0) che funge da baseline.
m0 = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6 \n text ~~ 0*visual"
cfa.m0 = cfa(model = m0,data = HolzingerSwineford1939)

m1 = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6 \n text ~~ visual"
cfa.m1 = cfa(model = m1,data = HolzingerSwineford1939)

# ..confronto tramite test chi-quadrato (slides 54,57,82 del modulo II-B)
anova(cfa.m0,cfa.m1)
# Il test rigetta l'ipotesi H0: m0=m1 per cui i due modelli sono diversi e il modello m1 è da preferire (si noti AIC minore)


# Indici di modifica (modification indices)
# The modification index (or score test) for a single parameter reflects (approximately) the improvement in model fit (in terms of the chi-square test statistic), 
# if we would refit the model but allow this parameter to be free.
modificationindices(object = cfa.visual.text,sort. = TRUE)

# La colonna con l'indice da controllare è "mi" mentre "op" indica il tipo di relazione da includere nel modello (correlazione o legame fattoriale).
# Ad esempio, l'analisi ci indica che vi è un alto residuo tra x2 e x3 che se incluso nel modello comporta una variazione del parametro per un valore espresso nella colonna "epc".
model.visual.text = "visual =~ x1+x2+x3 \n text =~ x4+x5+x6 \n x2 ~~ x3" 
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939)
summary(cfa.visual.text)
# Notiamo nel campo "Covariances" la covarianza tra i residui di x2 e x3. Ricalcoliamo gli indici di modifica:
modificationindices(object = cfa.visual.text,sort. = TRUE)
# e proviamo ad includere il legame visual =~ x5:
model.visual.text = "visual =~ x1+x2+x3+x5 \n text =~ x4+x5+x6 \n x2 ~~ x3" 
cfa.visual.text = cfa(model = model.visual.text,data = HolzingerSwineford1939)
summary(cfa.visual.text)
