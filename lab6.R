#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Descrizione del caso studio
# (B) Analisi e modelli
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2022_2023/testing_psicologico/") #to be changed based on your own working directory!
library(lavaan)

# (A) Descrizione caso studio ---------------------------------------------
# Data from: Jöreskog, K. G., Olsson, U. H., & Wallentin, F. Y. (2016). Confirmatory factor analysis (CFA). In: Multivariate Analysis with LISREL (pp. 283-339). Springer
# n=126 persons were given a three-part English Composition examination. Each part required the person to write an essay, and for each person, scores were obtained on the following: 
# (1) the original part 1 essay, 
# (2) a handwritten copy of the original part 1 essay, 
# (3) a carbon copy of the handwritten copy in (2),
# (4) the original part 2 essay.
# The investigator would like to know whether, on the basis of this sample of size 126, the four scores can be used interchangeably or whether scores on the
# copies (2) and (3) are less reliable than the originals (1) and (4).

# The hypotheses to be tested are that the measurements are:
# (1) parallel, 
# (2) tau-equivalent, 
# (3) congeneric.

load("laboratorio/data/essay_scoring.Rdata")
print(Sy) #matrice di covarianza originale
colnames(Sy) #nomi delle variabili


# (B) Analisi e modelli ---------------------------------------------------

## Modello (1): misure parallele (vedi slide 58, mod II-B) 
## E[y] = 0 (mu=0 per semplicità)
## Cov[y] = 1*phi*1 + I*tht --oppure: Cov[y] = l0*phi*l0+ I*theta (con l0 parametro reale da stimare diverso da 1)

# Definizione del modello in sintassi lavaan (invece di usare \n si può andare a capo direttamente premendo "Invio" sulla tastiera)
cfa1 = "
eta =~ l*ORIGPRT1 + l*WRITCOPY + l*CARBCOPY + l*ORIGPRT2    # modello di misura (con l diverso da 1)
ORIGPRT1 ~~ tht*ORIGPRT1                                    # varianza d'errore fissata ed uguale per tutte le osservate
WRITCOPY ~~ tht*WRITCOPY                                    # varianza d'errore fissata ed uguale per tutte le osservate
CARBCOPY ~~ tht*CARBCOPY                                    # varianza d'errore fissata ed uguale per tutte le osservate
ORIGPRT2 ~~ tht*ORIGPRT2                                    # varianza d'errore fissata ed uguale per tutte le osservate
"

# Adattamento del modello ai dati usando la metrica ULI (che comporta var(eta)=1 e mu=0)
cfa1.fit = cfa(model=cfa1,sample.cov=Sy,sample.nobs=n,std.lv=TRUE)
summary(cfa1.fit)


## Modello (2): misure tau-equivalenti (vedi slide 58, mod II-B) 
## E[y] = 0 (mu=0 per semplicità)
## Cov[y] = 1*phi*1 + Theta_delta --oppure: Cov[y] = l0*phi*l0+ I*theta (con l0 parametro reale da stimare diverso da 1)

# Definizione del modello in sintassi lavaan (invece di usare \n si può andare a capo direttamente premendo "Invio" sulla tastiera)
cfa2 = "
eta =~ l*ORIGPRT1 + l*WRITCOPY + l*CARBCOPY + l*ORIGPRT2    # modello di misura (con l diverso da 1)
ORIGPRT1 ~~ tht1*ORIGPRT1                                    # varianza d'errore per la prima osservata
WRITCOPY ~~ tht2*WRITCOPY                                    # varianza d'errore per la seconda osservata
CARBCOPY ~~ tht3*CARBCOPY                                    # varianza d'errore per la terza osservata
ORIGPRT2 ~~ tht4*ORIGPRT2                                    # varianza d'errore per la quarta osservata
"

# Adattamento del modello ai dati usando la metrica ULI (che comporta var(eta)=1 e mu=0)
cfa2.fit = cfa(model=cfa2,sample.cov=Sy,sample.nobs=n,std.lv=TRUE)
summary(cfa2.fit)


## Modello (3): misure congeneriche (vedi slide 58, mod II-B) 
## E[y] = 0 (mu=0, tau=0 per semplicità)
## Cov[y] = l*phi*l + Theta_delta 

# Definizione del modello in sintassi lavaan (invece di usare \n si può andare a capo direttamente premendo "Invio" sulla tastiera)
cfa3 = "
eta =~ l1*ORIGPRT1 + l2*WRITCOPY + l3*CARBCOPY + l4*ORIGPRT2 # modello di misura 
ORIGPRT1 ~~ tht1*ORIGPRT1                                    # varianza d'errore per la prima osservata
WRITCOPY ~~ tht2*WRITCOPY                                    # varianza d'errore per la seconda osservata
CARBCOPY ~~ tht3*CARBCOPY                                    # varianza d'errore per la terza osservata
ORIGPRT2 ~~ tht4*ORIGPRT2                                    # varianza d'errore per la quarta osservata
"

# Adattamento del modello ai dati usando la metrica ULI (che comporta var(eta)=1 e mu=0)
cfa3.fit = cfa(model=cfa3,sample.cov=Sy,sample.nobs=n,std.lv=TRUE)
summary(cfa3.fit)

## Confronto tra modelli
cfa.fits = matrix(data = NA,nrow = 3,6)
cfa.fits[1,] = fitMeasures(cfa1.fit,fit.measures = c("Chisq","df","pvalue","RMSEA","CFI","AIC"))
cfa.fits[2,] = fitMeasures(cfa2.fit,fit.measures = c("Chisq","df","pvalue","RMSEA","CFI","AIC"))
cfa.fits[3,] = fitMeasures(cfa3.fit,fit.measures = c("Chisq","df","pvalue","RMSEA","CFI","AIC"))
colnames(cfa.fits)=c("Chisq","df","pvalue","RMSEA","CFI","AIC")
rownames(cfa.fits)=c("parallelo","tau-equiv","congenerico")

print(round(cfa.fits,3))
# I risultati suggeriscono quanto segue: in termini di test del chi-quadrato (la statistica test Chisq sotto H0 di distribuisce come Chi-quadrato con df gradi di libertà), 
# dove H0:Sy=Sigma (dove Sigma è la matrice di covarianza riprodotta dal modello), sia il modello parallelo che tau-equiv consentono di rigettare l'ipotesi che
# il modello riproduce bene i dati osservati, ossia la matrice Sy. Ciò è anche confermato dai valori fuori range degli indici RMSEA e CFI.
# Al contrario, il modello congenerico riproduce bene la matrice di cov osservata Sy come evidenziato dal test del Chi-quadrato (che non consente di fatti di rigettare H0)
# e dagli ottimi valori riportati in termini di RMSEA e CFI. 
# Tale risultato è anche supportato dall'utilizzo del criterio del minimo-AIC, secondo cui il modello congenerico risulta da preferire.

# Una volta individuato quale modello meglio si adatta ai dati, possiamo passare all'interpretazione dei parametri di quest'ultimo.
summary(cfa3.fit,standardized=TRUE)
# Rispetto alla matrice Lambda dei coefficienti fattoriali, si nota come i punteggi (essay scores) ottenuti mediante i test ORIGPRT1 e ORIGPRT2 presentano 
# alti punteggi fattoriali rispetto a WRITCOPY e CARBCOPY (come risulta anche dalle alte varianze d'errore residue). 
# Per rispondere alla domanda inziale, i quattro test non possono essere usati in maniera interscambiabile: mentre (1) e (4) possono essere usati l'uno al posto dell'altro, lo stesso non può essere detto per (2) e (3).
# Thus, it appears that scores obtained from originals (ORIGPRT1,ORIGPRT2) are more reliable than scores based on copies (WRITCOPY,CARBCOPY).

cfa3.out = lavInspect(cfa3.fit,what="est")
lambda=cfa3.out$lambda
theta_delta=cfa3.out$theta
phi=cfa3.out$psi
Sigma_hat = lambda%*%phi%*%t(lambda) + theta_delta
print(Sigma_hat)




