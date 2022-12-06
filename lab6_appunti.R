load('Datasets-20221124/essay_scoring.Rdata')
source('Utilities-20221124/plot_lavaan_model.R')
library(lavaan)

Sy
#domdanda sperimentatore, usare i 4 modi di svolgimento è equivalente?
#il modello a test paralleli equivalenti è il più adatto per questo confronto

#mod 1 parallelo
mod1 <- ' eta1 =~ L*ORIGPRT1 +L*WRITCOPY +L*CARBCOPY +L*ORIGPRT2 \n
          ORIGPRT1 ~~ tht*ORIGPRT1
          WRITCOPY ~~ tht*WRITCOPY
          CARBCOPY ~~ tht*CARBCOPY
          ORIGPRT2 ~~ tht*ORIGPRT2' 

mod1_fit = cfa(model = mod1, sample.cov = Sy, sample.nobs = n, std.lv = TRUE) #UVI perchè vogliamo vedere se i lambda sono uguali
plot_lavaan_model(fitted_model = mod1_fit, what = 'est')

#modello tau equivalente
mod2 <- ' eta1 =~ L*ORIGPRT1 +L*WRITCOPY +L*CARBCOPY +L*ORIGPRT2 \n' 

mod2_fit = cfa(model = mod2, sample.cov = Sy, sample.nobs = n, std.lv = TRUE) #UVI perchè vogliamo vedere se i lambda sono uguali
plot_lavaan_model(fitted_model = mod2_fit, what = 'est')

# modello congenerico
mod3 <- ' eta1 =~ ORIGPRT1 +WRITCOPY +CARBCOPY +ORIGPRT2' 

mod3_fit = cfa(model = mod3, sample.cov = Sy, sample.nobs = n, std.lv = TRUE) #UVI perchè vogliamo vedere se i lambda sono uguali
plot_lavaan_model(fitted_model = mod3_fit, what = 'std')
inspect(object = mod3_fit,what = "std.all")


summary(object = mod3_fit)

#scelta modello migliore  
bfi.fits = matrix(NA,3,5) #matrice per i risultati dei fit dei modelli
bfi.fits[1,] = fitmeasures(object = mod1_fit,fit.measures = c("RMSEA","CFI","AIC","df","npar"))
bfi.fits[2,] = fitmeasures(object = mod2_fit,fit.measures = c("RMSEA","CFI","AIC","df","npar"))
bfi.fits[3,] = fitmeasures(object = mod3_fit,fit.measures = c("RMSEA","CFI","AIC","df","npar"))
rownames(bfi.fits) = c("parallelo","tau",'conva')
colnames(bfi.fits) = c("RMSEA","CFI","AIC","df","npar")

#AIC confronto modelli, ma bisogno controllare gli indici di fit comunque RMSEA e CFI in range
#CFI > 0.95 e RMSEA < 0.08

#interpretiamo il modello congenerico

#i 4 metodi per fare il compito sono tutti i buoni per fare il compito? dato che è stato rigettato il modello parallelo non sono tutti uguali per fare il compito
#ORIG e ORIG2 sono i migliori a livello di pulizia della previsione della latente in quanto hanno meno errore
#oltre a ciò quantificano in modo soddisfacente ai dati in quanto lambada è notevolmente più alto




#take on message
#il modello parallelo è iper vincolato, per questo, riproduce peggio i data (trade off tra gradi di libertà e fittamento)
#il modello parallelo fa un'ipotesi così specifico che è molto raro trovarlo nei dati, lo si utilizza più per confutare che resto



# E S A M E   D O M A N D A   T I P I C A
#si scelga con opportuna procedura/ si migliori con procedura razionale -> o si migliora con procedura statistica (indici di modifica) complessificano il modello, oppure buonsenso eliminando le non necessarie(es si costruisce il mdelllo cfa, si individuano i lambda basso <0.30 e le buttiamo) [lambda bassi spesso quantificano theta delta alti]
#la scelta tra i due metodi è nostra se non specificata, prima complessifica la seconda lo sempolifica

#nello togliere i labda dobbiamop considerare anche la semantica dell'item, per esempio si può lasciare per motivazione di costrutto teorico




#per verificare se un modello funzione o meno il metodo migliore è la capacità predittiva, in quanto se fitta bene non è dettoi che sia bravo a predirre, che è quello che interessa
#indici di modifica ci si ferma quando la capacità predittiva non cresce o per eccessiva complicanze interpretative



















