# F I L E    P E R    E S A M E 

# LIBRERIE E FILE DA IMPORTARE    ----
source('utilities.R')
library(lavaan)
library(semPlot)
library(mvtnorm)
load('data_exam.Rdata')

# ANALISI STRUTTURALE DEL DATASET   ----
str(datax)
head(datax)
summary(datax)
split_dataset(data = datax, prop = 0.6)

boxplot(datax)
barplot(table(datax$y1),main = "y1");
barplot(table(datax$y2),main = "y2");
barplot(table(datax$y3),main = "y3")


# CENTRATURA DATI ----
datax = scale(x = datax, center = TRUE, scale = FALSE)  #centrare variabili per assunzione che tau = 0

# FOR CONVERSIONE DATASET IN ORDINALI   -------
for(j in 1:NCOL(datax)){
  datax[,j] = factor(datax[,j],ordered = TRUE)
}
datax$gender <- as.factor(datax$gender) #costringo R a interpretare gender come fattore(categoriale)
datax$gender <- as.character(datax$nome) 

# ANALISI ESPLORATIVA VISIVA  -----
m_cor <- cor( datax, method = 'spearman') # method='spearman' se ci sono variabili categoriali (default o pearson altrimenti)
heatmap( m_cor, symm= TRUE, hclustfun = hclust)
# Il grafico delle correlazioni mediante heatmap e clustering gerarchico ci mostrano i gruppi di variabili che, a partire dalla loro distanza osservata nei dati,
# si raggruppano a formare clusters/gruppi (metodo di Ward: clusters hanno minima varianza). Notiamo, ad esempio, il gruppo N1-N5 mentre rispetto all'ispezione grafica precedente, il gruppo A1-A5 è stato
# sparso in altri gruppi. 
corrplot::corrplot(m_cor, method = "color") 
# tutte le variabili del dataset che si passa dono essere considerate numeriche

# CLUSTERING GERARCHICO -------
m_cor <- cor( datax, method = 'pearson') # method='spearman' se ci sono variabili categoriali (default o pearson altrimenti)
D <- dist(m_cor, method = 'euclidean')
hc <- hclust(d = D, method = 'ward.D2')
plot(hc)
cutree(tree = hc, k = 2)
mod_1 <-hclust2lavaan(tree = hc, ngroups = 2)

{
Il clustering gerarchico eseguito consente di individuare i raggruppamenti seguenti: y1, y2 (gruppo 1) e y3, y4 (gruppo 2). 
Il modello CFA corrispondente scritto secondo la sintassi di lavaan dovrà dunque contenere la definizione di due variabili latenti eta1 e eta2 corrispondenti a ciascuno dei due gruppi.
}

# ADATTARE MODELLO AI DATI   ----
mod_1_fit <- cfa( model = mod_1, data = datax) #varibili continue
mod_1_fit <- cfa( model = mod_1, data = datax, ordered=colnames(datax[,1:10]), estimator="DWLS") #variabili ordinali
# se non hai il dataset ma matrice di cor utilizzare sample.cov=X, sample.nobs=100,
# per ortogonale o orthogonal=TRUE o lat1 ~~ 0*lat2
# per UVI std.lv=TRUE
# non devono esserci variabili categoriali non ordinali nel dataset che si passa
lavaan_checkConvergence(mod_1_fit)
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
p = NCOL(datax)
100*(31/(p*(p+1)/2))

# MODELLO PARALLELO
mod_1= "eta1 =~ l1*a + l1*b + l1*c \n eta2 =~ l2*d + l2*e
        a ~~  tht * a \n b ~~  tht * b \n c ~~  tht * c \n d ~~  tht * d \n e ~~  tht * e"
mod_1_fit= cfa(model = mod_1, data = datax, std.lv=TRUE)
#poi si guarda indici di adattamento

# MODELLO TAU EQUIVALENTE
mod_1= "eta1 =~ l1*a + l1*b + l1*c \n eta2 =~ l2*d + l2*e"
mod_1_fit= cfa(model = mod_1, data = datax, std.lv=TRUE)
#poi si guarda indici di adattamento
{
Un modello τ-equivalente richiede un modello CFA in cui, per ciascuna scala che lo compone, i coefficienti fattoriali sono vincolati ad essere uguali mentre le varianze di errore sono lasciate libere di variare. 

Il modello stimato non presenta problemi di convergenza e, rispetto a quello precedente, risulta essere ancora più parsimonioso (il modello utilizza solo il 24% dei parametri totali).
Tuttavia gli indici di adattamento complessivo indicano un adattamento non ottimale ai dati e il modello precedente (i.e., un modello congenerico) risulta da preferire.
L’ipotesi di τ -equivalenza non è dunque supportata dai dati.
}

# MODELLO BIFACTOR
mod_3 <- "eta1 =~ y1+y2+y3+y4 \n eta2 =~ y5+y6 \n etabi =~ y1+y2+y3+y4+y5+y6 \n etabi ~~ 0*eta1 \n etabi ~~ 0*eta2"
mod_3_fit= cfa(model = mod_3, data = datax, std.lv=TRUE)
{
Un modello di tipo bi-factor è un modello di misura che include, oltre ai misurandi già ipotizzati, un ulteriore misurando incorrelato coi precedenti e direttamente connesso a tutte le osservate.
L’adattamento mediante algoritmo di massima verosimiglianza non presenta problemi di convergenza. 

Il modello bi-factor è meno parsimonioso rispetto a quelli precedenti, richiedendo questo 10 parametri addizionali (56% dei parametri totali).

La quantificazione dei misurandi mediante le scale eta1 e eta2 non è soddisfacente. 
La prima scala, difatti, presenta bassi coefficienti fattoriali; la seconda scala, invece, presenta due item su quattro con bassi coefficienti fattoriali.
Il confronto del modello di tipo bi-factor con il modello adattato al punto 2 sembra evidenziare un migliore adattatamento, soprattutto se si osservano gli indici CFI e AIC. 
Alla luce di tali risultati non definitivi si decide di mantenere per le successive analisi il modello adattato al punto 2 in quanto più parsimonioso 
}

# INDICI DI ADATTAMENTO   ----
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
# AIC non viene calcolato con DWLS

p = NCOL(datax)
100*(31/(p*(p+1)/2))
{
L’adattamento del modello ai dati avviene in questo mediante stimatori di massima verrosimiglianza  (se non dwls). 

Il modello adattato richiede 21 parametri da stimare è per cui un modello abbastanza parsimonioso (il modello utilizza solo il 38% dei parametri totali). 
La quantificazione dei due misurandi non è ottimale sopratutto per alcuni item (....) Ciò si riflette anche nelle varianze d’errore delle misurazioni che risultano alte per gli item che non riflettono i misurandi ipotizzati. 
Inoltre gli indici di fit globali sono fuori range e in modo particolare l’indice RMSEA evidenzia un cattivo adattamento del modello ai dati. 
Complessivamente il modello di misura, sebbene parsimonioso, necessita di una revisione, specialmente nella definizione del legame tra variabili osservate (item) e latenti (misurandi).
}

# MATRICI E LETTURA MODELLO ADATTATO -----
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
matrix = inspect(object = mod_1_fit, what="std.all") # what='est'
# 1) lambda_x1 = 1 sempre a causa della parametrizzazione ULI (default). 
# 2) Il calcolo degli standard errors (Std.Err) dei parametri in questo caso avviene utilizzando le proprietà asintotiche dello stimatore Lambda: Estimate (che sono i lambda del modello) si distribuiscono asintoticamente come Normali con media uguale a Estimate e Varianza pari a Std.Err^2.

# Per calcolare la matrice di covarianza riprodotta dal modello (implied covariance matrix) Sigma_hat:
Sigma_hat = fitted(mod_1_fit)
Sigma_hat = inspect(mod_1_fit,what = "fitted") #comando simile

# Per stimare i valori eta delle variabili latenti per ciascun individuo (usando il metodo standard "regression"):
eta = lavPredict(object = mod_1_fit, newdata = datax, type = "lv", method = "regression")

{
La matrice lambda indica la magnitudine di quel determinata osservata nell influenza della corrispondente latente.
Consideriamo buoni i valori di lamda sopra i 0.35. Si può inoltre guardare il valore dell errore e di P z per assicurarsi come la osservata di riferimento sia buona o meno in relazione alla sservata
La matrice phi indica la covarianza tra le latenti. Un alto valore di questi valori indica un cambiamento proporzionale e correlato tra loro; per cui probabilmente indicano costrutti simili.
In questo caso sarebbe il caso di controllare il contenuto semantico delle osservate coinvolte e per migliorare il modello valutare l inserimento di una sovraotdinata o di crossloadins, che però devono sempre essere accompagnati da un senso teorico alla base. 
Si può altrimenti considerare, migliore a livello di interpretazione un modello unidimensionale 
}

# ERRORE DI PREVISIONE ----
err_m1 = kFold_validation(model_definition= mod_1, dwls = TRUE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
err_m2 = kFold_validation(model_definition= mod_2, dwls = TRUE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
err_m3 = kFold_validation(model_definition= mod_3, dwls = TRUE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)

boxplot(err_m1, err_m2, err_m3)
t.test(err_m1, err_m2)

apply(cbind(err_m1,err_m2),2,sd) / apply(cbind(err_m1,err_m2),2,mean) # coefficiente di variazione


d(kf1) / abs (mean(kf1)) #coefficiente errore di previsione
{
In termini di errore di previsione, i due modelli presentano variabilita pressocche analoghe (come evidenziato dai coefficienti di variazione) sebbene il modello corrente presenti indici di tendenza centrale e quartili più bassi del modello unidimensionale.
La scelta finale dunque ricade sul modello corrente che necessiterebbe comunque di miglioramenti strutturali prima di un suo utilizzo pratico
}

# ATTENDIBILIA' DELLE SCALE E DEL MODELLO ----
reliability(mod_1_fit,return.total = TRUE)

# Notiamo come l'indice alpha è invariante nei due modelli mentre omega varia. Ciò è dovuto al fatto che alpha si calcola sulla matrice di covarianza S osservata
# mentre omega utilizza la matrice Lambda stimata dal modello CFA.
# quantifica accuratezza dei costrutti
{
L attendibilità complessiva del modello fattoriale multidimensionale è pari a ω total = 0.723 e l’attendibilità parziale per le cinque scale che lo compongono indica un’attendibilità media. 
Ciò difatti riflette la scarsa quantificazione dei misurandi da parte delle variabili osservate 

La scala DH02, sebbene presenti un’attendibilità accettabile, tuttavia non può essere considerata per utilizzi futuri in quanto la sua struttura fattoriale presenta problemi di adattamento complessivo ai dati.
Il suggerimento finale è quello della revisione del modello di misura alla luce dei risultati ivi ottenuti.
}

# GRAFICI -----
boxplot(err_m1, err_m2, err_m3)

plot_lavaan_model(fitted_model = mod_1_fit)
x11();semPaths(object = mod_1_fit, what="model", whatLabels = "std.all")
#stessa cosa

# INVARIANZA ------
datax$group = as.character(datax$group)

# L'invarianza ha quattro livelli annidati, ciascuno dei quali è valutato in relazione al precedente seguendo una procedura incrementale:
# 1) invarianza configurale, 2) invarianza debole, 3) invarianza forte, 4) invarianza esatta
# Per semplicità procederemo adattando i quattro modelli di CFA per l'invarianza senza esplorare le stime ottenute e/o senza rappresentare graficamente i modelli ottenuti.
# Una volta valutato il livello di invarianza esistente nei dati, procederemo all'esplorazione grafica del modello finale insieme alla valutazione dei parametri del modello stimati.

# 1) invarianza configurale
mod_1_conf = cfa(model = mod_1, data = datax,group = "group")

# Il modello configurale è la baseline della procedura incrementale

# 2) invarianza debole
mod_1_weak = cfa(model = mod_1, data = datax,group = "group",group.equal="loadings")
anova(mod_1_conf,mod_1_weak)
# L'invarianza debole è stabilita, il test del chi-quadrato non consente di rigettare l'ipotesi H0 dell'equivalenza dei due modelli. Possiamo procedere oltre.
# se è meno di 0.05 sono differenti i modelli per cui rifiuto
{
I coefficienti standardizzati invece, sono ottenuti dividendo i coefficienti grezzi stimati rispetto alle varianze diag(Φˆ) e diag(Σˆ) del modello adattato.
In particolare, il modello di misura è invariante per maschi e femmine, ad indicare come gli item quantificano il costrutto allo stesso modo per maschi e femmine.

A questo livello di invarianza, tuttavia, non può essere affermato ne che gli item siano interpretati allo stesso modo in entrambi i gruppi ne che i residui del modello siano i medesimi.
E possibile solo affermare che la struttura di misura è la medesima in entrambi i gruppi (nessun confronto rispetto ai punteggi fattoriali latenti può essere effettuato)
}

# 3) invarianza forte
mod_1_strong = cfa(model = mod_1, data = datax,group = "group",group.equal=c("loadings","intercepts"))
anova(mod_1_weak,mod_1_strong)
# Anche l'invarianza forte è stabilita, procediamo oltre.
{# Guardiamo alla sezione "Intercepts" di entrambi i gruppi, in particolare le intercette (medie) dei fattori latenti VCI, PRI, WMI e PSI.
  # Nel gruppo 1 (bambini tipici) sono fissate a zero: in questo modo il gruppo 1 funge da baseline per il confronto rispetto al secondo gruppo.
  # Nel gruppo 2 (bambini atipici) sono libere e sono state per questo stimate: in termini numerici sono diverse da zero (rispetto al gruppo 1) ma statisticamente 
  # non lo sono; infatti il test statistico Z non permette di rigettare l'ipotesi che le medie dei fattori WISC siano diversi tra tipici e atipici. 
  # In conclusione, i due gruppi presentano valori medi di VCI,PRI,WMI,PSI non statisticamente differenti.
  # (se maggiore z value > di pZ allora è significativo)
}

# 4) invarianza esatta 
mod_1_strict = cfa(model = mod_1, data = datax,group = "group",group.equal=c("loadings","intercepts","residuals"))
anova(mod_1_strong,mod_1_strict)
{
L invarianza esatta non è stabilita, come spesso accade nelle applicazioni reali. I due campioni condividono un modello di CFA ad invarianza forte con le medesime
strutture fattoriali (matrice Lambda) e le medesime intercette delle variabili osservate (gli items, dunque, assumono lo stesso significato in entrambi i gruppi).
I due gruppi possono essere dunque confrontati utilizzando i punteggi fattoriali.

Note: Strong invariance, also called "scalar invariance", implies that the meaning of the construct (the factor loadings), and the levels of the underlying items (intercepts) are equal in both groups. Consequently, groups can be compared on their scores on the latent variables. 
}



# INVARIANZA PARZIALE -----
evaluate_partial_invariance(fitted_model = mod_1_weak, type = "metric")
#Cerchiamo lambda che sono presenti in entrambi gruppi con alto indice di modifica
# eta2 =~ FWB1_5 e eta2 =~ FWB1_3 e eta2 =~ FWB2_1 candidati parametri da liberare
#Più parametri perché dobbiamo modificare tanto il nostro pvalue
mod_1_weak_parz = cfa(model = mod_1, data = datax, ordered = colnames(datax[x:y]), 
                         estimator = "DWLS", group = "group", group.equal = c("loadings"), 
                         group.partial = c("eta2 =~ FWB1_5","eta2 =~ FWB1_3","eta2 =~ FWB2_1"))

anova(mod_1_weak_parz, mod_1_conf)
#Modello invariante in sesnso debole tranne per item esclusi, per quelli non è invariante


summary_table(fitted_model = mod_1_strong, type_summary = 'latent')
# dato che le le lambda sono pressochè tutte uguali non ha senso guardarle perchè non ci forniscono informazioni oer la differenza tra i modelli

summary_table(fitted_model = mod_1_strong, type_summary = 'intercept', standardized = TRUE)
#intercetta = medie
#le intercette sono 0 perchè sono centrate
#possiamo confrontare le mi tra il primo e il secondo gruppo (uno dei due gruppi è fissato a zero come baseline)
#si interpretano sempre o le medie (intercette), o i factor scores o le varienze d'errore
#la differenza è significaztiva perchè il pvalue è 0 (<0.05) e il z è molto alto

# MATRICE DEI RESIDUI -----
m_cov = cov(datax)
Sigma_y = fitted(model_one_fit)
print(Sigma_y)
Sigma_y = Lambda %*% Phi %*% t(Lambda) + Theta_delta

#Sigma y e Sy sono vicine tra di loro (confronta valori nella diagonale)
#Matrice dei residui per stabilire bontà modello, più è viciono a 0, più buono
Sy[1:4,1:4] - Sigma_y

# REGRESSIONE LINEARE -----
pr_ftt = lavPredict(object = mod_2_fit,type = "lv",method = "regression")

data_b_mod <- datax$B
data_b_mod$pr <- pr_ftt[,1] 

rl_mod_2 = lm(pr ~ age + gender + age:gender, data=data_b_mod)

summary(rl_mod_2)

# ANALISI INDICI FATTORIALE -----

# Calcolo dei punteggi fattoriali eta_hat (Bartlett)
bfi.eta = lavPredict(object = mod_1_fit,type = "lv",method = "regression")

matrix2 = inspect(object = mod_2_fit, what="std.all")
zeta <- (bfi.eta %*% matrix2$psi) %*% c(1,1)

# Calcoliamo i profili medi dei 5 fattori misurati rispetto al genere. Per facilitare il calcolo possiamo usare comodamente la funzione aggregate():
# medie:
bfi.aggreg.gender = aggregate(bfi.eta,list(bfi.ord$gender),mean)
bfi.aggreg.educ = aggregate(bfi.eta,list(bfi.ord$education),mean)
# varianze:
bfi.aggreg.gender_var = aggregate(bfi.eta,list(bfi.ord$gender),var)
bfi.aggreg.educ_var = aggregate(bfi.eta,list(bfi.ord$education),var)

# Grafico 4x4 per i profili: in riga le variabili categoriali {gender, educ}, in colonna medie e varianze dei profili
x11(); par(mfrow=c(2,2))

plot(1:5,bfi.aggreg.gender[1,2:6],type="b",bty="n",ylim=c(-0.15,0.15),xlab="fattori latenti",ylab="medie",main="profili per genere")
points(1:5,bfi.aggreg.gender[2,2:6],type="b",col=4,lty=2)
legend("topleft", legend=c("maschi", "femmine"),col=c(1,4), lty=c(1,2))

plot(1:5,bfi.aggreg.gender_var[1,2:6],type="b",bty="n",ylim=c(-0.1,0.8),xlab="fattori latenti",ylab="varianze",main="profili per genere")
points(1:5,bfi.aggreg.gender_var[2,2:6],type="b",col=4,lty=2)
legend("topleft", legend=c("maschi", "femmine"),col=c(1,4), lty=c(1,2))

plot(1:5,bfi.aggreg.educ[1,2:6],type="b",bty="n",ylim=c(-0.15,0.3),xlab="fattori latenti",ylab="medie",main="profili per educ")
for(i in 2:5){
  points(1:5,bfi.aggreg.educ[i,2:6],type="b",col=i,lty=i)  
}
legend("topleft", legend=rownames(bfi.aggreg.educ),col=c(1:5),lty=c(1:5))

plot(1:5,bfi.aggreg.educ_var[1,2:6],type="b",bty="n",ylim=c(-0.1,0.95),xlab="fattori latenti",ylab="varianze",main="profili per educ")
for(i in 2:5){
  points(1:5,bfi.aggreg.educ_var[i,2:6],type="b",col=i,lty=i)  
}
legend("topleft", legend=rownames(bfi.aggreg.educ),col=c(1:5),lty=c(1:5))

# REVERSE ----
# Effettuiamo l'operazione di reversing usando la funzione reverse.code() della libreria psych 
big5data[,itms_toreverse] = psych::reverse.code(keys = rep(-1,length(itms_toreverse)),items = big5data[,itms_toreverse])

# POLICORICA ----
big5data_dk_cormat = psych::polychoric(x = big5data_dk)$rho #gruppo danese
{
  Una matrice policorica è una matrice di correlazione parziale che viene utilizzata nell analisi dei dati multivariati per stimare le correlazioni tra le variabili dipendenti, controllando gli effetti di altre variabili indipendenti.
  In altre parole, una matrice policorica tiene conto degli effetti di confondimento tra le variabili del dataset e fornisce una stima della correlazione tra le variabili dipendenti corretta per questi effetti.
  Ciò significa che, utilizzando una matrice policorica, è possibile ottenere stime più accurate delle correlazioni tra le variabili dipendenti, eliminando gli effetti di variabili indipendenti che potrebbero influire sui risultati.
}

# COMPONENTI PRINCIPALI -----
pc = prcomp(datax$A[,1:10],center = TRUE,scale. = TRUE)
var_spiegata = pc$sdev^2
plot(pc, type="l")
prop = var_spiegata/sum(var_spiegata)
prop_cumulata = cumsum(prop)

pc$rotation

mod_1 <- prcomp2lavaan(prcomp_output = pc, numPC = 3, thr = 0.3)

#lettere greche legenda ----

# Αα	Alfa	  Νν	Ni
# Ββ	Beta	  Ξξ	Xi
# Γγ	Gamma	  Οο	Omicron
# Δδ	Delta	  Ππ	Pi
# Εε	Epsilon	Ρρ	Rho
# Ζζ	Zeta	  Σσς	Sigma
# Ηη	Eta	    Ττ	Tau
# Θθ	Theta	  Υυ	Ypsilon
# Ιι	Iota	  Φφ	Phi
# Κκ	Kappa	  Χχ	Chi
# Λλ	Lambda	Ψψ	Psi
# Μμ	Mi	    Ωω	Omega
