# E s e r c i z i o   1
{
  #installo psych e controllo la struttura del dataset Chen
  install.packages('psych')
  library(psych)
  ?psych::Chen
  str(Chen)
  Chen
  colnames(Chen)
  
  # 1° punto
  library(corrplot)
  corrplot(Chen, method = 'circle')
  heatmap(Chen, symm= TRUE, hclustfun = hclust) #sempre vedo la distribuzione delle correlazioni ma permette di vedere le relazioni principali più facilmente
  
  colnames(Chen) = rownames(Chen)
  
  # 2° punto
  library(lavaan)
  mod_uni = ' lat1=~ Social_functioning+dif_reasoning+slow_react+confused+forgetful+dif_concent+tired+energetic.R+worn_out+peppy.R+calm.R+blue+happy.R+nervous+down+afraid+frustrated+worried \n '
  mod_uni_fit_UVI = cfa( model = mod_uni, sample.cov = Chen, sample.nobs = 403, std.lv = TRUE)
  mod_uni_fit_ULI = cfa( model = mod_uni, sample.cov = Chen, sample.nobs = 403)
  
  summary(mod_uni_fit_UVI, fit.measures = TRUE)
  summary(mod_uni_fit_ULI, fit.measures = TRUE) #si può vedere il riassunto del nostro modello fittato e la differenza tra uli e uvi
  
  # 3° punto
  matrix_mod_UVI = inspect(object = mod_uni_fit_UVI, what="std.all")
  matrix_mod_ULI = inspect(object = mod_uni_fit_ULI, what="std.all")
  
  matrix_mod_UVI
  matrix_mod_ULI
  
  # 4° punto
  mod_plu = ' lat1=~ nervous + blue + down + frustrated + worried + afraid \n
              lat2=~ slow_react + forgetful + confused + dif_reasoning + dif_concent \n
              lat3=~ Social_functioning + happy.R + calm.R + energetic.R + peppy.R + tired + worn_out \n
              lat1~~ 0*lat2 \n
              lat1~~ 0*lat3 \n
              lat2~~ 0*lat3 \n'
  
  mod_plu_fit_ULI = cfa( model = mod_plu, sample.cov = Chen, sample.nobs = 403, std.lv = TRUE)
  summary(mod_plu_fit_ULI, fit.measures = TRUE)
  matrix_mod_ULI = inspect(object = mod_plu_fit_ULI, what="std.all")
  matrix_mod_ULI
  
  mod_plu_fit_UVI = cfa( model = mod_plu, sample.cov = Chen, sample.nobs = 403)
  summary(mod_plu_fit_UVI, fit.measures = TRUE)
  matrix_mod_UVI = inspect(object = mod_plu_fit_UVI, what="std.all")
  matrix_mod_UVI
  
  # 5° punto
  library(semPlot)
  x11();semPaths(object = mod_plu_fit_UVI, what="model", whatLabels = "std.all")
  
  # 6° punto
  #??????????????????????????
  
  
}

# E s e r c i z i o   2
{
  library(datasets)
  str(attitude)
  summary(attitude)
  
  att2 <- attitude[,2:7] #prendo solo le colonne da 2 a 7, escludo per cui la prima variabile oddervata
  
  # 1° punto
  #describe(att2)
  
  # 2° punto
  source('Utilities-20221124/split_half.R')
  
  sh <- split_half(att2)
  #??? quanto è valido?
  
  # 3° punto
  punteggi = matrix(NA, 30,3)
  
  punteggi[,1] <- rowSums(att2)
  mediaGrezzo <- mean(punteggi[,1])
  punteggi[,2] <- punteggi[,1]*sh + (1-sh)*mediaGrezzo
  
  # 4° punto
  library(psych)
  summary(alpha(att2))
  ac <- 0.81
  punteggi[,3] <- punteggi[,1]*ac + (1-ac)*mediaGrezzo
  colnames(punteggi) <- c('grezzo', 'split_half', 'cronbach')
  
  # 5° punto
  plot(density(punteggi[,1]), main = 'Grezzo', xlim=c(150,550))
  plot(density(punteggi[,2]), main = 'Split Half', xlim=c(150,550))
  plot(density(punteggi[,3]), main = 'Cronbach', xlim=c(150,550))
  describe(punteggi)
  
  # 6° punto
  pairs(att2)
  heatmap(cov(att2), symm= TRUE, hclustfun = hclust)
  library(corrplot)
  corrplot(cor(att2))
  #commento boh????????
}

# E s e r c i z i o   3
{
  load('Datasets-20221124/mach/mach.Rdata')
  
  # 1° punto
  str(datax)
  summary(datax)  # insieme di punteggi categoriali da -8 a 8 su 20 item divisi per nazione
  
  # 2° punto
  dataxITA <- datax[datax$country == "IT",]
  dataxITA <- dataxITA[,1:20]
  
  # 3° punto
  library(lavaan)
  
  heatmap(cor(dataxITA), symm= TRUE, hclustfun = hclust)
  mod_uni = ' lat1=~ Q6A + Q10A + Q7A + Q3A + Q9A + Q16A \n '
  mod_uni_fit_UVI = cfa( model = mod_uni, data = dataxITA, std.lv = TRUE)
  
  summary(mod_uni_fit_UVI, fit.measures = TRUE )
  
  library(semPlot)
  x11();semPaths(object = mod_uni_fit_UVI, what="model", whatLabels = "std.all")
  
  # 4° punto
    #* L'adattamento del modello sembra buono, i valori di lambda stimati oscillano tra i 0.44 e i 0.67
    #* il RMSEA è molto basso 0.028 e inferiore al p-value < 0.05
  
  # 5° punto
  source('Utilities-20221124/reliability_semTools.R')
  reliability(mod_uni_fit_UVI)
  
  # 6° punto
  mod_plu = ' lat1=~ Q6A + Q7A \n
              lat2=~ Q4A + Q11A \n
              lat3=~ Q1A + Q2A '
  mod_plu_fit_UVI = cfa( model = mod_plu, data = dataxITA, std.lv = TRUE)
  summary(mod_plu_fit_UVI, fit.measures = TRUE)
  
  x11();semPaths(object = mod_plu_fit_UVI, what="model", whatLabels = "std.all")
  
  # 7° piano
  cfa_fits = matrix(NA, 2,6)  #creo mattice nuovo
  cfa_fits[1,] = fitmeasures(object = mod_uni_fit_UVI,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
  cfa_fits[2,] = fitmeasures(object = mod_plu_fit_UVI,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  
  colnames(cfa_fits) = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')
  rownames(cfa_fits) = c('uni', 'plu')
  cfa_fits
  
  reliability(mod_plu_fit_UVI)
}

# E s e r c i z i o   3   Modello Completo
{
  load('Datasets-20221124/mach/mach.Rdata')
  
  # 1° punto
  str(datax)
  summary(datax)  # insieme di punteggi categoriali da -8 a 8 su 20 item divisi per nazione
  
  # 2° punto
  dataxITA <- datax[datax$country == "IT",]
  dataxITA <- dataxITA[,1:20]
  
  # 3° punto
  library(lavaan)
  

  
  heatmap(cor(dataxITA), symm= TRUE, hclustfun = hclust)
  mod_uni = ' lat1=~ Q1A + Q2A + Q3A + Q4A + Q5A + Q6A + Q7A + Q8A + Q9A + Q10A + Q11A + Q12A + Q13A + Q14A + Q15A + Q16A + Q17A + Q18A + Q19A + Q20A \n'
  mod_uni_fit_UVI = cfa( model = mod_uni, data = dataxITA, std.lv = TRUE)
  
  summary(mod_uni_fit_UVI, fit.measures = TRUE, standardized=TRUE )
  
  library(semPlot)
  x11();semPaths(object = mod_uni_fit_UVI, what="model", whatLabels = "std.all")
  
  # 4° punto
  #* L'adattamento del modello ai dati sembra buono se si guarda l'indice RMSEA è molto basso 0.045 e inferiore al p-value < 0.05
  #* se guardiamo le lambda stimate ci si accorfe di lambda con direzioni opposte, per cui o non è il caso di inserirle in questa scala o andrebbero invertite
  
  # 5° punto
  source('Utilities-20221124/reliability_semTools.R')
  round(reliability(mod_uni_fit_UVI),5)
  #*secondo gli indici alpha e omega il nostro test ha una attendibilità interna pessima
  
  # 6° punto
  mod_plu = ' lat1=~ Q13A + Q1A + Q20A + Q12A + Q18A + Q5A + Q8A + Q2A + Q15A + Q19A \n
              lat2=~ Q7A + Q6A + Q10A + Q3A + Q9A + Q16A + Q4A + Q14A + Q17A + Q11A \n'
  
  mod_plu_fit_UVI = cfa( model = mod_plu, data = dataxITA, std.lv = TRUE)
  summary(mod_plu_fit_UVI, fit.measures = TRUE, standardized=TRUE)
  
  x11();semPaths(object = mod_plu_fit_UVI, what="model", whatLabels = "std.all")
  
  # 7° piano
  cfa_fits = matrix(NA, 2,6)  #creo mattice nuovo
  cfa_fits[1,] = fitmeasures(object = mod_uni_fit_UVI,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
  cfa_fits[2,] = fitmeasures(object = mod_plu_fit_UVI,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  
  colnames(cfa_fits) = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')
  rownames(cfa_fits) = c('uni', 'plu')
  round(cfa_fits, 4)
  
  reliability(mod_plu_fit_UVI)
  
  #*il secondo modello è migliore sotto ogni indice misurato
  #*sia gli alpha che gli omega hanno valori buoni sopra il 0.7
}

# E s e r c i z i o   4
{
  library(lavaan)
  library(semPlot)
  source('Utilities-20221124/plot_lavaan_model.R')
  source('Utilities-20221124/reliability_semTools.R')
  
  str(data_ex4)
  
  # 1° punto -----
  data_ord = data_ex4 # d'ora innanzi lavoriamo su bfi.ord che è lo stesso di bfi_B e contiene variabili dichiarate come ordinali
  for(j in 1:NCOL(data_ord)){
    data_ord[,j] = factor(data_ord[,j],ordered = TRUE)
  }
  
  mod_uni <- 'lat1=~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15 \n'
  mod_uni_fit <- cfa(model = mod_uni, data = data_ord, ordered = names(data_ord), estimator="DWLS")
  summary(mod_uni_fit, fit.measures = TRUE, standardized=TRUE )
  
  
  # 2° punto ------
  mod_tri_ort <- ' lat1=~ V1+V2+V3+V4+V5 \n
                    lat2=~ V6+V7+V8+V9+V10 \n
                    lat3=~ V11+V12+V13+V14+V15 \n
                    lat1~~ 0*lat2 \n
                    lat1~~ 0*lat3 \n
                    lat2~~ 0*lat3 \n'
  mod_tri_ort_fit <- cfa(model = mod_tri_ort, data = data_ord, ordered = names(data_ord), estimator="DWLS")
  summary(mod_tri_ort_fit, fit.measures = TRUE, standardized=TRUE )
  
  
  # 3° punto  ------
  mod_tri_clx <- ' lat1=~ V1+V2+V3+V4+V5 \n
                    lat2=~ V6+V7+V8+V9+V10 \n
                    lat3=~ V11+V12+V13+V14+V15 \n'
  mod_tri_clx_fit <- cfa(model = mod_tri_clx, data = data_ord, ordered = names(data_ord), estimator="DWLS")
  summary(mod_tri_clx_fit, fit.measures = TRUE, standardized=TRUE )
  
  
  # 4° punto  ------
  bfi.fits = matrix(NA,3,5) #matrice per i risultati dei fit dei modelli
  bfi.fits[1,] = fitmeasures(object = mod_uni_fit,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
  bfi.fits[2,] = fitmeasures(object = mod_tri_ort_fit,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
  bfi.fits[3,] = fitmeasures(object = mod_tri_clx_fit,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
  rownames(bfi.fits) = c("mod_uni","mod_tri_ort",'mod_tri_clx')
  colnames(bfi.fits) = c("RMSEA","CFI","chisq","df","npar")
      #il mod_tri_clx è notevolemente migliore secondo ogni indice
  
  # 5° punto  ------
  plot_lavaan_model(fitted_model = mod_tri_clx_fit)
  
  # Estrazione delle matrici del modello 
  A = inspect(object = mod_tri_clx_fit,what = "std.all")
  
  A$lambda #Lambda
  A$theta #Theta_delta
  A$psi #Phi
  
  reliability(mod_tri_clx_fit)
    #i valori di attendibilità del modello sono ancora molto bassi
  
  # 6° punto -----
  
  #miglioro il modello andando a ridurre i parametri di predizione
  
  
  
}

# E s e r c i z i o   5
{
  load('Datasets-20221124/data_ex5.Rdata')
  library(lavaan)
  library(semPlot)
  
  str(S)
  
  # 1° punto ------
  mod_duo_ort <- ' lat1=~ Y1+Y2+Y3+Y4+Y5 \n
                    lat2=~ Y6+Y7+Y8+Y9+Y10 \n
                    lat1~~ 0*lat2 \n'
  mod_duo_ort_fit <- cfa(model = mod_duo_ort, sample.cov = S, sample.nobs = 1250)
  
  
  # 2° punto -----
  fitmeasures(object = mod_duo_ort_fit,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
  #sia l' RMSEA che il cfi sono fuori range
  
  summary(mod_duo_ort_fit, fit.measures = TRUE, standardized=TRUE )
  
  #modifico con gli indici di modifica
  modificationindices(object = mod_duo_ort_fit,sort. = TRUE)
  
  mod1_duo_ort <- ' lat1=~ Y1+Y2+Y4+Y5 \n
                    lat2=~ Y6+Y7+Y8+Y9+Y10+Y5 \n
                    lat1~~ 0*lat2 \n'
  mod1_duo_ort_fit <- cfa(model = mod1_duo_ort, sample.cov = S, sample.nobs = 1250)
  summary(mod1_duo_ort_fit, fit.measures = TRUE, standardized=TRUE )
  modificationindices(object = mod1_duo_ort_fit,sort. = TRUE)
  
  
  mod2_duo_ort <- ' lat1=~ Y1+Y2+Y3+Y4+Y5 \n
                    lat2=~ Y6+Y7+Y8+Y9+Y10+Y5+Y4+Y1 \n
                    lat1~~ 0*lat2 \n'
  mod2_duo_ort_fit <- cfa(model = mod2_duo_ort, sample.cov = S, sample.nobs = 1250)
  summary(mod2_duo_ort_fit, fit.measures = TRUE, standardized=TRUE )
  modificationindices(object = mod2_duo_ort_fit,sort. = TRUE)
  
  
  
  cfa_fits = matrix(NA, 3,6)  #creo mattice nuovo
  cfa_fits[1,] = fitmeasures(object = mod_duo_ort_fit,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
  cfa_fits[2,] = fitmeasures(object = mod1_duo_ort_fit,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  cfa_fits[3,] = fitmeasures(object = mod2_duo_ort_fit,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  
  colnames(cfa_fits) = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')
  rownames(cfa_fits) = c('mod', 'mod1', 'mod2')
  round(cfa_fits, 4)
  
  #modifico con la riduzione della lambda
  mod3_duo_ort <- ' lat1=~ Y1+Y2+Y4+Y5 \n
                    lat2=~ Y6+Y7+Y8+Y9+Y10 \n
                    lat1~~ 0*lat2 \n'
  mod3_duo_ort_fit <- cfa(model = mod3_duo_ort, sample.cov = S, sample.nobs = 1250)
  summary(mod3_duo_ort_fit, fit.measures = TRUE, standardized=TRUE )
  
  mod3_duo_ort <- ' lat1=~ Y1+Y4+Y5 \n
                    lat2=~ Y6+Y7+Y8+Y9+Y10 \n
                    lat1~~ 0*lat2 \n'
  mod3_duo_ort_fit <- cfa(model = mod3_duo_ort, sample.cov = S, sample.nobs = 1250)
  summary(mod3_duo_ort_fit, fit.measures = TRUE, standardized=TRUE )
  
  
  cfa_fits = matrix(NA, 4,6)  #creo mattice nuovo
  cfa_fits[1,] = fitmeasures(object = mod_duo_ort_fit,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')) 
  cfa_fits[2,] = fitmeasures(object = mod1_duo_ort_fit,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  cfa_fits[3,] = fitmeasures(object = mod2_duo_ort_fit,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  cfa_fits[4,] = fitmeasures(object = mod3_duo_ort_fit,
                             fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  
  colnames(cfa_fits) = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')
  rownames(cfa_fits) = c('mod', 'mod1', 'mod2', 'mod3')
  round(cfa_fits, 4)
  
  #mod1 e 2 hanno parametri meglio ma l'interpretazione peggiora datto il numero di crossloading
  
  #3° punto -------
  
  source('Utilities-20221124/reliability_semTools.R')
  reliability(mod2_duo_ort_fit)
  
  #attendibilità della scala è na merda
  
}


# E s e r c i z i o   1 
{
  load('Datasets-20221124/finance.Rdata')
  source('Utilities-20221124/utilities.R')
  
  str(finance)
  
  #1
  fin <- split_dataset(data = finance, prop = 0.3, seed = 8219291)
  fin_train <- fin$B
  fin_test <- fin$A
  
  #2
  S <- cor(fin_test[,2:11], method = 'spearman')
  D <- dist(S, method = 'euclidean')
  hc <- hclust(d = D, method = 'ward.D2')
  plot(hc)
  
  hclust2lavaan(tree = hc, ngroups = 2)
  
  #3
  for(j in 2:11){
    fin_train[,j] = factor(fin_train[,j], order = TRUE)
  }
  fin_train$PPGENDER = as.character(fin_train$PPGENDER)
  
  mod = "eta1 =~ FWB1_1+FWB1_2+FWB1_4+FWB2_2 \n eta2 =~ FWB1_3+FWB1_5+FWB1_6+FWB2_1+FWB2_3+FWB2_4"
  mod_conf = cfa(model = mod, data = fin_train, order = colnames(fin_train)[2:11], estimator = 'DWLS', group = 'PPGENDER')
  
  mod_deb = cfa(model = mod, data = fin_train, order = colnames(fin_train)[2:11], estimator = 'DWLS', group = 'PPGENDER', group.equal = 'loadings')
  anova(mod_deb, mod_conf, test = 'chisq') #> 0.05
  #riufiutiamo h0, per cui l'invarianza è una merda
  #dobbiamo raggiungere almeno il livello forte per cui dobbiamo modificare il modello
  #come invarianza completa non si raggiunge nemmeno la debole
  
  #ora guardiamo l'invarianza parziale in quanto la completa non si raggiunge
  # la si valuta se e solo se non si raggiunge la completa
  #(parziale) qualche lambda diversa l'accettiamo
  
  evaluate_partial_invariance(fitted_model = mod_deb, type = c('metric'))
  # ci serve per capire che legami liberare
  # dato che il pvalue è molto basso liberiamo molti parametri subito così da aumnetare abbastanza il pvalue
  
  mod_deb_parz = cfa(model = mod, data = fin_train, order = colnames(fin_train)[2:11], estimator = 'DWLS', group = 'PPGENDER', group.equal = 'loadings', group.partial = c('eta2 =~ FWB1_5 ', 'eta2 =~ FWB1_3', 'eta2 =~ FWB2_1'))
  anova(mod_deb_parz, mod_conf, test = 'chisq') #> 0.05
  
  
  mod_forte = cfa(model = mod, data = fin_train, order = colnames(fin_train)[2:11], estimator = 'DWLS', group = 'PPGENDER', group.equal = c('loadings','intercepts'), group.partial = c('eta2 =~ FWB1_5 ', 'eta2 =~ FWB1_3', 'eta2 =~ FWB2_1'))
  anova(mod_forte, mod_deb_parz, test = 'chisq') #> 0.05
  
  
  
  
}

