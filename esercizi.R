# E s e r c i z i o   1  -----
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
  
  summary(mod_uni_fit_UVI, fit.measures = TRUE, standardized = TRUE)
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

# E s e r c i z i o   2  -----
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

# E s e r c i z i o   3  -----
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

# E s e r c i z i o   3   Modello Completo  -----
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

# E s e r c i z i o   4  -----
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

# E s e r c i z i o   5  -----
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

# E s e r c i z i o   5  di nuovo  -----
{
  load('Datasets-20221124/data_ex5.Rdata')
  
  #1° punto ------
  mod_ort <- ' lat1=~ Y1+Y2+Y3+Y4+Y5 \n
              lat2=~ Y6+Y7+Y8+Y9+Y10 \n
              lat1~~ 0*lat2 \n'
  
  mod_ort_fit <- cfa(model = mod_ort, sample.cov = S, sample.nobs = 1250)
  summary(mod_ort_fit, fit.measures = TRUE, standardized=TRUE )
  summary(mod_ort_fit, standardized=TRUE )
  
  # 2° punto
  fitmeasures(object = mod_ort_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  #sia rmsea che il cfi sono fuori scala per cui procedo con una procedura razionale per migliorare il modello
  
  summary(mod_ort_fit, fit.measures = TRUE, standardized=TRUE ) # Y3 ha una lambda estremamente bassa per cui la tolgo
  
  mod1_ort <- ' lat1=~ Y1+Y2+Y4+Y5 \n
              lat2=~ Y6+Y7+Y8+Y9+Y10 \n
              lat1~~ 0*lat2 \n'
  
  mod1_ort_fit <- cfa(model = mod1_ort, sample.cov = S, sample.nobs = 1250)
  summary(mod1_ort_fit, fit.measures = TRUE, standardized=TRUE )
  fitmeasures(object = mod1_ort_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  #sia rmsea che il cfi sono fuori scala per cui procedo con una procedura razionale per migliorare il modello
  
  summary(mod1_ort_fit, fit.measures = TRUE, standardized=TRUE ) #il lambda di Y2 è estremamente bassa per cui lo tolgo
  
  mod2_ort <- ' lat1=~ Y1+Y4+Y5 \n
              lat2=~ Y6+Y7+Y8+Y9+Y10 \n
              lat1~~ 0*lat2 \n'
  
  mod2_ort_fit <- cfa(model = mod2_ort, sample.cov = S, sample.nobs = 1250)
  summary(mod2_ort_fit, fit.measures = TRUE, standardized=TRUE )
  fitmeasures(object = mod2_ort_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar'))
  
  #utilizzando una procedura razionale sotrattiva l'adattamento non è migliorato a sufficienza per cui provo la procedura tramite gli indici di modifica
  
  modificationindices(object = mod2_ort_fit,standardized = TRUE)
  
  
  
}

# E s e r c i z i o   6  -----
{
  source('Utilities-20221124/utilities.R')
  library(lavaan)
  library(semPlot)
  
  str(data_ex4)
  
  # es4 ----------
  data_ord = data_ex4 # d'ora innanzi lavoriamo su bfi.ord che è lo stesso di bfi_B e contiene variabili dichiarate come ordinali
  for(j in 1:NCOL(data_ord)){
    data_ord[,j] = factor(data_ord[,j],ordered = TRUE)
  }
  
  mod_uni <- 'lat1=~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15 \n'
  mod_uni_fit <- cfa(model = mod_uni, data = data_ord, ordered = names(data_ord), estimator="DWLS")
  summary(mod_uni_fit, fit.measures = TRUE, standardized=TRUE )
  
  mod_tri_ort <- ' lat1=~ V1+V2+V3+V4+V5 \n
                    lat2=~ V6+V7+V8+V9+V10 \n
                    lat3=~ V11+V12+V13+V14+V15 \n
                    lat1~~ 0*lat2 \n
                    lat1~~ 0*lat3 \n
                    lat2~~ 0*lat3 \n'
  mod_tri_ort_fit <- cfa(model = mod_tri_ort, data = data_ord, ordered = names(data_ord), estimator="DWLS")
  summary(mod_tri_ort_fit, fit.measures = TRUE, standardized=TRUE )
  
  mod_tri_clx <- ' lat1=~ V1+V2+V3+V4+V5 \n
                    lat2=~ V6+V7+V8+V9+V10 \n
                    lat3=~ V11+V12+V13+V14+V15 \n'
  mod_tri_clx_fit <- cfa(model = mod_tri_clx, data = data_ord, ordered = names(data_ord), estimator="DWLS")
  summary(mod_tri_clx_fit, fit.measures = TRUE, standardized=TRUE )
  
  bfi.fits = matrix(NA,3,5) #matrice per i risultati dei fit dei modelli
  bfi.fits[1,] = fitmeasures(object = mod_uni_fit,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
  bfi.fits[2,] = fitmeasures(object = mod_tri_ort_fit,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
  bfi.fits[3,] = fitmeasures(object = mod_tri_clx_fit,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
  rownames(bfi.fits) = c("mod_uni","mod_tri_ort",'mod_tri_clx')
  colnames(bfi.fits) = c("RMSEA","CFI","chisq","df","npar")
  #il mod_tri_clx è notevolemente migliore secondo ogni indice
  
  plot_lavaan_model(fitted_model = mod_tri_clx_fit)
  
  # Estrazione delle matrici del modello 
  A = inspect(object = mod_tri_clx_fit,what = "std.all")
  
  A$lambda #Lambda
  A$theta #Theta_delta
  A$psi #Phi
  
  reliability(mod_tri_clx_fit)
  #i valori di attendibilità del modello sono ancora molto bassi
  
  
  # valutiamo l'errore di previzione --------
  source('Utilities-20221124/utilities.R')
  err_m1 = kFold_validation(model_definition = mod_uni, dwls = TRUE, data = data_ex4,nfold = 5,error = 'montecarlo',force_crossValid = TRUE, B = 100)
  err_m2 = kFold_validation(model_definition = mod_tri_ort, dwls = TRUE, data = data_ex4,nfold = 5,error = 'montecarlo',force_crossValid = TRUE, B = 100)
  err_m3 = kFold_validation(model_definition = mod_tri_clx, dwls = TRUE, data = data_ex4,nfold = 5,error = 'montecarlo',force_crossValid = TRUE, B = 100)
  
  boxplot(err_m1, err_m2, err_m3)
  t.test(err_m1, err_m2)
  
  
  
}

# E s e r c i z i o   7  -----
{
  load('Datasets-20221124/rse.RData')
  source('Utilities-20221124/utilities.R')
  library('lavaan')
  
  
  datax <- rse
  
  str(datax)
  head(datax)
  summary(datax)
  
  # 1° punto ----
  datax <- split_dataset(data = datax, prop = 0.6)
  
  # 2° punto ----
  m_cor <- cor( datax$A[1:10], method = 'spearman') # method='spearman' se ci sono variabili categoriali
  heatmap( m_cor, symm= TRUE, hclustfun = hclust)
  
  D <- dist(m_cor, method = 'euclidean')
  hc <- hclust(d = D, method = 'ward.D2')
  plot(hc)
  
  # 3° punto ----
  mod_1 <- hclust2lavaan(tree = hc, ngroups = 2)
  mod_2 <- "eta1 =~ Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10"
  mod_3 <- "eta1 =~ Q1+Q2+Q4 \n eta2 =~ Q3+Q5+Q6+Q7+Q8+Q9+Q10 \n sovr =~ eta1+eta2 \n"
  
  mod_1_fit <- cfa( model = mod_1, data = datax$B[,1:10], ordered=c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10'), estimator="DWLS")
  mod_2_fit <- cfa( model = mod_2, data = datax$B[,1:10], ordered=c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10'), estimator="DWLS")
  mod_3_fit <- cfa( model = mod_3, data = datax$B[,1:10], ordered=c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10'), estimator="DWLS")
  
  # 4° punto ----
  summary(mod_1_fit, standardized=TRUE )
  summary(mod_2_fit, standardized=TRUE )
  summary(mod_3_fit, standardized=TRUE )
  
  
  round(fitmeasures(object = mod_1_fit, fit.measures = c("RMSEA","CFI","AIC","chisq","df","npar")), 3)
  round(fitmeasures(object = mod_2_fit, fit.measures = c("RMSEA","CFI","AIC","chisq","df","npar")), 3)
  round(fitmeasures(object = mod_3_fit, fit.measures = c("RMSEA","CFI","AIC","chisq","df","npar")), 3)
  #il modello migliore è il primo, l'aggiunta di una sovraordinata non ha portato a un adattamento migliore ai dati
  #inoltre il terzo modello sembra avere errori di convergenza dato il mancato calcolo di alcuni standard error
  
  # 5° punto ----
  err_m1 = kFold_validation(model_definition= mod_1, dwls = TRUE, data= rse[,1:10], nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  err_m2 = kFold_validation(model_definition= mod_2, dwls = TRUE, data= rse[,1:10], nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  err_m3 = kFold_validation(model_definition= mod_3, dwls = TRUE, data= rse[,1:10], nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  
  boxplot(err_m1, err_m2, err_m3)
  t.test(err_m1, err_m2)
  
  # 6° punto ----
  
  matrix1 = inspect(object = mod_1_fit, what="std.all")
  matrix2 = inspect(object = mod_2_fit, what="std.all")
  matrix3 = inspect(object = mod_3_fit, what="std.all")
  
  matrix1$lambda
  matrix1$theta
  matrix1$psi
  
  matrix2$lambda
  matrix2$theta
  matrix2$psi
  
  matrix3$lambda
  matrix3$theta
  matrix3$psi
  
  x11();semPaths(object = mod_3_fit, what="model", whatLabels = "std.all")
  
  
  #tramite le analisi svolte notiamo, come prevedibile, che il modello meno adattato ai dati è quello con l'errore di previsione mediano minore e son la varianza di esso minore. 
  #Nonostante ciò preferisco scegliere come modello "migliore" il primo in quanto ha indici di adattamento migliori, soprattuto sull'RMSEA, che si avvicina maggiormente a valori validi.
  # Do per cui precedenza all'adattamento ai dati visto anche che, con un test t base degli errori di previsioni non sembra significativa la differenza tra il primo e il secondo
  
  # 7° punto
  reliability(mod_1_fit, return.total = TRUE)
}

# E s e r c i z i o   8  ----
{
  #semplicemente guardare il significato logico delle domande e creare un modello in base a quello
  #zero sbatta
}

# E s e r c i z i o   9  ----
{
  source('Utilities-20221124/utilities.R')
  library(lavaan)
  library(semPlot)
  
  datax <- dataex9
  
  str(datax)
  
  datax <- datax[,2:7]
  
  # 1° punto ----
  mod_1 <- "eta1 =~ y1+y2+y3+y4+y5+y6"
  mod_2 <- 'eta1 =~ y1+y2+y3+y4 \n eta2 =~ y5+y6'
  mod_3 <- "eta1 =~ y1+y2+y3+y4 \n eta2 =~ y5+y6 \n etabi =~ y1+y2+y3+y4+y5+y6 \n etabi ~~ 0*eta1 \n etabi ~~ 0*eta2"
  
  # 2° punto
  mod_1_fit <- cfa( model = mod_1, data = datax) #varibili continue
  mod_2_fit <- cfa( model = mod_2, data = datax) #varibili continue
  mod_3_fit <- cfa( model = mod_3, data = datax) #varibili continue
  
  round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
  round(fitmeasures(object = mod_2_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
  round(fitmeasures(object = mod_3_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
  #scelgo il modello 2
  
  # 3° punto
  err_m1 = kFold_validation(model_definition= mod_1, dwls = FALSE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  err_m2 = kFold_validation(model_definition= mod_2, dwls = FALSE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  err_m3 = kFold_validation(model_definition= mod_3, dwls = FALSE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  
  boxplot(err_m1, err_m2, err_m3)
  
  # 4° punto
  summary(mod_2_fit, fit.measures = TRUE, standardized=TRUE )
  #il valore della covarianza tra eta1 e eta2 ha una buona significatività. infatti standardizzato si attesta sui 0.76. questo potrebbe indicare un giovamento da una variabile sovraordinata
  mod_4 <- 'eta1 =~ y1+y2+y3+y4 \n eta2 =~ y5+y6 \n sovr =~ eta1 + eta2 '
  mod_4_fit <- cfa( model = mod_4, data = datax) #varibili continue
  round(fitmeasures(object = mod_4_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
  #perde un minimo di adattamento
  x11();semPaths(object = mod_4_fit, what="model", whatLabels = "std.all")
  
  # 5° punto
  err_m2 = kFold_validation(model_definition= mod_2, dwls = FALSE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  err_m4 = kFold_validation(model_definition= mod_4, dwls = FALSE, data= datax, nfold= 10, error= 'montecarlo', force_crossValid= TRUE, B= 100)
  boxplot(err_m4, err_m2)
  #mantengo la scelta del modello 2 senza sovraordinata perchè, seppur l'adattamento e la capacità previsionale è comparabile è di più facile interpretazione e d è più parsimonioso
  
  # 6° punto
  reliability(mod_2_fit,return.total = TRUE)
  
  # 7° punto
  
  pr_ftt = lavPredict(object = mod_2_fit,type = "lv",method = "regression")
  
  matrix2 = inspect(object = mod_2_fit, what="std.all")
  zeta <- (pr_ftt %*% matrix2$psi) %*% c(1,1)
  
  hist(bfi.eta[,1],main="eta1",ylab="",xlab="")
  hist(bfi.eta[,2],main="eta2",ylab="",xlab="")
  hist(fg[,1],main="eta1",ylab="",xlab="")
  hist(fg[,2],main="eta2",ylab="",xlab="")
}

# E s e r c i z i o   1 0  ----
{
  source('Utilities-20221124/utilities.R')
  library(lavaan)
  library(semPlot)
  library(mvtnorm)
  
  datax <- items_taylor_manifest_anxiety_scal
  
  str(datax)
  head(datax)
  summary(datax)
  split_dataset(data = datax, prop = 0.6)
  
}

# E s e r c i z i o   1 2  ----
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
  #il test è invariante in senso debole tranne per gli item FWB1_5 FWB1_3 FWB2_1
  
  
  mod_forte_parz = cfa(model = mod, data = fin_train, order = colnames(fin_train)[2:11], estimator = 'DWLS', group = 'PPGENDER', group.equal = c('loadings','intercepts'), group.partial = c('eta2 =~ FWB1_5 ', 'eta2 =~ FWB1_3', 'eta2 =~ FWB2_1'))
  anova(mod_forte_parz, mod_deb_parz, test = 'chisq') #> 0.05
  #il test è invariante in senso forte al netto dei legami specificati FWB1_5 FWB1_3 FWB2_1 non uguali tra i gruppi
  
  
  summary_table(fitted_model = mod_forte_parz, type_summary = 'latent')
  # dato che le le lambda sono pressochè tutte uguali non ha senso guardarle perchè non ci forniscono informazioni oer la differenza tra i modelli
  
  summary_table(fitted_model = mod_forte_parz, type_summary = 'intercept', standardized = TRUE)
  #intercetta = medie
  #le intercette sono 0 perchè sono centrate
  #possiamo confrontare le mi tra il primo e il secondo gruppo (uno dei due gruppi è fissato a zero come baseline)
  #si interpretano sempre o le medie (intercette), o i factor scores o le varienze d'errore
  #la differenza è significaztiva perchè il pvalue è 0 (<0.05) e il z è molto alto
  
  summary_table()
  
}

# E s e r c i z i o   1 3  ----
{
  load('Datasets-20221124/SCS.Rdata')
  library(lavaan)
  library(semPlot)
  library(mvtnorm)
  source('Utilities-20221124/utilities.R')
  
  # 1° punto
  datax <- SCS
  str(datax)
  head(datax)
  summary(datax)
  datax <- split_dataset(data = datax, prop = 0.4, seedx = 90211)
  
  # 2° punto
  pc = prcomp(datax$A[,1:10],center = TRUE,scale. = TRUE)
  var_spiegata = pc$sdev^2
  plot(pc, type="l")
  prop = var_spiegata/sum(var_spiegata)
  prop_cumulata = cumsum(prop)
  
  pc$rotation
  
  mod_1 <- prcomp2lavaan(prcomp_output = pc, numPC = 3, thr = 0.3)
  
  # 3° punto
  for(j in 1:10){
    datax$B[,j] = factor(datax$B[,j],ordered = TRUE)
  }
  datax$B$gender <- as.factor(datax$B$gender) #costringo R a interpretare gender come fattore(categoriale)
  mod_1 <-'eta1 =~ Q1+Q3+Q7+Q10
          eta2 =~ Q1+Q2+Q3+Q6+Q8+Q9 
          eta3 =~ Q3+Q5
          eta1 ~~ 0*eta2
          eta1 ~~ 0*eta3
          eta2 ~~ 0*eta3'
  
  mod_1_fit <-  cfa( model = mod_1, data = datax$B[,1:10], ordered=colnames(datax$B[,1:10]), estimator="DWLS")
  lavaan_checkConvergence(mod_1_fit)
  round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
  summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
  
  mod_2 <-'eta1 =~ Q1+Q3+Q7+Q10+Q2+Q6+Q8+Q9+Q5'
  
  mod_2_fit <-  cfa( model = mod_2, data = datax$B[,1:10], ordered=colnames(datax$B[,1:10]), estimator="DWLS")
  lavaan_checkConvergence(mod_2_fit)
  round(fitmeasures(object = mod_2_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
  summary(mod_2_fit, fit.measures = TRUE, standardized=TRUE )
  
  # 4° punto
  reliability(mod_1_fit,return.total = TRUE)
  reliability(mod_2_fit,return.total = TRUE)
  
  #
  
}

# E s e r c i z i o   1 4  ----
{
  # 1) invarianza configurale
  mod_2_conf = cfa(model = mod_2,data = datax$B,group = "gender")
  # Il modello configurale è la baseline della procedura incrementale
  
  # 2) invarianza debole
  mod_2_weak = cfa(model = mod_2,data = datax$B,group = "gender",group.equal="loadings")
  anova(mod_2_conf,mod_2_weak)
  # L'invarianza debole è stabilita, il test del chi-quadrato non consente di rigettare l'ipotesi H0 dell'equivalenza dei due modelli. Possiamo procedere oltre.
}

# E s e r c i z i o   1 5  -----
{
  pr_ftt = lavPredict(object = mod_2_fit,type = "lv",method = "regression")
  
  data_b_mod <- datax$B
  data_b_mod$pr <- pr_ftt[,1] 
  
  rl_mod_2 = lm(pr ~ age + gender + age:gender, data=data_b_mod)
  
  summary(rl_mod_2)
}

# E s e r c i z i o   1 6   ----
{
  source('Utilities-20221124/utilities.R')
  library(lavaan)
  library(semPlot)
  library(mvtnorm)
  load('Datasets-20221124/mimic.Rdata')
  
  datax <- mimic
  datax$z <- z
  
  
}





