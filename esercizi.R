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
  corrplot(Chen)
  heatmap(Chen, scale='none') #sempre vedo la distribuzione delle correlazioni ma permette di vedere le relazioni principali più facilmente
  
  # 2° punto
  library(lavaan)
  mod_uni = ' lat1=~ afraid + frustrated + worried \n '
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
  mod_plu = ' lat1=~ afraid + frustrated + worried \n
              lat2=~ dif_concent + dif_reasoning + confused \n
              lat3=~ worn_out + tired \n
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
  x11();semPaths(object = mod_plu_fit_UVI, what="model", whatLabels = "std")
  
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
  describe(att2)
  
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
  heatmap(cov(att2), scale='none')
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
  
  heatmap(cor(dataxITA), scale = 'none')
  mod_uni = ' lat1=~ Q6A + Q10A + Q7A + Q3A + Q9A + Q16A \n '
  mod_uni_fit_UVI = cfa( model = mod_uni, data = dataxITA, std.lv = TRUE)
  
  summary(mod_uni_fit_UVI, fit.measures = TRUE)
  
  library(semPlot)
  x11();semPaths(object = mod_uni_fit_UVI, what="model", whatLabels = "std")
  
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
  
  x11();semPaths(object = mod_plu_fit_UVI, what="model", whatLabels = "std")
  
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
