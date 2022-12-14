library(lavaan)

data('HolzingerSwineford1939')
summary(HolzingerSwineford1939)
str(HolzingerSwineford1939)

#  =~ simbolo lavaan che indica legame tra latente e osservate
#  +  indica la concatenazione, non la somma
mod_visual <- " percezione =~ x1 + x2 + x3 "
mod_visual_fit <- cfa(model = mod_visual, data = HolzingerSwineford1939)

# UTILIZZATA METRICA ULI
# estimate(latent) = lambda
# estimate(variances) = teta delta
# estimate(percezione) = fi 11
summary(mod_visual_fit) 
summary(mod_visual_fit, standardized = TRUE) #ci interessa std.all (sup o.30 solitamente si interpretano, meno che se fotte)

library(semPlot)
semPaths(object= mod_visual_fit, what= 'model', whatLabels = 'est')

mod_visual <- " percezione =~ x1 + x2 + x3 "
mod_visual_fit <- cfa(model = mod_visual, data = HolzingerSwineford1939, std.lv = TRUE) #standardizza secondo uli (FALSE UVI, TRUE ULI)


#usare solo la matrice di covarianza per fare una cfa con lavaan
Sy = cov(HolzingerSwineford1939[,7:15])
print(Sy)

apply(X = HolzingerSwineford1939[,7:15], MARGIN = 2, FUN = mean)  #utilizzare apply per lavorare a colonne (MARGIN=2->columnwise, MARGIN=1->rowwise)

hz_std = scale(x = HolzingerSwineford1939[,7:15], center = TRUE, scale = FALSE)  #centrare variabili per assunzione che tau = 0
apply(X = hz_std, MARGIN = 2, FUN = mean) #ora le medie tendono a zero
apply(X = hz_std, MARGIN = 2, FUN = var) #var diversa da 1 perche non abbiamo fatto la scalatura( con solo centratura la varianza non ha subito modifiche)
#scalatura divide per varianza mentre centratura sottrae la media

Sy = cov(hz_std)

mod_visual_fit<-cfa(model = mod_visual, sample.cov = Sy, sample.nobs = 301) #necessiamo della numerosit√† -> z-value √® il valore della statistica utilizzata per fare inferenza sui lamba
# per calcolare lo std error ci serve la numerosit√† che a sua volta ci serve per la distribuzione z

model_visual_stimato = inspect( object = mod_visual_fit, what= 'est') #con what = std  standardizza le matrici stimate
print(model_visual_stimato)

lambda = model_visual_stimato$lambda
Phi = model_visual_stimato$psi
Theta_delta = model_visual_stimato$theta #varianza residui mentre varianza degli errori 0

#matrice dei residui per valurare bont√† modello
