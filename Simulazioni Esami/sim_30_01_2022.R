source('Utilities-20221124/utilities.R')
library(lavaan)
library(semPlot)
library(mvtnorm)
load('Simulazioni Esami/data_exam.Rdata')

# 1
datax <- Y

str(datax)
head(datax)
summary(datax)
datax <- split_dataset(data = datax, prop = 0.5, seedx=16001)

d_a <- datax$A
d_b <- datax$B

m_cor <- cor( d_a, method = 'pearson') # method='spearman' se ci sono variabili categoriali (default o pearson altrimenti)
D <- dist(m_cor, method = 'euclidean')
hc <- hclust(d = D, method = 'ward.D2')
plot(hc)
mod_1 <- hclust2lavaan(tree = hc, ngroups = 2)

#2 
mod_1_fit <- cfa( model = mod_1, data = d_b) #varibili continue
# se non hai il dataset ma matrice di cor utilizzare sample.cov=X, sample.nobs=100,
# per ortogonale o orthogonal=TRUE o lat1 ~~ 0*lat2
# per UVI std.lv=TRUE
# non devono esserci variabili categoriali non ordinali nel dataset che si passa
lavaan_checkConvergence(mod_1_fit)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)

p = NCOL(d_a)
100*(31/(p*(p+1)/2))
# (p * (p +1) )/2

# 3
mod_2 <- "eta1 =~ Y1+Y8+Y15 \n eta2 =~ Y3+Y9+Y10+Y11+Y13+Y14"
mod_2_fit <- cfa( model = mod_2, data = d_b)
lavaan_checkConvergence(mod_2_fit)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)

# 4
mod_3 <- "eta1 =~ Y1+Y8+Y15+Y3+Y9+Y10+Y11+Y13+Y14"
mod_3_fit <- cfa( model = mod_3, data = d_b)
lavaan_checkConvergence(mod_3_fit)
summary(mod_3_fit, fit.measures = TRUE, standardized=TRUE )
round(fitmeasures(object = mod_3_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)

p = NCOL(d_a)
100*(18/(p*(p+1)/2))

# 5
mod_4 <- "eta1 =~ Y1+Y8+Y15+Y3+Y9+Y10+Y11+Y13+Y14
Y1~~Y8
Y8~~Y15
Y15~~Y3
Y3~~Y9
Y10~~Y11
Y11~~Y13
Y13~~Y14"

mod_4_fit <- cfa( model = mod_4, data = d_b)
lavaan_checkConvergence(mod_4_fit)
summary(mod_4_fit, fit.measures = TRUE, standardized=TRUE )
round(fitmeasures(object = mod_4_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)

err_m1 = kFold_validation(model_definition= mod_3, dwls = TRUE, data= d_b[,c('Y1','Y8','Y15','Y3','Y9','Y10','Y11','Y13','Y14')], nfold= 7, error= 'montecarlo', force_crossValid= TRUE, B= 20)
err_m2 = kFold_validation(model_definition= mod_4, dwls = TRUE, data= d_b[,c('Y1','Y8','Y15','Y3','Y9','Y10','Y11','Y13','Y14')], nfold= 7, error= 'montecarlo', force_crossValid= TRUE, B= 20)
err_m1 =  kFold_validation(model_definition= mod_1, dwls = TRUE, data= d_b, nfold= 7, error= 'montecarlo', force_crossValid= TRUE, B= 20)
boxplot(err_m1, err_m2)
t.test(err_m1, err_m2)


# (p * (p +1) )/2
