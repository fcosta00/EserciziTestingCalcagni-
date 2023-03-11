# simulazione esa,e es 12


source('Utilities-20221124/utilities.R')
library(lavaan)
library(semPlot)
library(mvtnorm)
load('Datasets-20221124/finance.Rdata')

datax <- finance

str(datax)
head(datax)
summary(datax)

datax$PPGENDER <- as.factor(datax$PPGENDER)

# 1
datax <- split_dataset(data = datax, prop = 0.3, seed = 8219291)
d_a <- datax$A
d_b <- datax$B

# 2
m_cor <- cor( d_a[,2:11], method = 'spearman') # method='spearman' se ci sono variabili categoriali (default o pearson altrimenti)
D <- dist(m_cor, method = 'euclidean')
hc <- hclust(d = D, method = 'ward.D2')
plot(hc)
cutree(tree = hc, k = 2)
mod_1 <-hclust2lavaan(tree = hc, ngroups = 2)
#il modello secondo le analisi delle altezze nel dendrogramma risultante alla efa propone due variabili latenti così divise eta1 =~ FWB1_1+FWB1_2+FWB1_4+FWB2_2 \n eta2 =~ FWB1_3+FWB1_5+FWB1_6+FWB2_1+FWB2_3+FWB2_4

#3
#lo eseguo dopo in quanto mi serve numerico per la matrice di correlazione
for(j in 2:(NCOL(d_b)-1)){
  d_b[,j] = factor(d_b[,j],ordered = TRUE)
}
d_b$PPGENDER <- as.character(d_b$PPGENDER)

d_b <- d_b[,2:12]

mod_1_fit = cfa( model = mod_1, data = d_b, ordered=colnames(d_b)[1:10], estimator="DWLS")
lavaan_checkConvergence(mod_1_fit)
round(fitmeasures(object = mod_1_fit, fit.measures = c('RMSEA', 'CFI', 'AIC', 'chisq', 'df', 'npar')), 3)
summary(mod_1_fit, fit.measures = TRUE, standardized=TRUE )

mod_1_conf = cfa(model = mod_1, data = d_b, order = colnames(d_b)[1:10], estimator = 'DWLS', group = 'PPGENDER')
mod_1_weak = cfa(model = mod_1, data = d_b, order = colnames(d_b)[1:10], estimator = 'DWLS', group = 'PPGENDER',group.equal="loadings")
anova(mod_1_conf,mod_1_weak)
# il modello non è nemmeno invariante debole infatti è statisticamente differente. questo significa che anche solo il vincolo dei lambda uguali è eccessivo per garantire la bontà del modello, perciò gli item non sono interpretati allo stesso modo tra i generi



evaluate_partial_invariance(fitted_model = mod_1_weak, type = "metric")
#Cerchiamo lambda che sono presenti in entrambi gruppi con alto indice di modifica
# eta2 =~ FWB1_5 e eta2 =~ FWB1_3 e eta2 =~ FWB2_1 candidati parametri da liberare
#Più parametri perché dobbiamo modificare tanto il nostro pvalue
mod_1_weak_parz = cfa(model = mod_1, data = d_b, ordered = colnames(d_b), 
                      estimator = "DWLS", group = "PPGENDER", group.equal = c("loadings"), 
                      group.partial = c("eta2 =~ FWB1_5","eta2 =~ FWB1_3","eta2 =~ FWB2_1"))
anova(mod_1_weak_parz, mod_1_conf)

mod_1_strong_parz = cfa(model = mod_1, data = d_b, ordered = colnames(d_b), 
                      estimator = "DWLS", group = "PPGENDER", group.equal = c("loadings",'intercepts'), 
                      group.partial = c("eta2 =~ FWB1_5","eta2 =~ FWB1_3","eta2 =~ FWB2_1"))
anova(mod_1_weak_parz,mod_1_strong_parz)

summary_table(fitted_model = mod_1_strong_parz, type_summary = 'intercept', standardized = TRUE)
