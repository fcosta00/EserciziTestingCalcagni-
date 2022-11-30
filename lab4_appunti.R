source('Utilities-20221124/split_dataset.R')

#import dataset con File -> import dataset -> from text(base) -> e controllare i parametri che scegliamo
#alternativamente read.csv(file = '..', header = TRUE)

str(bfi_reversed)
summary(bfi_reversed) #gender age e education sono categoriali

bfi_reversed$gender <- as.factor(bfi_reversed$gender) #costringo R a interpretare gender come fattore(categoriale)

library(lavaan); library(semPlot); library(corrplot)

barplot(table(bfi_reversed[,1]))
plot(table(bfi_reversed[,1])) #brutto raro

#automatizziamo la creazione dei 25 grafici
x11();par(mfrow=c(5,5)) #tabella 5x5 dove sistemo i grafici
for(j in 1:25){
  barplot(table(bfi[,j]),main=colnames(bfi_reversed)[j])  
} #ci sono item che hanno praticamente risposto le stesse domande, questo significa che l'item di interesse è poco informativo in quanto non c'è differenza nel nostro campione testato

#CIAO TOMMI SEI L'EROE DEL CORSO TI AMIAMO TUE FANS ANNA E ANNA  <3

bfi_split <- split_dataset(data = bfi_reversed, prop = 0.5, seedx =121 ) #splitto il dataset in proporzione 0.5 nella prima parte, seedx è il seme per la generazione random

bfi_a <- bfi_split$A #efa
bfi_b <- bfi_split$B #cfa

bfi_a_cor <- cor( bfi_a[,1:25], method ='spearman') #spearman perchè sono categoriali

corrplot(corr =bfi_a_cor, method = 'circle')
heatmap(bfi_a_cor, symm= TRUE, hclustfun = hclust)

#ascolta i cucineremo ciabelle per piangere

#estrazione gruppi automatica, facoltativa
bfi_hclust = hclust( d= dist(bfi_a_cor), method = 'ward.D2')
plot(bfi_hclust);
bfi_grps = cutree(tree = bfi_hclust, k = 3)
names(bfi_grps[bfi_grps==1])
