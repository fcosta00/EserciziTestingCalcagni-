library(corrplot)

R = cor(mtcars)
R

corrplot(R, type = 'upper', method='ellipse')

heatmap(R, scale='column')
corrplot(R, type = 'upper', method='ellipse', order = 'hclust')


#meglio perchè i dati non sono normali ma ci sono parametri speudo qualitativi
Rs = cor(mtcars, method = 'spearman')
Rs

corrplot(Rs, type = 'upper', method='ellipse')

heatmap(Rs, scale='column')
corrplot(Rs, type = 'upper', method='ellipse', order = 'hclust')

qqnorm(mtcars$disp)

library(qgraph)
qgraph(Rs, minimum = 0.3)

str(mtcars)


load('casi_studio/080222/dati_esame.Rdata')

str(dass21)
summary(dass21)

dass21$Gender = NULL
R = cor(dass21, method='spearman')
corrplot(R, type = 'upper', method='ellipse')
corrplot(R, type = 'upper', method='ellipse', order = 'hclust')
qgraph(R, minimum = 0.3)
