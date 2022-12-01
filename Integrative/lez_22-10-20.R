iris #dataframe di default già riempiti

str(iris) #struttua dataset (factor -> variabile qualitative)

NROW(iris) #numero righe
NCOL(iris) #numero colonne

head(iris) #stampa prime 6 righe del dataset

summary(iris) #sommario delle variabili, statistiche di sintesi

iris$Species #accesso a colonna
levels(iris$Species) #tutte le altenative (livelli) di un qualcosa (colonna tabella)

vex = table(iris$Species) #vettore con conteggio di tutti livelli
vex / NROW(iris$Species) #divido vettore per numero oggetti per vedere la percentuale di scelta di ognun

iris[1,1] #accedo a prima riga e prima colonna del dataframe
iris[1:3, 1] #accedo alle prime 3 righe e alla prima colonna
iris[c(2,5),1] #accedo alla riga 2 e 5 e alla prima colonna
iris[1:5, 1:2] #accedo alle prime 5 righe e alle 2 colonne

iris[1,] #prima riga e tutte le colonne
iris[,1:3] #tutte le righe delle prime 2 colonne

mean(iris$Petal.Length) #media della lunghezza dei setali

mean( iris$Sepal.Length[iris$Species ==  'setosa'] ) #media di solo i sepali della specie setosa
mean( iris[iris$Species == 'setosa' , 1] )

dataNostro = data.frame("lung_sepali" = iris$Sepal.Length,
                        "lung_petali" = iris$Petal.Length) #creo dataframe con solo quesgli oggetti con quei nomei

hist( dataNostro$lung_sepali ) #Istogramma della colonna del nostro nuovo dataframe
plot( dataNostro$lung_sepali, dataNostro$lung_petali)#grafico correlazione di due colonne

dat2 = dataNostro[ dataNostro$lung_petali >= 2.5 , ] #tolgo i dati con lunghezza petali sotto i 2.5
plot(dat2)

cor(dat2)
cor(dat2$lung_sepali, dat2$lung_petali) #correlazione lineare tra due variabili

modello = lm(lung_petali ~ lung_sepali, data = dat2) #modello regressione lineare
print(modello)

plot(dat2)
abline(modello$coefficients, col='pink') #aggiunta linea al grafico

#ESERCIZIO
str(mtcars)
mtcars[,1]
head(mtcars)
