#   E S E R C I Z I O   1

str(mtcars)       #verifico struttura dataset
summary(mtcars)   #non necessario, interessante vedere distribuzione variabili

nrow(mtcars)      #numero righe (obj)
ncol(mtcars)      #numero colonne(vrb)

head(mtcars)      #stampa prime righe dataset

mean(mtcars$mpg)  #calcolo la media della colonna mpg
sd(mtcars$mpg)    #calcolo la deviazione standard della colonna mpg

quartili = quantile(mtcars$mpg, probs = seq(0, 1, 0.25))  #ricavo i quartili da 0 a 1 di probabilità con salti di 0.25
print(quartili)
quantile99 = quantile(mtcars$mpg, probs = 0.99)           #ricavo quantile a 0.99
print(quantile99)

hist(mtcars$mpg, main='hist MPG') #istogramma con titolo modificato
hist(mtcars$cyl, main='hist CYL')

counts = table(mtcars$cyl)
barplot(counts, main="Barplot di cyl", ylim = c(0,15))  #faccio un bar plot di solo i 3 valori possibili

plot(mtcars$cyl, mtcars$mpg)  #grafico mettendo in relazione cyl e mpg

regRetta = lm( mtcars$mpg ~ mtcars$cyl )    #calcolo retta regressione
abline( regRetta$coefficients, col='red' )



#   E S E R C I Z I O   2

str(cars)       #verifico struttura dataset
summary(cars)   #non necessario, interessante vedere distribuzione variabili

nrow(cars)      #numero righe (obj)
ncol(cars)      #numero colonne(vrb)

head(cars)      #stampa prime righe dataset

mean(cars$speed)  #calcolo la media della colonna speed
sd(cars$dist)    #calcolo la deviazione standard della colonna dist

quartili = quantile(cars$speed, probs = seq(0, 1, 0.25))  #ricavo i quartili da 0 a 1 di probabilità con salti di 0.25
print(quartili)
quantile99 = quantile(cars$speed, probs = 0.99)           #ricavo quantile a 0.99
print(quantile99)

hist(cars$speed, main='hist speed') #istogramma con titolo modificato
hist(cars$dist, main='hist dist')

plot(cars$speed, cars$dist)  #grafico mettendo in relazione speed e dist

regRetta = lm( dist ~ speed, data=cars )    #calcolo retta regressione !!usare sempre la forma con data esplicito
abline( regRetta$coefficients, col='red' )

#forma breve per aggiungere una variabile a un dataset
cars$speedKmh <- (cars$speed * 1.609) #aggiungo un speedKmh facendo una operazione sul vettore 
cars$distKm <- (cars$dist * 0.3048)   #aggiungo un distKm facendo una operazione sul vettore 

predict(regRetta, newdata = data.frame("speed" = 10)) #funzione per predirre i valori in newdata col modello regRetta
beta = coef(regRetta)
beta[1] + beta[2] * 10


