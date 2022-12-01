#!!!!!!!!!!!!!!!!!!!!! S T U D I A !!!!!!!!!!!!!! 

# Esercizi e prove personali non presenti a lezione
{
plot(mpg ~ hp, data = mtcars)

mtcars_reg <- lm(mpg ~ hp, data = mtcars)
summary(mtcars_reg)

par(mfrow = c(3,1), mar = c(2,2,1,1)) #tre grafici in fila

{
  # Retta di regressione
  plot(mpg ~ hp, data = mtcars)
  abline(mtcars_reg$coefficients, col = "red")
  
  # Pattern nei residui
  plot(mtcars_reg$residuals, main = "Residui")
  
  # Distribuzione in quantili 
  qqnorm(mtcars_reg$residuals)
  qqline(mtcars_reg$residuals)
  
  plot(mtcars_reg, which = 1)
} #metodi verificare bontà modello

#prova regressione curva
mtcars_reg2 <- lm(mpg ~ hp + I(hp^2), data = mtcars)
summary(mtcars_reg2)
plot(mtcars_reg2, which = 1)


x = seq(min(mtcars$hp),
        max(mtcars$hp),
        length = 1000)

yhat = predict(mtcars_reg2, newdata = data.frame('hp'= x))

plot(mpg ~ hp, data = mtcars)

lines(x, yhat, col=2)

#es1
summary(mtcars)

plot(mpg ~ wt, data = mtcars)
mpgwt_reg <- lm(mpg  ~ wt, data = mtcars)

summary(mpgwt_reg)

plot(mpg  ~ wt, data = mtcars)
abline(mpgwt_reg$coefficients, col = "red")

plot(mpgwt_reg, which=1)

mpgwt_reg2 <- lm(mpg  ~ wt + I(wt^2), data = mtcars)
summary(mpgwt_reg2)

plot(mpgwt_reg2, which=1)

x = seq(min(mtcars$wt),
        max(mtcars$wt),
        length = 1000)

yhat = predict(mpgwt_reg2, newdata = data.frame('wt'= x))

plot(mpg ~ wt, data = mtcars)

lines(x, yhat, col=2)


#es regrex multipla
multi_reg <- lm(mpg  ~ wt + hp, data = mtcars)
summary(multi_reg)

#prova reg multipla sensata
multi_reg_pro <- lm(mpg  ~ ., data = mtcars)
multi_reg_pro <- lm(mpg  ~ wt * am * hp, data = mtcars)
summary(multi_reg_pro)

plot(mpg  ~ am, data= mtcars )

multi_reg <- lm(mpg  ~ wt + am + hp, data = mtcars)
summary(multi_reg)

} # https://thefreolo.github.io/book/regressione.html#regressione-multipla



# E S E R C I Z I O   1
{
  summary(iris)
  
  #stesso identi grafici, x(ascisse) -> Sepal.Length, y(ordinate) -> Petal.Width,
  plot( iris$Sepal.Length, iris$Petal.Width )
  plot( Petal.Width ~ Sepal.Length , data = iris)
  
  #coloro il grafico in base alla specie
  plot( iris$Sepal.Length, iris$Petal.Width, col = iris$Species )
  plot( iris$Sepal.Length, iris$Petal.Width, pch = 20, col = c('red', 'blue', 'green')[iris$Species])
  
  #la specie sembra essere indicativa in quanto nei due cluster le specie non sono mescolate ma son ben divise (nel cluster 1 è presente solo una specie)
  
  # Modello senza Species
  modNoSpecies = lm(Petal.Width ~ Sepal.Length, data=iris)
  summary(modNoSpecies)
  
  # Modello con Species
  modNoInt = lm(Petal.Width ~ Sepal.Length + Species, data=iris)
  summary(modNoInt)
  
  # Modello con interazione Sepal.Length e Species (equivalente a dire Sepal.Length*Species, coefficiente angolare specifico)
  modInt = lm(Petal.Width ~ Sepal.Length + Species + Sepal.Length:Species, data=iris)
  summary(modInt)
  
  
  library(sjPlot)
  plot_model(modNoSpecies, type="pred", terms=c("Sepal.Length"), ci.lvl = NA)
  plot_model(modNoInt, type="pred", terms=c("Sepal.Length", "Species"), ci.lvl = NA)
  plot_model(modInt, type="pred", terms=c("Sepal.Length", "Species"), ci.lvl = NA)
  
  
  
  #controllo se il modello di interazione è significativo rispetto a quello senza interazione
  anova(modNoInt, modInt)
  
  #valore del test F molto alto, accetto per cui l'ipotesi H0, il modello con interazione non è significativamente differente
  
  anova(modInt) #test che mostra la devianza dovuta a ogni regressore inserito
  
}

# E S E R C I Z I O   2
{
  library(MASS)
  
  #funzioni per capire la struttura del database che stiamo analizzando
  summary(anorexia) 
  head(anorexia)
  
  #converto il peso da libbre a kg
  anorexia$Prewt <- anorexia$Prewt*0.4535
  anorexia$Postwt <- anorexia$Postwt*0.4535
  
  #grafico per il confronto delle terapie pre e post
  boxplot(anorexia$Prewt[anorexia$Treat == 'Cont'], anorexia$Postwt[anorexia$Treat == 'Cont'], 
          anorexia$Prewt[anorexia$Treat == 'CBT'], anorexia$Postwt[anorexia$Treat == 'CBT'],
          anorexia$Prewt[anorexia$Treat == 'FT'], anorexia$Postwt[anorexia$Treat == 'FT'],
          names = c("Cont Pre","Cont Post","CBT Pre", 'CBT Post', 'FT Pre', 'FT Post'),
          col = c('cyan4', 'cyan3', 'coral4', 'coral3', 'darkolivegreen4', 'darkolivegreen3'),
          ylab = 'Weigth')
  
  #grafico per il confronto delle terapie con delta post pre
  boxplot(Postwt-Prewt ~ Treat, data = anorexia,
          col = c('coral3', 'cyan3', 'darkolivegreen3'),
          ylab = 'Weigth')
  
  mod_interazione <- lm(Postwt ~ Prewt, data = anorexia[anorexia$Treat == 'FT',])
  plot(Postwt ~ Prewt, data = anorexia[anorexia$Treat == 'FT',])
  
  anorexiere = anorexia
  
  anorexia$Treat = relevel(anorexia$Treat, ref = "Cont")
  
  mod = lm(Postwt-Prewt ~ 0 + Treat, data = anorexia)
  mod = lm(Postwt-Prewt ~ Treat, data = anorexia)
  summary(mod)
  
  
  anorexiere$Treat <- relevel(anorexiere$Treat, "Cont")
  
  mod <- lm(Postwt ~ Prewt + Treat + Prewt:Treat, data = anorexiere)
  plot_model(mod, type="pred", terms=c("Prewt", "Treat"))
  
  modCont <- lm(Postwt-Prewt ~ Prewt + Treat + Prewt:Treat, data = anorexia)
  mod <- lm(Postwt-Prewt ~ Treat, data = anorexia)
  modCont <- lm(Postwt-Prewt ~ Treat, data = anorexia[anorexia$Treat == 'Cont',])
  summary(mod)
  
  plot( iris$Sepal.Length, iris$Petal.Width )
  plot( Postwt-Prewt ~ Treat , data = anorexia)
  
  plot_model(mod, type="pred", terms=c("Treat"), ci.lvl = NA)
  
  anova(modCont)
  
  boxplot(Postwt-Prewt ~ Treat, data = anorexia)
  
  
}
