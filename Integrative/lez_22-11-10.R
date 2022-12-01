load('casi_studio/080722/dati_esame.Rdata')
head(datax)

mod = lm( Y1 ~ Y2 + Y3 + Y4 + Y5 +  Y6 + Y7 + Y8 + group_label, data=datax) #pvalue test ipotesi tutti i coefficienti siano  0 rispetto al contratio

summary(mod)

install.packages('MASS')

library(MASS)

plot(height  ~ age, data=Loblolly)

modAge = lm(height ~ age, data=Loblolly)
summary(modAge)

cor(Loblolly$height, Loblolly$age)^2
abline(modAge, col = 2)
{
  # Pattern nei residui
  plot(modAge$residuals, main = "Residui")
  
  # Distribuzione in quantili 
  qqnorm(modAge$residuals)
  qqline(modAge$residuals)
  
  plot(modAge, which = 1)
  
  
}

#grafici diagnostici modello
#residui: variabile oss - stima

plot(modAge, which = 1)

#modello lineare non lineare in quanto metto un parametro quadratico
modAge = lm(height ~ age + I(age^2), data=Loblolly)
plot(height ~ age, data=Loblolly)


x = seq(min(Loblolly$age),
        max(Loblolly$age),
        length = 1000)

yhat = predict(modAge, newdata = data.frame('age'= x))

lines(x, yhat, col=2)

