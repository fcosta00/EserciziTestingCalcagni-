set.seed(123)
campione = rnorm(100) #set normale di 100 valori da 0 a 1

testEsempio = t.test(campione)

curve(dt(x, df=testEsempio$parameter), from = -4, to = 4, yla= 't')

abline(v= testEsempio$statistic, lty = 'dashed')

load('casi_studio/080722/dati_esame.Rdata')

boxplot(datax$Y1)
boxplot(datax$Y1, outline = FALSE)
mean(datax$Y1)
boxplot(Y1 ~ group_label, data=datax, outline = FALSE)

yslow = datax$Y1[datax$group_label == "slow"]
yfast = datax$Y1[datax$group_label == "fast"]

c(mean(yslow), mean(yfast))

mean(yslow) - mean(yfast)

testEq = t.test(yslow, yfast, var.equal = TRUE) #confronto assumendo che le varianze siano uguali
testEq

testDiv = t.test(yslow, yfast, var.equal = FALSE) #confronto assumendo che le varianze siano uguali
testDiv

mod0 = lm(Y1 ~ 1, data= datax)
summary(mod0)

mean(datax$Y1)


mod1 = lm(Y1 ~ group_label, data= datax)
summary(mod1)
 
par = mod1$coefficients
par[1] + par[2]



