head(mtcars)
pc = prcomp(mtcars, scale. = TRUE)
pc
plot(pc, type = 'l')

var_spiegata = pc$sdev^2
prop = cumsum(var_spiegata) / sum(var_spiegata)

plot(prop, type="b", ylim = c(0,1))

biplot(pc)
pc$rotation
pc

ggplot2::plot

.Machine$double.eps




load('Integraztive/casi_studio/250121/dati_esame.Rdata')

heatmap( cor(Y0), scale=  'none') 

cor()

pc = prcomp( Y0, scale. = TRUE )
pc
plot(pc, type = 'l')

var_spiegata = pc$sdev^2
prop = cumsum(var_spiegata) / sum(var_spiegata)

plot(prop, type="b", ylim = c(0,1))

x11();biplot(pc)

