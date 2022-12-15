load('Datasets-20221124/wisc_simdata.Rdata')
library(lavaan)

w_t <- wisc[wisc$group == 'tipici',]
w_a <- wisc[wisc$group == 'atipici',]

#wisc$group <- as.character(wisc$group) per raggruoppare con lavaan serve sempre un carattere

#livello 1: invarianza configurale baseline
mod1 =  ' VCI =~ SO+VC+CO \n
          PRI =~ DC+CI+RM \n
          WMI =~ MC+LN \n
          PSI =~ CR+RS \n'

mod1_conf = cfa(model = mod1, data = wisc, group = 'group')
summary(mod1_conf)


mod_debole = cfa(model = mod1, data = wisc, group = 'group', group.equal = 'loadings')
#group.equal = 'loadings impone le matrici lambda uguali
#verificare la cosa con test del chi quadro
anova(mod_debole, mod1_conf, test = 'chisq') #> 0.05

mod_forte = cfa(model = mod1, data = wisc, group = 'group', group.equal = c('loadings','intercepts'))
#group.equal = 'loadings impone le matrici lambda uguali
anova(mod_forte, mod_debole, test = 'chisq') #> 0.05

mod_extreme = cfa(model = mod1, data = wisc, group = 'group', group.equal = c('loadings','intercepts','residuals'))
anova(mod_extreme, mod_forte, test = 'chisq')
#tocca rifiutare

