library(lavaan)
library(semPlot)

load('Datasets-20221124/wisc.Rdata')

source('Utilities-20221124/plot_lavaan_model.R')


mod_M3_def =  ' VCI =~ SO+VC+CO \n
                PRI =~ DC+CI+RM \n
                WMI =~ MC+LN \n
                PSI =~ CR+RS \n'

mod_M3_1_def =  'VCI =~ SO+VC+CO \n
                PRI =~ DC+CI+RM+SO \n
                WMI =~ MC+LN \n
                PSI =~ CR+RS \n'

mod_M3_2_def =  'VCI =~ SO+VC+CO \n
                PRI =~ DC+CI+RM+SO \n
                WMI =~ MC+LN \n
                PSI =~ DC+CR+RS \n'

mod_M3_fit = cfa(model = mod_M3_def, data = wisc)
mod_M3_1_fit = cfa(model = mod_M3_1_def, data = wisc)
mod_M3_2_fit = cfa(model = mod_M3_2_def, data = wisc)

plot_lavaan_model(mod_M3_fit)
plot_lavaan_model(mod_M3_1_fit)
plot_lavaan_model(mod_M3_2_fit)

