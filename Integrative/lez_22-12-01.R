library(lavaan)
library(semPlot)

load('Datasets-20221124/wisc.Rdata')

source('Utilities-20221124/plot_lavaan_model.R')
source('Utilities-20221124/lavaanExt.R')


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

set.seed(123)
n = NROW(wisc)
indiciRighe = 1:n
indiciTest = sample(indiciRighe, 0.5*n)

testData = wisc[indiciTest, ]
trainData = wisc[-indiciTest, ]

mod_M3_fit = cfa(model = mod_M3_def, data = trainData)
mod_M3_1_fit = cfa(model = mod_M3_1_def, data = trainData)
mod_M3_2_fit = cfa(model = mod_M3_2_def, data = trainData)

errLavaan(testData, mod_M3_fit)
errLavaan(testData, mod_M3_1_fit)
errLavaan(testData, mod_M3_2_fit)

#CROSS VALIDATION
nfold = 10
folds = cut( seq(1,NROW(wisc)), breaks = nfold, labels = FALSE) 

output = matrix(NA, nrow = nfold, ncol = 3)

for (i in 1:nfold) {
  indiciTest = which( folds == i)
  
  testData = wisc[indiciTest, ]
  trainData = wisc[-indiciTest, ]
  
  mod_M3_fit = cfa(model = mod_M3_def, data = trainData)
  mod_M3_1_fit = cfa(model = mod_M3_1_def, data = trainData)
  mod_M3_2_fit = cfa(model = mod_M3_2_def, data = trainData)
  
  output[i, 1] = errLavaan(testData, mod_M3_fit)
  output[i, 2] = errLavaan(testData, mod_M3_1_fit)
  output[i, 3] = errLavaan(testData, mod_M3_2_fit)
  
}

colnames(output) = c('err_mod', 'err_mod1', 'err_mod2')

boxplot(output)
