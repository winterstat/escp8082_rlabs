##############################
# ESCP 8082 R Labs           #
# Created by Sonja D. Winter #
##############################

library(lavaan)
library(psych)
library(GPArotation)

data("HolzingerSwineford1939")

# print the variable names of the full data frame 
# and locate the relevant variables
colnames(HolzingerSwineford1939)

# use the [,] operator to select only the relevant 
# columns/variables (here in column 7 to 15)
HSdata <- HolzingerSwineford1939[,7:15]

fa.parallel(HSdata, fa = "fa", n.iter = 50)

efa_3f <- fa(HSdata, nfactors = 3, 
             fm = "minres", 
             rotate = "oblimin")

round(efa_3f$communalities, 
      digits = 3)

print(efa_3f$loadings, 
      cutoff = .3)

print(efa_3f$loadings, 
      cutoff = -1)

round(efa_3f$Phi, 
      digits = 3)

efa_2f <- fa(HSdata, nfactors = 2, 
             fm = "minres", 
             rotate = "oblimin")

round(efa_2f$communalities, 
      digits = 3)

print(efa_2f$loadings, 
      cutoff = .3)

round(efa_2f$Phi, 
      digits = 3)

efa_4f <- fa(HSdata, nfactors = 4, 
             fm = "minres", 
             rotate = "oblimin")

round(efa_4f$communalities, 
      digits = 3)

print(efa_4f$loadings, 
      cutoff = .3)

round(efa_4f$Phi, digits = 3)
