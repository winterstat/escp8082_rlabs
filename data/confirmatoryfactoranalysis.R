##############################
# ESCP 8082 R Labs           #
# Created by Sonja D. Winter #
##############################

library(lavaan)
library(semTools)
library(psych)

data("HolzingerSwineford1939")

describe(HolzingerSwineford1939)

?HolzingerSwineford1939

#CFA model specification
HSmodel <- "visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9"

fit1 <- cfa(model = HSmodel, 
            data = HolzingerSwineford1939)

summary(fit1, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

fitMeasures(fit1, 
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "rmsea", "rmsea.ci.lower", 
                             "rmsea.ci.upper", "srmr"))

fitMeasures(fit1, 
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "rmsea", "rmsea.ci.lower", 
                             "rmsea.ci.upper", "srmr"),
            output = "text")

standardizedSolution(fit1, 
                     zstat = FALSE, pvalue = FALSE,
                     output = "text")

modindices(fit1, minimum.value = 10, sort = TRUE)

#Reanalysis
HSmodel2 <- "visual  =~ x1 + x2 + x3 + x9
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9"

fit2 <- cfa(model = HSmodel2, 
            data = HolzingerSwineford1939)

summary(fit2, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

comp_fit1_fit2 <- compareFit(fit1, fit2)
summary(comp_fit1_fit2)

HSmodel3 <- "visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9

            ability =~ visual + textual + speed"

HSmodel4 <- "visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9

            visual ~~ 0*textual
            visual ~~ 0*speed
            textual ~~ 0*speed"

HSmodel5 <- "visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9

            visual ~~ a*textual
            visual ~~ a*speed
            textual ~~ a* speed"
