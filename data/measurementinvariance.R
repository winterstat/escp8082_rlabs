##############################
# ESCP 8082 R Labs           #
# Created by Sonja D. Winter #
##############################

library(lavaan)
library(semTools)

finance <- read.csv("data/finance.csv")

psych::describe(finance, skew = FALSE)

# Two-factor CFA model
financemodel <- "positive =~ item1 + item2 + item4 + item8
                 negative =~ item3 + item5 + item6 + item7 + item9 + item10"


# Configural model
config <- cfa(model = financemodel, data = finance, 
              std.lv = TRUE,
              group = "sector")

summary(config, fit.measures = T, estimates = F)

## # Examine parameter estimates
## summary(config)
## 
## # Split data is two
## public <- subset(finance, sector == "public")
## private <- subset(finance, sector == "private")
## 
## # Run parallel analysis for each group
## # (can be followed up by full EFA examination)
## library(psych)
## fa.parallel(public[,1:10], fa = "fa")
## fa.parallel(private[,1:10], fa = "fa")

# Metric model
metric <- cfa(model = financemodel, data = finance, 
              group = "sector", 
              std.lv = TRUE,
              group.equal = "loadings")

summary(compareFit(config, metric))

# Scalar model
scalar <- cfa(model = financemodel, data = finance, 
              group = "sector", 
              std.lv = TRUE,
              group.equal = c("loadings","intercepts"))

summary(compareFit(metric, scalar))

# Adjust the model for partial invariance testing
lavTestScore(scalar)

# To view the entire parameter table, simply use this code 
# (remove the hashtag in front of the next line):
# parTable(scalar)

# To filter the output (this literal code will only work for 
# this example):
subset(parTable(scalar), op == "~1" & group == 2)[,c(1:4, 11:15)]

scalar2 <- cfa(model = financemodel, data = finance, 
               group = "sector",
               std.lv = TRUE,
               group.equal = c("loadings","intercepts"), 
               group.partial = c("item7 ~ 1")) 
# for a loading, group.partial would look like: "negative =~ item7"

summary(compareFit(metric, scalar2))

# Adjust the model for partial invariance testing
lavTestScore(scalar2)

scalar3 <- cfa(model = financemodel, data = finance, 
               group = "sector", 
               std.lv = TRUE,
               group.equal = c("loadings","intercepts"), 
               group.partial = c("item7 ~ 1", "item4 ~ 1"))

summary(compareFit(metric, scalar3))

# Adjust the model for partial invariance testing
lavTestScore(scalar3)

scalar4 <- cfa(model = financemodel, data = finance, 
               group = "sector", 
               std.lv = TRUE,
               group.equal = c("loadings","intercepts"), 
               group.partial = c("item7 ~ 1", "item4 ~ 1", 
                                 "item1 ~ 1"))

summary(compareFit(metric, scalar4))

#Strict model
strict <- cfa(model = financemodel, data = finance, 
              group = "sector", 
              std.lv = TRUE,
              group.equal = c("loadings","intercepts","residuals"), 
              group.partial = c("item7 ~ 1", "item4 ~ 1", 
                                "item1 ~ 1",
                                "item7 ~~ item7", 
                                "item4 ~~ item4",
                                "item1 ~~ item1"))

summary(compareFit(scalar4, strict))

subset(parameterEstimates(scalar4), 
       (op == "~1" & (lhs == "positive" | lhs == "negative")))

lvvar <- cfa(model = financemodel, data = finance, 
               group = "sector", 
               std.lv = TRUE,
               group.equal = c("loadings","intercepts", 
                               "lv.variances"), 
               group.partial = c("item7 ~ 1", "item4 ~ 1", 
                                 "item1 ~ 1"))

summary(compareFit(scalar4, lvvar))

subset(parameterEstimates(lvvar), 
       (op == "~1" & (lhs == "positive" | lhs == "negative")))

subset(parameterEstimates(scalar4), 
       (group == 2 & 
          (op == "~1" | op == "~~") & (lhs == "positive" | lhs == "negative")))

# positive
((-.216) - 0) / sqrt(1)

# negative
((.086) - 0) / sqrt(1)

# positive
((-.216) - 0) / sqrt(.973)

# negative
((.086) - 0) / sqrt(.967)
