##############################
# ESCP 8082 R Labs           #
# Created by Sonja D. Winter #
##############################


library(rio)
library(psych)
library(ggplot2)
library(correlation)

tempice <- import(file = "data/tempice.csv")

## getwd()

## # Mac OS:
## setwd("~/Dropbox/Work/Teaching/Measurement/R Labs")
## 
## # Windows:
## setwd("C:/Users/sonja/Dropbox/Work/Teaching/Measurement/R Labs")
## 
## # Note: the folder that you are using for this class will very
## # likely be in a different location.

1 + 1

5 / 3.21

4*4

x <- 10

y <- 5

x*y

z <- c(1, 2, 3, 4)

z * x

z[1]

z[1:3]

z[c(1,2,4)]

# retrieve the value in the first row, first column
tempice[1,1]

# retrieve the first column
tempice[,1]

# retrieve the second column by using its column name
tempice$x2

# get some summary information about each column
summary(tempice)

# Create scatterplot of variables x1 and x2
plot(x = tempice$x1, y = tempice$x2,
     xlab = "Temperature (F)",
     ylab = "Ice cream sales ($)")

ggplot(tempice, aes(x = x1, y = x2)) +
  geom_point() +
  labs(x = "Temperature (F)",
       y = "Ice cream sales ($)")

tempice

# Mean:
x1bar <- (67.56 + 71.52 + 63.42 + 69.36 + 75.30 + 81.78 +
            76.92 + 87.18 + 84.12 + 74.58 + 82.68 + 72.96) / 12

# Mean (less by hand):
x1bar_2 <- sum(tempice$x1) / nrow(tempice)

# The result is equivalent:
x1bar
x1bar_2

# Variance:
s2x1 <- ((67.56 - x1bar) ^ 2 +
           (71.52 - x1bar) ^ 2 +
           (63.42 - x1bar) ^ 2 +
           (69.36 - x1bar) ^ 2 +
           (75.30 - x1bar) ^ 2 +
           (81.78 - x1bar) ^ 2 +
           (76.92 - x1bar) ^ 2 +
           (87.18 - x1bar) ^ 2 +
           (84.12 - x1bar) ^ 2 +
           (74.58 - x1bar) ^ 2 +
           (82.68 - x1bar) ^ 2 +
           (72.96 - x1bar) ^ 2) / (12 - 1)

# Variance (less by hand):
s2x1_2 <- sum((tempice$x1 - x1bar)^2) / (nrow(tempice) - 1)

# Standard deviation:
sx1_2 <- sqrt(s2x1)
sx1_2

# Getting these things by doing even less by hand:
x1bar <- mean(tempice$x1)
s2x1 <- var(tempice$x1)
sx1 <- sd(tempice$x1)
sx1

# Same idea for variable x2:
x2bar <- mean(tempice$x2)
s2x2 <- var(tempice$x2)
sx2 <- sd(tempice$x2)

# Compute the sum of cross-products:
CP <- (tempice$x1 - x1bar) * (tempice$x2 - x2bar)
CP

sumCP <- sum(CP)
sumCP

# Sample size
n <- nrow(tempice) 
n

# Covariance and correlation
covariance <- sumCP/(n - 1)
covariance

correlation <- covariance/(sx1 * sx2)

# Are ice cream sales and temperature correlated?
correlation

cov(tempice$x1, tempice$x2)
cor(tempice$x1, tempice$x2)

cor.test(tempice$x1, tempice$x2)

tempicecurve <- import(file = "data/tempicecurve.csv")

plot(tempicecurve$x1, tempicecurve$x2, pch=19,
     xlab = "Temperature (F)",
     ylab = "Ice cream sales ($)",
     ylim = c(0,800))

cor.test(tempicecurve$x1, tempicecurve$x2)

plot(tempicecurve$x1,tempicecurve$x2,pch=19,
     xlab = "Temperature (F)",
     ylab = "Ice cream sales ($)",
     ylim = c(0,800),
     col = tempicecurve$group)


# select only group = 1 (cooler to hot temps)
tempicecurve1 <- subset(tempicecurve, group == 1)
cor.test(tempicecurve1$x1, tempicecurve1$x2)

# select only group = 2 (hot to hottest temps)
tempicecurve2 <- subset(tempicecurve, group == 2)
cor.test(tempicecurve2$x1, tempicecurve2$x2)

SATscores_out <- rio::import("data/SATscores_outlier.csv")

hist(SATscores_out$verbal, xlab = "Verbal", main = "")
hist(SATscores_out$quant, xlab = "Quant", main = "")

# Pearson
cor.test(SATscores_out$verbal, SATscores_out$quant)

# biweight
cor_test(SATscores_out, "verbal", "quant", method = "biweight")

# Winsorized
cor_test(SATscores_out, "verbal", "quant", winsorize = TRUE)


SATscores <- SATscores_out[1:10,]

# Shapiro Wilk test of normality.
shapiro.test(SATscores$verbal)
shapiro.test(SATscores$quant)

# Pearson
cor.test(SATscores$verbal, SATscores$quant)

# Spearman (you can use cor.test or cor_test)
# cor.test(SATscores$verbal, SATscores$quant, method = "spearman")
cor_test(SATscores, "verbal", "quant", method = "spearman")

# Kendall (you can use cor.test or cor_test)
#cor.test(SATscores$verbal, SATscores$quant, method = "kendall")
cor_test(SATscores, "verbal", "quant", method = "kendall")

SATscores_rank <- data.frame(verbal = rank(SATscores$verbal),
                             quant = rank(SATscores$quant))

# Camparing Pearson correlation coefficients 
# (now using the cor_test function)
cor_test(SATscores, "verbal", "quant", method = "pearson")
cor_test(SATscores_rank, "verbal", "quant", method = "pearson")

# Comparing Spearman correlation coefficients
cor_test(SATscores, "verbal", "quant", method = "spearman")
cor_test(SATscores_rank, "verbal", "quant", method = "spearman")

QoLHealth <- import("data/QoLHealth.csv")

QoLHealth$health <- factor(QoLHealth$health, level = c("Poor", "Moderate","Good"), ordered = T)
QoLHealth$QoL <- factor(QoLHealth$QoL, level = c("Low", "Medium", "High"), ordered = T)

table(QoLHealth)

cor_test(QoLHealth, "health", "QoL", method = "polychoric")

polychoric(table(QoLHealth))
