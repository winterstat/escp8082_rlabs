##############################
# ESCP 8082 R Labs           #
# Created by Sonja D. Winter #
##############################

## install.packages("MBESS")

library(psych)
library(MBESS)

data("attitude")

describe(attitude)

fa(attitude)$loadings

ci.reliability(attitude, type = "omega", 
               conf.level = 0.95, 
               interval.type = "mlr")

# compute the SD of the attitude sum scores
sd_x <- sd(rowSums(attitude))

# compute sem: sd_x * sqrt(1 - reliability)
sem <- sd_x * sqrt(1 - 0.8563268)
sem

attitude_alpha <- alpha(attitude)

summary(attitude_alpha)

attitude_alpha$alpha.drop

attitude_alpha$feldt

ci.reliability(attitude, type = "alpha", 
               conf.level = 0.95, 
               interval.type = "feldt")

splitHalf(attitude)

# Split attitude data into two parts 
# for demonstration
part1 <- attitude[,c(1:4)]
part2 <- attitude[,c(5:7)]

# Compute summed scores of 
# each part using rowSums()
sumscore1 <- rowSums(part1)
sumscore2 <- rowSums(part2)

# Compute correlation between summed scores
obscor <- cor(sumscore1,sumscore2)
obscor

#examine tau equivalence
fa(part1)$loadings
fa(part2)$loadings

#record omega reliability estimates of both parts
omega1 <- ci.reliability(part1)$est
omega2 <- ci.reliability(part2)$est

omega1
omega2

# Disattenuated correlation between tests
discor <- obscor / sqrt(omega1 * omega2)
discor
