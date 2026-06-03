##############################
# ESCP 8082 R Labs           #
# Created by Sonja D. Winter #
##############################

install.packages(c("rio", "ggplot2", "psych","correlation",
                    "GPArotation", "lavaan", "MBESS",
                    "semTools", "lavaan.mi"),
                  dependencies = TRUE)


library(rio)


tempice <- import(file = "data/tempice.csv")


getwd()


# # Mac OS:
# setwd("~/Dropbox/Work/Teaching/Measurement/R Labs")
# 
# # Windows:
# setwd("C:/Users/sonja/Dropbox/Work/Teaching/Measurement/R Labs")
# 
# # Note: the folder that you are using for this class will very
# # likely be in a different location.

x <- 11
x

x + 1

summary(tempice)

