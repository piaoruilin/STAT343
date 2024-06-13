#Question2
library(nnet)

alligators <- read.table("Alligators2.dat", header = TRUE)

model <- multinom(y1 ~ y5 + lake, data = Alligators2)
summary(model)

coefficients <- summary(model)$coefficients
coefficients_rounded <- round(coefficients, 3)
coefficients_rounded