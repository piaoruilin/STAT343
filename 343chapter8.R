###############
## Chapter 8 ##
###############
#####################################
## Example: Environmental Opinions ##
#####################################
## McNemar test
Opinions <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Envir_opinions.dat", 
                       header = T)
head(Opinions, 6)
tab <- xtabs(~ y1 + y2, data = Opinions)
tab
mcnemar.test(tab, correct = FALSE) # don't use continuity correction

## Marginal Models for Marginal Proportions
Opinions <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Opinions.dat", header = T)
head(Opinions, 6)
library(gee) 

fit <- gee(y ~ question, id = person, family = binomial(link = "identity"),data = Opinions)
summary(fit)
fit.lm <- gee(y ~ question, id = person, family = gaussian,data = Opinions)
summary(fit.lm) # estimated scale parameter is different. (Estimated coef. are the same)

fit2 <- gee(y ~ question, id = person, family = binomial(link = "logit"),data = Opinions)
summary(fit2)

########################################
## Example: Coffee Brand Market Share ##
########################################
Coffee <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Coffee.dat", header = T)
head(Coffee, 6)
library(multgee)
fit <- nomLORgee(y ~ purchase, id = person, LORstr = "independence", data = Coffee)
summary(fit)
fit0 <- nomLORgee(y ~ 1, id = person, LORstr = "independence", data = Coffee)
waldts(fit0, fit)

##############################################################
## Example:  Recycle or Drive Less to Help the Environment? ##
##############################################################
Envir <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Envir.dat", header = T)
head(Envir, 4)
library(multgee)
fit <- ordLORgee(y ~ question, id = person, LORstr = "independence", data = Envir)
summary(fit)
