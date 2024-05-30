#################
## Chapter 5.1 ##
#################
#############################################################
## Example: Whether a Female Horseshoe Crab Has Satellites ##
#############################################################
Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat",header = TRUE)
head(Crabs, 7) # showing 7 out of 173 observations
# y is satellite indicator, y = 1 when sat > 0
fit <- glm(y ~ weight + width + factor(color) + factor(spine), 
           family = binomial, data = Crabs) 
summary(fit)
1-pchisq(225.76-185.20,df=172-165)
cor(Crabs$weight, Crabs$width) # There exists multicollinearity


## Variable selection
fit <- glm(y ~ width + factor(color), family = binomial, data = Crabs) 
-2 * logLik(fit)
AIC(fit) # adds 2(number of parameters) = 10 to -2 * logLik(fit)
## Stepwise backward selection using AIC
fit <- glm(y ~ weight + width + factor(color) + factor(spine), 
           family = binomial, data = Crabs) 
library(MASS)
forwardAIC <- stepAIC(fit, direction="forward")
backwardAIC <- stepAIC(fit, direction="backward")
stepwiseAIC <- stepAIC(fit, direction="both")

? stepAIC # We can use BIC by setting 'k' to 'log(n)'.

backwardBIC <- stepAIC(fit,k=log(173)) # BIC is used instead of AIC

# Stepwise method by AIC considering up to all-way interaction model
stepAIC<-stepAIC(fit,scope=list(upper= ~weight*width*factor(color)*factor(spine),lower=~1))


# Stepwise method by BIC considering up to all-way interaction model
stepBIC<-stepAIC(fit,scope=list(upper= ~weight*width*factor(color)*factor(spine),lower=~1),k=log(173))



#################
## Chapter 5.2 ##
#################
#######################################################
## Example: Goodness of Fit for Marijuana Use Survey ##
#######################################################
Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat",
                        header = TRUE)
Marijuana
fit <- glm(yes/(yes+no) ~ gender + race, weights = yes + no, 
           family = binomial, data = Marijuana)
fit$deviance
fit$df.residual  # df for the fitted model
fit$df.null  # df for the null model
1-pchisq(fit$deviance, fit$df.residual)
fitted(fit)
n <- apply(cbind(Marijuana$yes, Marijuana$no), 1, sum)
fit.yes <- n * fitted(fit); fit.no <- n * (1 - fitted(fit))
attach(Marijuana)
data.frame(race, gender, yes, fit.yes, no, fit.no)
cbind(rstandard(fit, type = "pearson"), residuals(fit, type = "pearson"),
      residuals(fit, type = "deviance"), rstandard(fit, type = "deviance"))

##################
## Exercise 5.1 ##
##################
Crabs = read.table("http://users.stat.ufl.edu/~aa/cat/data/Crabs.dat",header=T) 
head(Crabs)
fit <- glm(y ~ weight + width, family = binomial, data = Crabs) 
summary(fit)
library(car)
Anova(fit)
library(MASS)
backwardAIC <- stepAIC(fit) 
##################
## Exercise 5.3 ##
##################
Crabs2 = read.table("http://users.stat.ufl.edu/~aa/cat/data/Crabs2.dat",header=T) 
head(Crabs2)
