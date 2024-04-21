###############
## Chapter 3 ##
###############
########################################
## Example: Snoring and Heart Disease              ##
########################################
Heart <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Heart.dat", header = T)
Heart
library(dplyr)
Heart$x <- recode(Heart$snoring, never=0, occasional=2,nearly_every_night=4, every_night=5)
Heart
n <- Heart$yes + Heart$no
n
fit <- glm(yes/n ~ x, family = binomial(link = logit), weights = n, data = Heart) # nolint
summary(fit)

# fitting a linear model
fit.linear<- lm(yes/n ~ x, weights = n, data = Heart)
summary(fit.linear)

#########################################################
## Example: Female Horseshoe Crabs and their Satellites                     ##
#########################################################
# More on horseshoe crabs can be found below
# https://www.nhm.ac.uk/discover/horseshoe-crab-blood-miracle-vaccine-ingredient.html

Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = T)
head(Crabs)
dim(Crabs)
plot(sat ~ width, xlab="Width", ylab="Number of satellites", data=Crabs)
library(gam) # generalized additive model smoothing fit
gam.fit <- gam(sat ~ s(width), family=poisson, data=Crabs)
curve(predict(gam.fit, data.frame(width=x), type="resp"), add=T,lwd=2,col=4)
fit.pois <- glm(sat ~ width, family = poisson(link = log), data = Crabs)
summary(fit.pois)
curve(predict(fit.pois, data.frame(width=x), type="resp"), add=T,col=2,lwd=2)



########################################################
## Example: Political Ideology and Belief in Evolution                       ##
########################################################
Evo <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Evolution.dat", header = T)

n <- Evo$true + Evo$false
fit <- glm(true/n ~ ideology, family = binomial, weights = n, data = Evo)
summary(fit)
confint(fit) # profile likelihood CI
library(car)
Anova(fit) # likelihood-ratio tests for effect parameters in a GLM

library(statmod)
fit0 <- glm(true/n ~ 1, family=binomial, weights=n, data=Evo) # null model
glm.scoretest(fit0, Evo$ideology)? # squaring a z score statistic
#104.101 # score chi-squared statistic with df=1

#################
# Exercise 3.5         #
#################
Crabs2 <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = T)

# fit a logistic regression
EX3.5a = glm(y~weight, family=binomial(link="logit"), data=Crabs2)
summary(EX3.5a)

# Error with binomial family with identity link
EX3.5b = glm(y~weight, family=binomial(link="identity"), data=Crabs2)

# Fit a linear model with Gaussian link.
EX3.5c = glm(y~weight, family=gaussian, data=Crabs2)
summary(EX3.5c)
predict(EX3.5c, newdata=data.frame(weight = 5.2)) #fitted value, or fitted probability is greater than 1, which is a problem. 
1 
1.533186 

predict(EX3.5a, newdata=data.frame(weight = 5.2))
1 
5.744025   # This is a linear predictor, or systematic component.

predict(EX3.5a, newdata=data.frame(weight = 5.2), type="response")
1 
0.9968084  # This is the fitted probability. 


#################
# Exercise 3.6         #
#################
y <- c(5,18,19,25,7,7,2); n <- c(6,21,20,36,17,18,3)
x <- c(1,2,3,4,5,6,7)
model3.6<- glm(y/n~x,family=binomial, weights=n)

summary(model3.6)


#################
# Exercise 3.9   #
#################
Credit = read.table("credit.dat",header=T)
model3.9 = glm(cards/n ~ income, family = binomial, data = Credit, weights = n)
summary(model3.9)


#################
# Exercise 3.11 #
#################

Y = c(8,7,6,6,3,4,7,2,3,4,9,9,8,14,8,13,11,5,7,6)
X = factor(c(rep("A",10),rep("B",10)))
model3.11 = glm(Y~X,family=poisson)
summary(model3.11)



## Prediction equation is
# log(mu.hat) = 1.6094 + 0.5878X

### Interpretation 1 
# Log of the estimated average imperfections for treatment B is higher than the log of estimated average imperfections for 
# treatment A by 0.5878

### Interpretation 2
# The estimated average imperfections for treatment B is  e^(0.588)=1.800 times higher than that for treatment A.



#################
# Exercise 3.13 #
#################
Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = T)
model3.13 = glm(sat ~ weight, family=poisson, data=Crabs)
summary(model3.13)


#q1
exp(-0.42841+0.58930*2.44) 
2.744179

exp(predict(model3.13,data.frame(weight=2.44)))  # more accurate answer
1 
2.74422 

#Q3) LRT: 
1-pchisq(632.79 - 560.87, df=172-171)
0
#Alternative way to conduct LRT. (More accurate)
library(car)
Anova(model3.13)
# "weight" is very significant variable for explaning the number of satellites.