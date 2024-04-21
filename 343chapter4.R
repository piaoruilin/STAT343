#####################
## Chapter 4.1     ##
#####################

Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
head(Crabs, 7) # showing 7 out of 173 observations
# y is satellite indicator, y = 1 when sat > 0
fit <- glm(y ~ width, family = binomial, data = Crabs)  # link=logit is default
summary(fit)
predict(fit, data.frame(width = 21.0), type = "response")
predict(fit, data.frame(width = mean(Crabs$width)), type = "response")
predict(fit, data.frame(width = 21.0), type ="link")

fit$coef

fit$coef[1]+fit$coef[2]*21.0

#####################
## Chapter 4.2     ##
#####################

library(car)
Anova(fit)    # Likelihood ratio test 
confint(fit) #profile likelihood CI

pred.prob <- fitted(fit) # ML fitted value estimate of P(Y=1)
lp <- predict(fit, se.fit = TRUE) # linear predictor

# Confidence bounds for linear predictor
LB <- lp$fit - qnorm(0.975) * lp$se.fit
UB <- lp$fit + qnorm(0.975) * lp$se.fit

# Confidence intervals for P(Y=1)
LB.p <- exp(LB) / (1+exp(LB)); UB.p <- exp(UB) / (1+exp(UB))  
pred.crabs <- cbind(Crabs$width, pred.prob, LB.p, UB.p)
head(pred.crabs) # fitted probabilities and confidence intervals

# Alternative way for CI for P(Y=1)
CI.for.prob = predict(fit, type="response",se.fit=TRUE)
             

plot(jitter(y,0.1) ~ width, xlim=c(18,34), pch=16, ylab="Prob(satellite)",data=Crabs)
data.plot <- data.frame(width=(18:34))
plot.prob <- predict(fit, newdata=data.plot, se.fit=TRUE,type="response")

curve(predict(fit, data.frame(width=x), type="response"), add=TRUE)
lines(18:34, plot.prob$fit-qnorm(0.975)*plot.prob$se.fit, col="red",lwd=2)
lines(18:34, plot.prob$fit+qnorm(0.975)*plot.prob$se.fit, col="blue",lwd=2)

#################
## Chapter 4.3 ##
#################

#########################################
## Example: Survey about Marijuana Use ##
#########################################
MJ <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat",header = TRUE)
MJ
fit <- glm(yes / (yes + no) ~ gender + race, weights = yes + no,
family = binomial, data = MJ)
summary(fit)
library(car)
Anova(fit)

#################
## Chapter 4.4 ##
#################

##############################################################
## Example: Horseshoe Crabs with Color and Width Predictors ##
##############################################################
summary(glm(y ~ width, family = binomial, data = Crabs))
library(car)
Anova(glm(y ~ width + factor(color), family = binomial, data = Crabs))

##############################################################
## Example: Horseshoe Crabs with Color and Width Predictors ##
##############################################################
fit <- glm(y ~ width + factor(color), family = binomial, data = Crabs)
summary(fit)
summary(glm(y ~ width, family = binomial, data = Crabs))
summary(fit)
Anova(fit)
#######################################################
## Example: Treating Color as Quantitative or Binary ##
#######################################################
fit2 <- glm(y ~ width + color, family=binomial, data=Crabs)
summary(fit2) # color treated as quantitative with scores (1, 2, 3, 4)
anova(fit2, fit, test = "LRT")
Crabs$c4 <- ifelse(Crabs$color == 4, 1, 0)
fit3 <- glm(y ~ width + c4, family = binomial, data = Crabs)
anova(fit3, fit, test="LRT")

## Allowing Interaction between Explanatory Variables
fit4<-glm(y ~ width + c4 + width:c4, family = binomial, data = Crabs)
summary(fit4)
anova(fit3,fit4, test="LRT")  # fit3 model is preferred


#################
## Chapter 4.6 ##
#################
              
# Classification Tables
prob <- sum(Crabs$y) / nrow(Crabs)
prob # In horseshoe crab data, a sample proportion is 0.6416.
fit <- glm(y ~ width + factor(color), family = binomial, data = Crabs)
predicted <- as.numeric(fitted(fit) > prob) # cutoff value is prob.
xtabs(~ Crabs$y + predicted)
predicted2 = round(fitted(fit)) # cutoff value is 0.5
xtabs(~ Crabs$y + predicted2)

# ROC Curves
library(pROC)
rocplot <- roc(y ~ fitted(fit), data = Crabs)
rocplot$thresholds # cufoff values for drawing ROC curve.
plot.roc(rocplot, legacy.axes = T) # Specificity on x axis if legacy.axes=F
plot.roc(rocplot) # This is not common thesedays

auc(rocplot) # auc = area under ROC curve = concordance index
# Multiple Correlation
cor(Crabs$y, fitted(fit))


##################
## Exercise 4.8 ##
##################

Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = T)
crab.glm <- summary(glm(y ~ weight, family = binomial, data = Crabs)); crab.glm
# ROC Curves
library(pROC)
rocplot <- roc(y ~ fitted(fit), data = Crabs)
plot.roc(rocplot, legacy.axes = T) # Specificity on x axis if legacy.axes=F
crab.glm <- summary(glm(y ~ weight, family = binomial, data = Crabs)); crab.glm
crab.glm <- glm(y ~ weight, family = binomial, data = Crabs)
summary(crab.glm)
library(mfx)
install.packages("mfx")
#install.packages("mfx")
library(mfx)
logitmfx(y~weight,data=crab,atmean = F)
logitmfx(y ~ weight,data = Crabs, atmean = F)
pihat <- predict(crab.glm, type="response")
yhat <- as.numeric(pihat > sum(crab$y==1)/nrow(crab)) confusion = table(crab$y, yhat) ;confusion
yhat <- as.numeric(pihat > sum(Crabs$y==1)/nrow(Crabs)) confusion = table(Crabs$y, yhat) ;confusion
pihat <- predict(crab.glm, type="response")
pihat
Crabs$y
nrow(Crabs)
yhat <- as.numeric(pihat > sum(Crabs$y==1)/nrow(Crabs))
confusion = table(Crabs$y, yhat) ;confusion
library(pROC)
crab.roc = roc(y~fitted(crab.glm), data = Crabs)
plot.roc(crab.roc, legacy.axes=T, col="red", print.auc=TRUE,
max.auc.polygon=TRUE, print.thres=TRUE, print.thres.pch=19,
print.thres.col = "red", auc.polygon=TRUE)
##################
## Exercise 4.9 ##
##################
crab.glm1 = glm(y ~ factor(color), family = binomial,
data = Crabs) #qualitative
summary(crab.glm1)
crab.glm2 = glm(y ~ color, family = binomial, data = Crabs)
summary(crab.glm2)
crab.glm3 = glm(y ~ I((weight-mean(weight))/sd(weight))+ I((color-mean(color))/sd(color)), family = binomial, data = Crabs)
summary(crab.glm3)
###################
## Exercise 4.14 ##
###################
infection < read.table("http://users.stat.ufl.edu/~aa/cat/data/infection.dat",header=T)

###################
## Exercise 4.16 ##
###################
sore <- read.table("http://www.stat.ufl.edu/~aa/cat/data/SoreThroat.dat",header = T)

sore
sore.glm1 <- glm(Y ~ D + T + D:T, family = binomial, data = sore)
summary(sore.glm1)
sore.glm2 <- glm(Y ~ D + T, family = binomial, data = sore)
summary(sore.glm2)
cor(sore$Y, fitted(sore.glm1))
cor(sore$Y, fitted(sore.glm2))
