  ###############
## Chapter 7 ##
###############
#############################################
## Example: Happiness and Belief in Heaven ##
#############################################
HappyHeaven <- read.table("http://www.stat.ufl.edu/~aa/cat/data/HappyHeaven.dat", 
                          header=TRUE)
HappyHeaven
fit <- glm(count ~ happy + heaven, family = poisson, data = HappyHeaven)
summary(fit)
# saturated loglinear model
fit2 <- glm(count ~ happy + heaven + happy:heaven, family = poisson, data = HappyHeaven)
summary(fit2) 
############################################################
## Example: Student Alcohol, Cigarette, and Marijuana Use ##
############################################################
Drugs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Substance.dat", header=TRUE)
Drugs
A <- Drugs$alcohol 
C <- Drugs$cigarettes 
M <- Drugs$marijuana

# Mutually independent model
fit.MI <- glm(count ~ A + C + M, family = poisson, data = Drugs)
summary(fit.MI)

# Jointly independent model
fit.JI <- glm(count ~ A*C + M, family = poisson, data = Drugs)
summary(fit.JI)

# Conditionally independent model
fit.CI <- glm(count ~ A*M + C*M, family = poisson, data = Drugs)
summary(fit.CI)


# Homogeneous association model
fit.HA <- glm(count ~ A + C + M + A*C + A*M + C*M, family = poisson, data = Drugs)
summary(fit.HA)

fit.HA2 <- glm(count ~ A*C + A*M + C*M, family = poisson, data = Drugs)
summary(fit.HA2) #  Having higher order terms allows lower order terms to be included

# Saturated model
fit.S <- glm(count ~ A*C*M, family = poisson, data = Drugs)
summary(fit.S)

> summary(fit.S)

Call:
glm(formula = count ~ A * C * M, family = poisson, data = Drugs)

Deviance Residuals: 
[1]  0  0  0  0  0  0  0  0

Coefficients:
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)     5.63121    0.05987  94.060  < 2e-16 ***
Ayes            0.49128    0.07601   6.464 1.02e-10 ***
Cyes           -1.87001    0.16383 -11.414  < 2e-16 ***
Myes           -4.93806    0.70964  -6.959 3.44e-12 ***
Ayes:Cyes       2.03538    0.17576  11.580  < 2e-16 ***
Ayes:Myes       2.59976    0.72698   3.576 0.000349 ***
Cyes:Myes       2.27548    0.92746   2.453 0.014149 *  
Ayes:Cyes:Myes  0.58951    0.94236   0.626 0.531600    
---
Signif. codes:  0   ***   0.001   **   0.01   *   0.05   .   0.1       1

(Dispersion parameter for poisson family taken to be 1)

# Estimated odds ratios for (ACM)level 1 in Table 7.5. in Slide page 21
> exp(2.03538+0.58951)
[1] 13.80306
> exp(2.59976+0.58951)
[1] 24.2707
> exp(2.27548+0.58951)
[1] 17.54888

# Estimated odds ratios for (ACM)level 2 in Table 7.5. (level 2 is default) 
> exp(2.03538)
[1] 7.655161
> exp(2.59976)
[1] 13.46051
> exp(2.27548)
[1] 9.73259


#################
## Chapter 7.2 ##
#################


res <- rstandard(fit.HA, type="pearson")
res2 <- rstandard(fit.CI, type="pearson")
data.frame(A, C, M, Drugs$count, fitted(fit), res, fitted(fit2), res2)
deviance(fit.HA)
deviance(fit.CI)
library(car)
Anova(fit.HA)

# profile likelihood CI for conditional OR
exp(confint(fit.HA))

##################################################
## Example: Automobile Accidents and Seat Belts ##
##################################################
Accidents <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Accidents2.dat", 
                        header=T)
Accidents
G <- Accidents$gender; L <- Accidents$location
S <- Accidents$seatbelt; I <- Accidents$injury
fit.HA = glm(count~G*S+G*L+G*I+L*S+L*I+S*I,family = poisson, data = Accidents)
summary(fit.HA)
fit <- glm(count ~ G*L*S + G*I + L*I + S*I, family = poisson, data = Accidents)
summary(fit)
fitted(fit)
tmp <- data.frame(Accidents, fitted(fit))
tmp
 # Slide 36 
> exp(-0.28274) # Estimated odds ratio for GL (S=no)
[1] 0.7537157
> exp(-0.28274+0.12858) # Estimated odds ratio for GL (S=yes)
[1] 0.8571349
> exp(-0.54186+0.12858) # Estimated odds ratio for GS (L=urban)
[1] 0.661477
> exp(-0.54186) # Estimated odds ratio for GS (L=rural)
[1] 0.5816653


## The fitted GS odds ratios (when L="urban")
tmp[L=="urban" & I=="yes", c(1,3,6)]
fitted(fit)[16]*fitted(fit)[6] / (fitted(fit)[8]*fitted(fit)[14])
tmp[L=="urban" & I=="no", c(1,3,6)]
fitted(fit)[15]*fitted(fit)[5] / (fitted(fit)[7]*fitted(fit)[13])
## The fitted GL odds ratios (when S="yes")
tmp[S=="yes" & I=="yes", c(1,2,6)]
fitted(fit)[16]*fitted(fit)[4] / (fitted(fit)[8]*fitted(fit)[12])
tmp[S=="yes" & I=="no", c(1,2,6)]
fitted(fit)[3]*fitted(fit)[15] / (fitted(fit)[7]*fitted(fit)[11])
## The fitted GL odds ratios (when S="no")
tmp[S=="no" & I=="yes", c(1,2,6)]
fitted(fit)[14]*fitted(fit)[2] / (fitted(fit)[6]*fitted(fit)[10])
tmp[S=="no" & I=="no", c(1,2,6)]
fitted(fit)[13]*fitted(fit)[1] / (fitted(fit)[5]*fitted(fit)[9])

# dissimilarity index for loglinear model (GLS, GI, LI, SI)
sum(abs(Accidents$count - fitted(fit))) / (2*sum(Accidents$count))
fit2 <- glm(count ~ G*L + G*S + L*S + G*I + L*I + S*I, family = poisson, 
            data = Accidents)
# dissimilarity index for loglinear model (GL, GS, LS, GI, LI, SI)
sum(abs(Accidents$count - fitted(fit2))) / (2*sum(Accidents$count))

###########################################
## Example: Auto Accident Data Revisited ##
###########################################
Injury <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Injury_binom.dat", 
                     header = T)
Injury
G <- Injury$gender
L <- Injury$location
S <- Injury$seatbelt
fit2 <- glm(yes/(no+yes) ~ G + L + S, family = binomial, weights = no+yes,
            data = Injury)
summary(fit2)

##################
## Exercise 7.4 ##
##################
MBTI <- read.table("http://www.stat.ufl.edu/~aa/intro-cda/data/MBTI.dat", header = T)
MBTI
fit = glm(n ~ EI + SN + TF + JP + EI:JP + SN:TF + TF:JP + EI:SN + EI:TF + SN:JP, 
          family = poisson, data = MBTI)
summary(fit)
##################
## Exercise 7.5 ##
##################
Accidents <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Accidents2.dat", 
                        header=T)
Accidents
tmp <- matrix(c(sum(Accidents$count[1:2]), sum(Accidents$count[3:4]), 
                sum(Accidents$count[5:6]), sum(Accidents$count[7:8]), 
                sum(Accidents$count[9:10]), sum(Accidents$count[11:12]), 
                sum(Accidents$count[13:14]), sum(Accidents$count[15:16])),
                4, 2, byrow=T)
tmp
no <- tmp[,1]; yes <- tmp[,2]
Seat <- cbind(Accidents[c(1,5,9,13), c(1,2)], no, yes)
Seat
G <- Seat$gender; L <- Seat$location
fit <- glm(yes/(no+yes) ~ G + L, family = binomial, weights = no+yes,
            data = Seat)
summary(fit)
Injury <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Injury_binom.dat", 
                     header = T)
Injury
G <- Injury$gender
L <- Injury$location
S <- Injury$seatbelt
fit2 <- glm(yes/(no+yes) ~ G + L + S, family = binomial, weights = no+yes,
            data = Injury)
summary(fit2)
