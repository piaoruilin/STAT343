#################
## Chapter 6.1 ##
#################
######################################
## Example: What Do Alligators Eat? ##
######################################
Gators <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat", 
                     header = TRUE)
head(Gators, 5)
library(VGAM) # package for multivariate GLMs, such as multinomial models
fit <- vglm(y ~ x, family = multinomial, data = Gators)
coef(fit, matrix = TRUE)

summary(fit) # Estimated Coef for x:2 is significant. 
dim(Gators)
# Note: sample size is 59, but each sample has two fitted probabilites. 
# This lead to 59*2=118 d.f. Since we have 4 estimates, residual deviance has 114 d.f. 

# now using y = 2 (I = Invertebrates) as baseline category
fit2 <- vglm(y ~ x, family = multinomial(refLevel = "I"), data = Gators)
coef(fit2, matrix = TRUE)
summary(fit2)

# Fitted probability for having a fish when an alligator is 2.0m and 2.5m long
# when 'Others' is a baseline category
> coef(fit, matrix = TRUE)
            log(mu[,1]/mu[,3]) log(mu[,2]/mu[,3])
(Intercept)           1.617731           5.697444
x                    -0.110109          -2.465446

> exp(1.617731-0.110109*2)/(1+exp(1.617731-0.110109*2)+exp(5.697444-2.465446*2))
[1] 0.5620215  # pi_1(x=2) --(a1)
> exp(1.617731-0.110109*2.5)/(1+exp(1.617731-0.110109*2.5)+exp(5.697444-2.465446*2.5))
[1] 0.7017144  # pi_1(x=2.5) --(a2)

# when 'Invertebrates' is a baseline category
>  coef(fit2, matrix=TRUE)
            log(mu[,1]/mu[,2]) log(mu[,3]/mu[,2])
(Intercept)          -4.079713          -5.697444
x                     2.355337           2.465446

> exp(-4.079713+2.355337*2)/(1+exp(-4.079713+2.355337*2)+exp(-5.697444+2.465446*2))
[1] 0.5620215 # pi_1(x=2) --(b1) 
>  exp(-4.079713+2.355337*2.5)/(1+exp(-4.079713+2.355337*2.5)+exp(-5.697444+2.465446*2.5))
[1] 0.7017144 # pi_1(x=2.5) --(b2)

# Now, we use predict function. 
> predict(fit2, data.frame(x=c(2,2.5)),type="response")
          F         I         O
1 0.5620216 0.2990405 0.1389379
2 0.7017144 0.1149959 0.1832896
# From the model 'fit' pi_3(x=2) (having 'Others') is calculated by
> 1/(1+exp(1.617731-0.110109*2)+exp(5.697444-2.465446*2))
[1] 0.1389379
# From the model 'fit2' the prob. having 'Others' when aligator is 2m is calculated by
> exp(-5.697444+2.465446*2)/(1+exp(-4.079713+2.355337*2)+exp(-5.697444+2.465446*2))
[1] 0.1389379
# From the model 'fit2' the prob. having 'Invertebrates' when aligator is 2m is calculated by
> 1/(1+exp(-4.079713+2.355337*2)+exp(-5.697444+2.465446*2))
[1] 0.2990405
  
# null model
fit0 <- vglm(y ~ 1, family = multinomial, data = Gators) 
deviance(fit0)
deviance(fit) 
# lrtest function available in VGAM package for LR tests
lrtest(fit, fit0) 
# the first 10 estimated response probabilities for outcome categories
head(fitted(fit), 10) 
##################################
## Example: Belief in Afterlife ##
##################################
Afterlife <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Afterlife.dat", header = TRUE)
Afterlife
library(VGAM)
fit <- vglm(cbind(yes,undecided,no) ~ gender + race, family = multinomial, 
            data = Afterlife)
summary(fit)
# removing gender from the model
fit.race <- vglm(cbind(yes,undecided,no) ~ race, family = multinomial, 
                 data = Afterlife) 
deviance(fit.race)
# lrtest function available in VGAM package
lrtest(fit, fit.race) 
data.frame(Afterlife$race, Afterlife$gender, fitted(fit))
# removing race from the model
fit.gender <- vglm(cbind(yes,undecided,no) ~ gender, family = multinomial, 
                   data = Afterlife) 
deviance(fit.gender)
# lrtest function available in VGAM package
lrtest(fit, fit.gender) 

data.frame(Afterlife$race, Afterlife$gender, fitted(fit))

# Residual deviance can be calculated using observed and expected values. 
expected = fitted(fit)*apply(Afterlife[,c(3:5)],1,sum)
observed = Afterlife[,c(3:5)]
> 2*sum(observed*log(observed/expected))
[1] 0.8538556


###############
## Chapter 6.2   ##
###############
#################################################################
## Example: Political Ideology and Political Party Affiliation ##
#################################################################
Polviews <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Polviews.dat", 
                       header = TRUE)
Polviews
library(VGAM)
# "parallel=TRUE" imposes proportional odds structure
fit <- vglm(cbind(y1,y2,y3,y4,y5) ~ party + gender, 
            family = cumulative(parallel=TRUE), data = Polviews)
summary(fit)
deviance(fit)
attach(Polviews)
# y1 = very lib., y5 = very conserv.
data.frame(gender, party, fitted(fit)) 
fit2 <- vglm(cbind(y1,y2,y3,y4,y5) ~ gender, 
             family = cumulative(parallel=TRUE), 
             data = Polviews)
deviance(fit2)
deviance(fit2)-deviance(fit)
lrtest(fit, fit2)
2*(logLik(fit) - logLik(fit2))

###############
## Exercise 6.3   ##
###############
Gators2 <- read.table("http://users.stat.ufl.edu/~aa/cat/data/Alligators2.dat", header=T)
# size = 1: Length <= 2.3 meters, size = 0: Length > 2.3 meters
# lake = 1 = Hancock, lake = 2 = Oklawaha
# lake = 3 = Trafford, lake = 4 = George
# y1 = Fish, y2 = Invertebrate, y3 = Reptile, y4 = Bird, y5 = Other
Gators2
library(VGAM)
fit <- vglm(cbind(y1, y2, y3, y4, y5) ~ as.factor(lake) + size, 
            family = multinomial(refLevel = "y1"), data = Gators2)
summary(fit)

Gators2$lake <- as.factor(rep(c("Hancock","Oklawaha","Trafford","George"),rep(2,4)))
Gators2
fit <- vglm(cbind(y1, y2, y3, y4, y5) ~ lake + size, 
            family = multinomial(refLevel = "y1"), data = Gators2)
summary(fit)

##################
## Exercise 6.8 ##
##################
trt <- c("sequential", "sequential", "alternating", "alternating") 
gender <- c("Male", "Female", "Male", "Female") 
cat1<- c(28, 4, 41, 12); cat2 <- c(45, 12, 44, 7)
cat3 <- c(29, 5, 20, 3); cat4 <- c(26, 2, 20, 1)
cancer <- data.frame(trt = trt, gender = gender, ProgDisease = cat1,
                     NoChange = cat2, ParRemission = cat3,
                     CompRemission = cat4)
cancer
fit <- vglm(cbind(ProgDisease, NoChange, ParRemission, CompRemission) ~ 
                trt + gender, family = cumulative(parallel=T), data = cancer)
summary(fit)    # cumulative(parallel=FALSE) is the default.
fit2 <- vglm(cbind(ProgDisease, NoChange, ParRemission, CompRemission) ~ 
                 trt + gender + trt:gender, family = cumulative(parallel=T),
             data = cancer)
summary(fit2)
lrtest(fit2, fit)

fitted(fit2)

exp(0.07702)/(1+exp(0.07702))  # Fitted P(Y=1) for alternating & female
