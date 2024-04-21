###############
## Chapter 2 ##
###############
# Slide 15
prop.test(c(189, 104), c(11034, 11037), conf.level = 0.95, correct=FALSE)
# We obtain X-squared = 25.014 from the above
# Let's obtain X-squared value using the formula in slide 13 
p1=189/11034; p2=104/11037 # sample proportions
pi.c = (189+104)/(11034+11037) #common success probability under H0:pi1=pi2
std.err = sqrt(pi.c*(1-pi.c)*(1/11034+1/11037))  # SE in Slide 13
z.stat= (p1-p2)/std.err # z test statistic 
z.stat^2 # X-squared value

# Another equivalent test is Pearson's Chi-squared test. (Details will come later.)
Table2.3 = matrix(c(189,104,10845,10933),nrow=2,ncol=2)
chisq.test(Table2.3,correct=F) # Without continuity correction

library(PropCIs)
diffscoreci(189, 11034, 104, 11037, conf.level = 0.95)
riskscoreci(189, 11034, 104, 11037, conf.level=0.95)

# Slide 24
log.odds=log(189*10933/(104*10845))
SE.odds=sqrt(1/189+1/10845+1/104+1/10933)
log.LB = log.odds-qnorm(0.975)*SE.odds
log.UB = log.odds+qnorm(0.975)*SE.odds
c(exp(log.LB),exp(log.UB))
1.440042 
2.330780 # 95% Wald CI for odds ratio

# Slide 37
## Example: Gender Gap in Political Party Affiliation
Political <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Political.dat", header = T)
head(Political)
dim(Political)
Party <- factor(Political$party, level=c("Dem", "Rep", "Ind"))
Party
GenderGap  <- xtabs(~ gender + party, data = Political)
GenderGap
## Chi-square test
chisq.test(GenderGap)
chisq.test(GenderGap)$stdres
GenderGap
GenderGap.2 <- matrix(c(495, 272, 590, 330, 265, 498), ncol=3, byrow=T)
GenderGap.2
Chi.test = chisq.test(GenderGap)
ls(Chi.test) # Show all the outputs
Chi.test$expected
Chi.test$observed

## Likelihood Ratio test
LRT=2*sum(Chi.test$observed*log(Chi.test$observed/Chi.test$expected))
1-pchisq(LRT,df=Chi.test$parameter)
                                    

# Slide 48
##################################################
## Example: Alcohol Use and Infant Malformation ##
##################################################
Malform <- matrix(c(17066, 14464, 788, 126, 37, 48, 38, 5, 1, 1), ncol = 2)
library(vcdExtra)
CMHtest(Malform, rscores = c(0, 0.5, 1.5, 4.0, 7.0))
sqrt(6.5699)       # M test statistic
1 - pnorm(2.56318) # one-sided standard normal p-values for M statistic
CMHtest(Malform, rscores = c(2, 2.5, 3.5, 6.0, 9.0)) # different scores but same interval
# different interval changes the results 
# limitation of CMH test since the scores are subjective.
CMHtest(Malform, rscores = c(0, 1, 2, 3, 4)) 

#############################################
## Example: Fisher's Tea Tasting Colleague ##
#############################################
tea <- matrix(c(3, 1, 1, 3), ncol = 2)
tea
fisher.test(tea, alternative = "greater")
fisher.test(tea)
# Same success probability with increased experiments 
fisher.test(matrix(c(9,3,3,9), ncol=2), alternative="greater")
