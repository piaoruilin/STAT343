
# Slide 20

pi.hat=400/893
z.test = (pi.hat-0.5)/sqrt(0.5*(1-0.5)/893)
z.test
#-3.112126
pnorm(z.test)
#0.0009287249
2*pnorm(z.test) # P-value since two-sided testing
#0.00185745
2*(1-pnorm(-z.test)) # P-value is small. Reject the null hypothesis
#0.00185745

# We form a 95% approximate confidence interval
pi.hat-qnorm(0.975)*sqrt(pi.hat*(1-pi.hat)/893)
0.4153128  # lower bound
pi.hat+qnorm(0.975)*sqrt(pi.hat*(1-pi.hat)/893)
0.4805439  # upper bound
# Because 0.5 is not included in the interval,
# 0.5 is not a likely value under alpha=0.05.

# Slide 29
# Wald test
Wald.stat = (0.9-0.5)/sqrt(0.9*0.1/10)
2*(1-pnorm(Wald.stat))

# Score test
Score.stat = (0.9-0.5)/sqrt(0.5*0.5/10)
2*(1-pnorm(Score.stat))
               
# Likelihood Ratio Test 
LRT.stat = 2*(9*log(9/5)+1*log(1/5))
1-pchisq(LRT.stat,df=1)