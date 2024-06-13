#Question1
486/1500-qnorm(0.995)*sqrt(486/1500*(1-486/1500)/1500)
#0.2928744
486/1500+qnorm(0.995)*sqrt(486/1500*(1-486/1500)/1500)
#0.3551256

#Question2
z.test = (486/1500-0.5)/sqrt(0.5*(1-0.5)/1500)
z.test #-13.6329

#to find p value
2*pnorm(z.test)
#2.552278e-42
#because p-value is close to 0, we reject the null hypothesis.

#Question3
yes=570/555453
no=433/8482
(yes-no) - qnorm(0.975)*sqrt(yes*(1-yes)/555453+no*(1-no)/8482)
#-0.05470783
(yes-no) + qnorm(0.975)*sqrt(yes*(1-yes)/555453+no*(1-no)/8482)
#-0.04533835

#Question4
pi.c = (570+433)/(555453+8482) # common success probability.
z.test2=(yes-no)/sqrt(pi.c*(1-pi.c)*(1/555453+1/8482))
z.test2 # test statistic
2*pnorm(z.test2) # p-value

#Question5
chisq.test(matrix(c(433,570,8049,554883),nrow=2),correct=F)
#Because p-value is small, we reject the null hypothesis and conclude that the two variables are related

#Question6
RR = yes/no = (570/555453)/(433/8482)
RR

#Question7
#LRT Statistic
2*(802*log(802/516.833)+53*log(53/338.167)+34*log(34/319.167)+494*log(494/208.833))
#1206.728
pvalue = 1-pchisq(1206.728,df=1)
#p-value is 0. So reject null hypothesis. The two variables are not independent.

#Question8
odds = 802*494/(34*53)
log(odds)-qnorm(0.975)*sqrt(1/802+1/53+1/34+1/494) #4.947985
log(odds)+qnorm(0.975)*sqrt(1/802+1/53+1/34+1/494) #5.837998