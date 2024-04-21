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
