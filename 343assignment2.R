#####################
## Question 1      ##
#####################

Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)

#fit linear regression model with identity
model <- glm(sat ~ weight, data = Crabs, family = gaussian(link = "identity"))
summary(model)

#estimated coefficients
coefficients <- coef(model)
intercept <- coefficients[1]
slope <- coefficients[2]

#estimated regression equation
regression_equation <- paste("satellites =", round(intercept, 2), "+", round(slope, 2), "*(weight)")
cat("regression equation:", regression_equation, "\n")

#fitted probability
weight_new <- 5.20
fitted_value <- predict(model, newdata = data.frame(weight = weight_new), type = "response")
cat("fitted probability:", fitted_value, "\n")

#####################
## Question 2      ##
#####################

#logistic regression model
logistic_model <- glm(y ~ weight, data = Crabs, family = binomial(link = "logit"))
summary(logistic_model)

#estimated coefficients
coefficients_logistic <- coef(logistic_model)
intercept_logistic <- coefficients_logistic[1]
slope_logistic <- coefficients_logistic[2]

#estimated regression equation
regression_equation_logistic <- paste("log(p / (1 - p)) =", round(intercept_logistic, 2), "+", round(slope_logistic, 2), "*(weight)")
cat("estimated regression equation:", regression_equation_logistic, "\n")

#fitted probability
weight_new <- 5.20
log_odds <- intercept_logistic + slope_logistic * weight_new
probability <- exp(log_odds) / (1 + exp(log_odds))
cat("fitted probability:", probability, "\n")

#####################
## Question 3      ##
#####################

y <- c(5, 18, 19, 25, 7, 7, 2); n <- c(6, 21, 20, 36, 17, 18, 3)
x <- c(1, 2, 3, 4, 5, 6, 7)
fit <- glm(y/n ~ x, family = binomial, weights = n)

#estimated coefficients
coefficients_fit <- coef(fit)
intercept_fit <- coefficients_fit[1]
slope_fit <- coefficients_fit[2]

#estimated regression equation
regression_equation_fit <- paste("log(p / (1 - p)) =", round(intercept_fit, 2), "+", round(slope_fit, 2), "* x")
cat("estimated regression equation:", regression_equation_fit, "\n")

#direction of the estimated effect
if (slope_fit > 0) {
  cat("Direction is positive.\n")
} else if (slope_fit < 0) {
  cat("Direction is negative.\n")
} else {
  cat("There is no direction.\n")
}

#Question 4
summary_fit <- summary(fit)

coefficient_x <- coef(fit)["x"]
se_x <- summary_fit$coefficients["x", "Std. Error"]

#Wald confidence interval
z_value <- qnorm(0.975)  # 1.96 for a 95% confidence interval
lower_bound <- coefficient_x - z_value * se_x
upper_bound <- coefficient_x + z_value * se_x

cat("95% Wald confidence interval: (", round(lower_bound, 3), ", ", round(upper_bound, 3), ")\n")

#Question 5
wald_statistic <- (coefficient_x / se_x)^2

p_value <- 1 - pchisq(wald_statistic, df = 1)

cat("Wald test statistic:", round(wald_statistic, 3), "\n")
cat("p-value:", p_value, "\n")
if (p_value < 0.05) {
  cat("Reject the null hypothesis. There is a significant effect of x.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant effect of x.\n")
}

#Question 6
fit_reduced <- glm(y/n ~ 1, family = binomial, weights = n)

log_likelihood_full <- logLik(fit)
log_likelihood_reduced <- logLik(fit_reduced)

LR_statistic <- 2 * (log_likelihood_full - log_likelihood_reduced)
p_value_LR <- 1 - pchisq(LR_statistic, df = 1)

cat("Likelihood-ratio test statistic:", round(LR_statistic, 3), "\n")
cat("p-value:", p_value_LR, "\n")
if (p_value_LR < 0.05) {
  cat("Reject the null hypothesis. There is a significant effect of x.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant effect of x.\n")
}

#Question 7~9
LI <- c(8,8,10,10,12,12,12,14,14,14,16,16,16,18,20,20,20,22,22,24,26,28,32,34,38,38,38)
y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,1,0,1,1,1,0)
summary(glm(y ~ LI, family=binomial))
confint(glm(y ~ LI, family=binomial))

fit <- glm(y ~ LI, family = binomial)

LI_values <- seq(min(LI), max(LI), by = 0.1)
predicted_probabilities <- predict(fit, newdata = data.frame(LI = LI_values), type = "response")
LI_ans <- LI_values[which.min(abs(predicted_probabilities - 0.5))]

coef_LI <- coef(fit)["LI"]
odds_ratio <- exp(coef_LI)
cat("estimated odds ratio:", round(odds_ratio, 2), "\n")

effect_ratio <- odds_ratio * (1 + 1)
round(effect_ratio, 2)

LI_value <- 8
z <- coef(fit)["(Intercept)"] + coef(fit)["LI"] * LI_value
rate_of_change <- coef(fit)["LI"] * exp(z) / (1 + exp(z))^2
round(rate_of_change, 3)

