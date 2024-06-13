#Question1
deviance <- 0.82
df <- 1
p_value <- 1 - pchisq(0.82, 1)

estimate <- 1.4165
std_error <- 0.0752
z_value <- 1.96
margin_of_error <- z_value * std_error
lower_bound <- estimate - margin_of_error
upper_bound <- estimate + margin_of_error
cat("95% Wald Confidence Interval for λ_yes: (", lower_bound, ", ", upper_bound, ")\n", sep = "")

#Question 2
