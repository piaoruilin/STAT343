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
library(readr)
library(dplyr)
deathpenalty <- read_table("/Users/piaoruilin/Desktop/DATASCIENCE/STAT343/DeathPenalty.dat")

model <- glm(count ~ D * V * P, family = poisson, data = deathpenalty)
summary(model)
coefficients <- summary(model)$coefficients

expanded_data <- deathpenalty %>%
  mutate(
    P = ifelse(P == "yes", 1, 0)
  )

logistic_model <- glm(P ~ D * V, family = binomial(link = "logit"), data = expanded_data)
summary(logistic_model)

coefficients <- summary(logistic_model)$coefficients

odds_ratio_D_white <- exp(coefficients["Dwhite", "Estimate"])
odds_ratio_D_white

interaction_term <- "Dwhite:Vwhite"
odds_ratio_interaction <- exp(coefficients[interaction_term, "Estimate"])
odds_ratio_interaction

cat("Odds Ratio for Dwhite:", odds_ratio_D_white, "\n")
cat("Conditional Odds Ratio for", interaction_term, ":", odds_ratio_interaction, "\n")

# Write the fitted equation for the logistic regression model
intercept <- coefficients["(Intercept)", "Estimate"]
D_white <- coefficients["Dwhite", "Estimate"]
V_white <- coefficients["Vwhite", "Estimate"]
interaction <- coefficients[interaction_term, "Estimate"]

cat("Fitted logistic regression equation:\n")
cat("logit(P(yes)) =", intercept, "+", D_white, "* D_white +", V_white, "* V_white +", interaction, "* D_white * V_white\n") # nolint

#Question3
accident_data <- data.frame(
  S = c("Seat belt", "Seat belt", "None", "None"),
  E = c("Yes", "No", "Yes", "No"),
  K = c("Nonfatal", "Nonfatal", "Nonfatal", "Nonfatal", "Fatal", "Fatal", "Fatal", "Fatal"),
  count = c(1105, 411111, 4624, 157342, 14, 483, 497, 1008)
)
print(accident_data)

accident_data <- accident_data %>%
  mutate(
    S = factor(S, levels = c("Seat belt", "None")),
    E = factor(E, levels = c("Yes", "No")),
    K = factor(K, levels = c("Nonfatal", "Fatal"))
  )

model1 <- glm(count ~ S * E * K, family = poisson, data = accident_data)
model2 <- glm(count ~ S * E + S * K + E * K, 
              family = poisson, data = accident_data)
model3 <- glm(count ~ S * E + S * K, family = poisson, data = accident_data)
model4 <- glm(count ~ S * E + E * K, family = poisson, data = accident_data)

aic_values <- data.frame(
  Model = c("S * E * K", "S * E + S * K + E * K", "S * E + S * K", 
            "S * E + E * K"),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4))
)
print(aic_values) #lowest value = best model

best_model <- model1
summary(best_model)

best_model <- glm(count ~ S * E * K, family = poisson, 
                  data = accident_data)
observed <- accident_data$count
expected <- fitted(best_model)
dissimilarity_index <- 0.5 * sum(abs((observed - expected) / observed))
dissimilarity_index

#d
coefficients <- summary(model1)$coefficients
lambda <- coefficients["(Intercept)", "Estimate"]
lambda_S_None <- coefficients["SNone", "Estimate"]
lambda_E_Yes <- coefficients["EYes", "Estimate"]
lambda_K_Fatal <- coefficients["KFatal", "Estimate"]
lambda_SE_None_Yes <- coefficients["SNone:EYes", "Estimate"]
lambda_SK_None_Fatal <- coefficients["SNone:KFatal", "Estimate"]
lambda_EK_Yes_Fatal <- coefficients["EYes:KFatal", "Estimate"]
lambda_SEK_None_Yes_Fatal <- coefficients["SNone:EYes:KFatal", "Estimate"]

# Calculate log of expected count for "Seat belt", "Yes", "Fatal"
# Note: Seat belt = "None" means it is NOT seat belt
log_mu_Seatbelt_Yes_Fatal <- lambda + 0 + lambda_E_Yes + lambda_K_Fatal + 0 + 0 + lambda_EK_Yes_Fatal + 0  # Only include terms relevant to "Seat belt" = Yes, which means "None" = 0

# Exponentiate to get the expected count
mu_Seatbelt_Yes_Fatal <- exp(log_mu_Seatbelt_Yes_Fatal)

#Question4