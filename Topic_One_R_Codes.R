# =============================================================================
# BAYESIAN DATA ANALYSIS
# Topic One: Review of Bayes' Theorem and Introduction to Bayesian Approach
# Dr. Margaret Wambui Kinyua
# Department of Mathematics, Statistics & Actuarial Science
# Karatina University
# =============================================================================


# =============================================================================
# EXAMPLE 1: STOCK PRICE ANALYSIS
# =============================================================================
# Problem: Prior probability that stock price rises (theta) is equally likely
# to be 0.4 or 0.6. After observing 5 consecutive price increases, what is
# the posterior probability that theta = 0.6?
# -----------------------------------------------------------------------------

# Prior probabilities
p_theta_06 <- 0.5          # P(theta = 0.6)
p_theta_04 <- 0.5          # P(theta = 0.4)

# Likelihoods: P(A | theta) = theta^5
# probability of 5 consecutive price increases given each theta
p_A_given_06 <- 0.6^5     # P(A | theta = 0.6)
p_A_given_04 <- 0.4^5     # P(A | theta = 0.4)

# Posterior probability using Bayes' theorem
# P(theta = 0.6 | A) = P(A | theta=0.6) * P(theta=0.6) /
#                      [P(A|theta=0.6)*P(theta=0.6) + P(A|theta=0.4)*P(theta=0.4)]
posterior_06 <- (p_A_given_06 * p_theta_06) /
  (p_A_given_06 * p_theta_06 + p_A_given_04 * p_theta_04)

posterior_04 <- 1 - posterior_06

cat("=== Example 1: Stock Price Analysis ===\n")
cat("Prior P(theta = 0.6)     :", p_theta_06, "\n")
cat("Prior P(theta = 0.4)     :", p_theta_04, "\n")
cat("Likelihood P(A|theta=0.6):", round(p_A_given_06, 6), "\n")
cat("Likelihood P(A|theta=0.4):", round(p_A_given_04, 6), "\n")
cat("Posterior P(theta=0.6|A) :", round(posterior_06, 4), "\n")
cat("Posterior P(theta=0.4|A) :", round(posterior_04, 4), "\n\n")


# =============================================================================
# EXAMPLE 2: STUDENTS WEARING TROUSERS
# =============================================================================
# Problem: Mixed school — 60% boys, 40% girls. Girls wear trousers or skirts
# equally. Boys all wear trousers. Given a student is wearing trousers, what
# is the probability the student is a girl?
# -----------------------------------------------------------------------------

p_G         <- 0.4    # P(Girl)
p_B         <- 0.6    # P(Boy)
p_T_given_G <- 0.5    # P(Trousers | Girl)
p_T_given_B <- 1.0    # P(Trousers | Boy)

# Total probability of wearing trousers — Law of Total Probability
# P(T) = P(T|G)*P(G) + P(T|B)*P(B)
p_T <- p_T_given_G * p_G + p_T_given_B * p_B

# Posterior: P(Girl | Trousers) — Bayes' Theorem
# P(G|T) = P(T|G)*P(G) / P(T)
p_G_given_T <- (p_T_given_G * p_G) / p_T

cat("=== Example 2: Students Wearing Trousers ===\n")
cat("P(G)              :", p_G, "\n")
cat("P(B)              :", p_B, "\n")
cat("P(T | Girl)       :", p_T_given_G, "\n")
cat("P(T | Boy)        :", p_T_given_B, "\n")
cat("P(T)              :", p_T, "\n")
cat("P(Girl | Trousers):", p_G_given_T, "\n\n")


# =============================================================================
# EXAMPLE 3: DIABETES SCREENING TEST
# =============================================================================
# Problem: Evaluate sensitivity, specificity, prevalence, false negative rate,
# and false positive rate from a diabetes screening test on 10,000 people.
# -----------------------------------------------------------------------------

# Confusion matrix values
true_positive  <- 350     # Has diabetes AND tested positive
false_negative <- 150     # Has diabetes BUT tested negative
false_positive <- 1900    # No diabetes BUT tested positive
true_negative  <- 7600    # No diabetes AND tested negative

total_disease    <- true_positive + false_negative    # 500
total_no_disease <- false_positive + true_negative    # 9500
total_tested     <- total_disease + total_no_disease  # 10000

# Diagnostic metrics
sensitivity    <- true_positive / total_disease        # P(+ve | has disease)
specificity    <- true_negative / total_no_disease     # P(-ve | no disease)
prevalence     <- total_disease / total_tested         # P(has disease)
false_neg_rate <- false_negative / total_disease       # P(-ve | has disease)
false_pos_rate <- false_positive / total_no_disease    # P(+ve | no disease)

# Positive Predictive Value — P(disease | +ve test)  [Bayes' Theorem]
# PPV = sensitivity * prevalence /
#       [sensitivity * prevalence + (1-specificity) * (1-prevalence)]
PPV <- (sensitivity * prevalence) /
  (sensitivity * prevalence + (1 - specificity) * (1 - prevalence))

# Negative Predictive Value — P(no disease | -ve test)
NPV <- (specificity * (1 - prevalence)) /
  (specificity * (1 - prevalence) + (1 - sensitivity) * prevalence)

cat("=== Example 3: Diabetes Screening ===\n")
cat("Sensitivity (True Positive Rate) :", round(sensitivity, 4),
    "=", round(sensitivity * 100, 1), "%\n")
cat("Specificity (True Negative Rate) :", round(specificity, 4),
    "=", round(specificity * 100, 1), "%\n")
cat("Prevalence                       :", round(prevalence, 4),
    "=", round(prevalence * 100, 1), "%\n")
cat("False Negative Rate              :", round(false_neg_rate, 4),
    "=", round(false_neg_rate * 100, 1), "%\n")
cat("False Positive Rate              :", round(false_pos_rate, 4),
    "=", round(false_pos_rate * 100, 1), "%\n")
cat("Positive Predictive Value (PPV)  :", round(PPV, 4),
    "=", round(PPV * 100, 1), "%\n")
cat("Negative Predictive Value (NPV)  :", round(NPV, 4),
    "=", round(NPV * 100, 1), "%\n\n")


# =============================================================================
# EXAMPLE 4: DRUG TEST
# =============================================================================
# Problem: A drug test is 98% accurate (sensitivity = specificity = 98%).
# Only 0.5% of people use the drug. Given a positive test result, what is
# the probability the person actually uses the drug?
# -----------------------------------------------------------------------------

sensitivity_drug <- 0.98   # P(+ve | user)    — true positive rate
specificity_drug <- 0.98   # P(-ve | non-user) — true negative rate
prevalence_drug  <- 0.005  # P(user)           — 0.5% use the drug

p_pos_given_user    <- sensitivity_drug          # P(+ve | user)
p_pos_given_nonuser <- 1 - specificity_drug      # P(+ve | non-user)

# Bayes' Theorem:
# P(user | +ve) = P(+ve|user)*P(user) /
#                 [P(+ve|user)*P(user) + P(+ve|non-user)*P(non-user)]
numerator_drug   <- p_pos_given_user * prevalence_drug
denominator_drug <- numerator_drug +
  p_pos_given_nonuser * (1 - prevalence_drug)

posterior_drug <- numerator_drug / denominator_drug

cat("=== Example 4: Drug Test ===\n")
cat("Sensitivity (accuracy for users)    :", sensitivity_drug * 100, "%\n")
cat("Specificity (accuracy for non-users):", specificity_drug * 100, "%\n")
cat("Prevalence (drug users in population):", prevalence_drug * 100, "%\n")
cat("P(+ve test, user)                    :",
    round(numerator_drug, 6), "\n")
cat("P(+ve test, non-user)                :",
    round(p_pos_given_nonuser * (1 - prevalence_drug), 6), "\n")
cat("P(user | Positive Test)              :",
    round(posterior_drug * 100, 2), "%\n")
cat("Interpretation: Despite a positive test, the person is more likely\n")
cat("NOT a drug user due to the low prevalence (0.5%).\n\n")


# =============================================================================
# EXAMPLE 5: CEO REPLACEMENT AND STOCK PRICE
# =============================================================================
# Problem: 60% of companies with >5% stock price increase replaced their CEO.
# Only 35% of companies without >5% increase replaced their CEO.
# P(stock increases >5%) = 4%.
# Given a company replaced its CEO, find P(stock increases >5%).
# -----------------------------------------------------------------------------

p_A            <- 0.04    # P(stock increases > 5%)
p_B_given_A    <- 0.60    # P(CEO replaced | stock > 5%)
p_B_given_notA <- 0.35    # P(CEO replaced | stock NOT > 5%)

# Total probability of CEO replacement — Law of Total Probability
# P(B) = P(B|A)*P(A) + P(B|~A)*P(~A)
p_B <- p_B_given_A * p_A + p_B_given_notA * (1 - p_A)

# Posterior: P(stock > 5% | CEO replaced) — Bayes' Theorem
# P(A|B) = P(B|A)*P(A) / P(B)
p_A_given_B <- (p_B_given_A * p_A) / p_B

# Prior odds vs Posterior odds
prior_odds     <- p_A / (1 - p_A)
posterior_odds <- p_A_given_B / (1 - p_A_given_B)
bayes_factor   <- posterior_odds / prior_odds

cat("=== Example 5: CEO Replacement and Stock Price ===\n")
cat("P(A)  — P(stock > 5%)               :", p_A, "\n")
cat("P(B|A) — P(CEO replaced | stock>5%) :", p_B_given_A, "\n")
cat("P(B|~A)— P(CEO replaced |stock<=5%) :", p_B_given_notA, "\n")
cat("P(B)  — P(CEO replaced)             :", round(p_B, 4), "\n")
cat("P(A|B) — P(stock>5% | CEO replaced) :",
    round(p_A_given_B * 100, 2), "%\n")
cat("Prior Odds  P(A)/P(~A)              :", round(prior_odds, 4), "\n")
cat("Posterior Odds P(A|B)/P(~A|B)       :", round(posterior_odds, 4), "\n")
cat("Bayes Factor (Likelihood Ratio)     :", round(bayes_factor, 4), "\n\n")


# =============================================================================
# GENERAL UTILITY FUNCTION: Bayes' Theorem for Two Hypotheses
# =============================================================================
# A reusable function to apply Bayes' theorem for any two-hypothesis problem.
# Inputs:
#   prior      — P(H)        prior probability of hypothesis H
#   likelihood — P(E | H)    likelihood of evidence given H is true
#   lik_comp   — P(E | ~H)   likelihood of evidence given H is false
# Output: posterior P(H | E)
# -----------------------------------------------------------------------------

bayes_two_hypothesis <- function(prior, likelihood, lik_comp) {
  numerator   <- likelihood * prior
  denominator <- numerator + lik_comp * (1 - prior)
  posterior   <- numerator / denominator
  return(posterior)
}

# Verify Example 1 using the function
cat("=== Utility Function Verification ===\n")
cat("Example 1 (stock, theta=0.6) via function:",
    round(bayes_two_hypothesis(
      prior      = 0.5,
      likelihood = 0.6^5,
      lik_comp   = 0.4^5
    ), 4), "\n")

cat("Example 4 (drug test) via function:",
    round(bayes_two_hypothesis(
      prior      = 0.005,
      likelihood = 0.98,
      lik_comp   = 0.02
    ) * 100, 2), "%\n")

cat("Example 5 (CEO stock) via function:",
    round(bayes_two_hypothesis(
      prior      = 0.04,
      likelihood = 0.60,
      lik_comp   = 0.35
    ) * 100, 2), "%\n\n")


# =============================================================================
# END OF TOPIC ONE R CODES
# =============================================================================
