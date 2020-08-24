### Accompanying Code for the PLS Primer in R Workbook
## Chapter 4: Reflective Model Assessment

# Load the SEMinR library
library(seminr)

# Loading and cleaning Data ----
corp_rep_data <- corp_rep_data
cleaned_data <- mean_replacement(data = corp_rep_data,
                                 missing_value_ind = -99)

# Creating measurement model ----
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Creating structural model ----
corp_rep_sm <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"),         to = c("CUSL"))
)

# Estimating the model
corp_rep_pls_model <- estimate_pls(data              = cleaned_data,
                                   measurement_model = corp_rep_mm,
                                   structural_model  = corp_rep_sm)

# Iterations to converge
corp_rep_pls_model$iterations

# Summarize the model results
summary_corp_rep <- summary(corp_rep_pls_model)
summary_corp_rep$validity$fl_criteria
summary_corp_rep$reliability

# Inspect the outer loadings
print(summary_corp_rep$loadings, digits = 3, na.print = ".")

# Inspect the indicator reliability
print(summary_corp_rep$loadings^2, digits = 3, na.print = ".")

# Inspect the composite reliability
print(summary_corp_rep$reliability, digits = 3, na.print = ".")

# Plot the rhoA reliabilities
plot_reliability(summary_corp_rep, "rhoA", 0.7)

# Plot the rhoC reliabilities
plot_reliability(summary_corp_rep, "rhoC", 0.7)

# Plot the AVE reliabilities
plot_reliability(summary_corp_rep, "AVE", 0.5)

# Table of the FL criteria
fl_criteria_table(summary_corp_rep)

# HTMT Ratio
print(summary_corp_rep$htmt, digits = 3, na.print = ".")

# Bootstrap the model
boot_corp_rep <- bootstrap_model(seminr_model = corp_rep_pls_model,
                                 nboot =        5000)

# Store the summary of the bootstrapped model
sum_boot_corp_rep <- summary(boot_corp_rep)

# Extract the bootstrapped loadings
sum_boot_corp_rep$bootstrapped_HTMT

