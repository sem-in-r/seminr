### Accompanying Code for the PLS Primer in R Workbook
## Chapter 3: Introduction to SEMinR

# Download and install the SEMinR package;
# You only need to do this once to equip RStudio on your computer with SEMinR
install.packages("seminr")

# Make the SEMinR library read to use;
# You must do this everytime you restart RStudio and wish to use SEMinR
library(seminr)

# Load the corporate reputation data
corp_rep_data <- read.csv(file = "Corporate Reputation Data.csv",
                          header = TRUE, sep = ";")

# Show the first several rows of the corporate reputation data
head(corp_rep_data)

# Create measurement model ----
simple_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3), weights = mode_A),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B))

# Create structural model ----
simple_sm <- relationships(
  paths(from = c("CSOR", "ATTR"), to = "COMP")
)

# Estimate the model
corp_rep_simple_model <- estimate_pls(data = corp_rep_data,
  measurement_model = simple_mm,
  structural_model  = simple_sm,
  inner_weights = path_weighting,
  missing = mean_replacement,
  missing_value = "-99")

# Estimate the model with default settings
corp_rep_simple_model <- estimate_pls(data = corp_rep_data,
  measurement_model = simple_mm,
  structural_model  = simple_sm)

# Summarize the model results
summary_simple_corp_rep <- summary(corp_rep_simple_model)

# Inspect the structural paths
summary_simple_corp_rep$paths

# Inspect the construct reliability metrics
summary_simple_corp_rep$reliability

# Bootstrap the model
boot_simple_corp_rep <- bootstrap_model(seminr_model = corp_rep_simple_model,
  nboot = 1000,
  cores = NULL,
  seed = 123)

# Store the summary of the bootstrapped model
sum_boot_simple_corp_rep <- summary(boot_simple_corp_rep)

# Inspect the bootstrapped structural paths
sum_boot_simple_corp_rep$bootstrapped_paths

# Inspect the bootstrapped outer weights
sum_boot_simple_corp_rep$bootstrapped_weights

# Write the bootstrapped paths object to csv file
write.csv(x = sum_boot_simple_corp_rep$bootstrapped_weights, file = "boot_paths.csv")

# Generate the plot for exporting
plot(summary_simple_corp_rep$reliability)
