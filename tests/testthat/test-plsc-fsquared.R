context("SEMinR correctly evaluates fSquared for PLSc estimation\n")

# data_cov <- utils::read.csv(file = paste(test_folder, "fsq_data_cov.csv", sep=""))
# data_means <- as.vector(t(utils::read.csv(file = paste(test_folder, "fsq_data_means.csv", sep=""))))
#
# data_sim <- as.data.frame(MASS::mvrnorm(250, mu = data_means, Sigma = data_cov))
#
# create_item_names <- function(names) {
#   as.vector(sapply(names, multi_items, item_numbers=1:3))
# }
#
# construct_names <- c("GG", "FF", "EE", "DD", "CC", "BB", "AA")
# item_names <- create_item_names(construct_names)
# names(data_sim) <- item_names
# write.csv(data_sim, file = "test-plsc-fsquared-data.csv")

data_sim <- utils::read.csv(file = paste(test_folder, "test-plsc-fsquared-data.csv", sep=""))

measurements <- constructs(
  reflective("AA", multi_items("AA", 1:3)),
  reflective("BB", multi_items("BB", 1:3)),
  reflective("CC", multi_items("CC", 1:3)),
  reflective("DD", multi_items("DD", 1:3)),
  reflective("EE", multi_items("EE", 1:3)),
  reflective("FF", multi_items("FF", 1:3)),
  reflective("GG", multi_items("GG", 1:3))
)

structure <- relationships(
  paths(from = "AA", to = "BB"),
  paths(from = "BB", to = c("CC", "EE")),
  paths(from = "CC", to = "DD"),
  paths(from = "EE", to = "FF"),
  paths(from = c("FF", "DD"), to = "GG")
)

# Compose SEM models from individual parts
pls_model <- estimate_pls(data = data_sim,
                          measurement_model = measurements,
                          structural_model = structure)

# Testing
summarize <- function() {
  model_summary <- summary(pls_model)
  model_summary$fSquare
}

test_that("No error occurs trying to summarize model fSquares", {
  expect_error(summarize(), NA)
})

context("SEMinR correctly evaluates fSquared for interaction model\n")

# Create measurement model ----
corp_rep_mm_mod <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("SC", multi_items("switch_", 1:4)),
  composite("CUSL", multi_items("cusl_", 1:3)),
  interaction_term(iv = "CUSA", moderator = "SC", method = two_stage, weights = mode_A))

# Create structural model ----
corp_rep_sm_mod <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA", "SC", "CUSA*SC"), to = c("CUSL"))
)

# Estimate the model ----
corp_rep_pls_model_mod <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm_mod,
  structural_model  = corp_rep_sm_mod,
  missing = mean_replacement,
  missing_value = "-99")

sum_corp_rep_mod <- summary(corp_rep_pls_model_mod)

output <- sum_corp_rep_mod$fSquare

## Output originally created using following lines
# write.csv(sum_corp_rep_mod$fSquare, file = "tests/fixtures/V_3_6_0/interaction_fsquare.csv", row.names=TRUE)

# Load controls
output_control <- as.matrix(read.csv(file = paste(test_folder,"interaction_fsquare.csv", sep = ""), row.names = 1, check.names = FALSE))

# Testing
test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(as.matrix(output[1:10,1:10]), output_control, tolerance = 0.00001)
})
