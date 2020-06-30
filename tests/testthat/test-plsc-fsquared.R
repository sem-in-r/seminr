context("SEMinR correctly evaluates fSquared for PLSc estimation\n")

data_cov <- utils::read.csv(file = paste(test_folder, "fsq_data_cov.csv", sep=""))
data_means <- as.vector(t(utils::read.csv(file = paste(test_folder, "fsq_data_means.csv", sep=""))))

data_sim <- as.data.frame(MASS::mvrnorm(250, mu = data_means, Sigma = data_cov))

create_item_names <- function(names) {
  as.vector(sapply(names, multi_items, item_numbers=1:3))
}

construct_names <- c("GG", "FF", "EE", "DD", "CC", "BB", "AA")
item_names <- create_item_names(construct_names)
names(data_sim) <- item_names

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
