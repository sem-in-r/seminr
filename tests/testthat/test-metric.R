context("SEMinR correctly estimates the model R-Squared\n")

# Test cases
## Regular case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = "A"),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = "A"),
  composite("Value",        multi_items("PERV", 1:2),weights = "A"),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = "A")
)


# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",  from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL ,mobi_sm)


# Load outputs
rsquared <- seminr_model$rSquared

## Output originally created using following lines
# write.csv(seminr_model$rSquared, file = "tests/fixtures/rsquared1.csv")


# Load controls
rsquared_control <- as.matrix(read.csv("../fixtures/rsquared1.csv", row.names = 1))


# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(rsquared[,1], rsquared_control[,1])
})

