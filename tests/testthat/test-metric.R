context("SEMinR correctly estimates the model R-Squared\n")

# Test cases
## Regular case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)


# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",  from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm)

# Load outputs
rsquared <- seminr_model$rSquared
fsquared <- fSquared(seminr_model, "Image", "Satisfaction")

## Output originally created using following lines
# write.csv(seminr_model$rSquared, file = "tests/fixtures/rsquared1.csv")
# write.csv(fSquared(seminr_model, "Image", "Satisfaction"), file = "tests/fixtures/V_3_5_X/fsquared1.csv")
# write.csv(fSquared(seminr_model, "Image", "Satisfaction"), file = "tests/fixtures/V_3_6_0/fsquared1.csv")


# Load controls
rsquared_control <- as.matrix(read.csv(file = paste(test_folder,"rsquared1.csv", sep = ""), row.names = 1))
fsquared_control <- as.matrix(read.csv(file = paste(test_folder,"fsquared1.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr estimates the Rsquared correctly", {
  expect_equal(rsquared[1:2,1], rsquared_control[1:2,1], tolerance = 0.00001)
})

test_that("Seminr estimates the fSquared correctly", {
  expect_equal(fsquared, fsquared_control[1,1], tolerance = 0.00001)
})
