context("SEMinR correctly estimates CBSEM interaction models\n")

write_controls <- function(cbsem_model, cbsem_summary, approach) {
  filename <- function(values, approach) {
    paste("tests/fixtures/cbsem-interaction-", approach, "-", values, ".csv", sep="")
  }

  write.csv(cbsem_summary$paths$coefficients, file = filename("paths-coefficients", approach))
  write.csv(cbsem_summary$quality$reliability, file = filename("quality-reliability", approach))
  write.csv(cbsem_model$construct_scores, file = filename("factor_scores", approach), row.names = FALSE)

  print("Please move files into correct fixture folders by hand")
}

load_controls <- function(test_folder, approach) {
  list(
    betas = as.matrix(read.csv(file = paste(test_folder, "cbsem-interaction-", approach, "-paths-coefficients.csv", sep = ""), row.names = 1)),
    reliability = as.matrix(read.csv(file = paste(test_folder, "cbsem-interaction-", approach, "-quality-reliability.csv", sep = ""), row.names = 1)),
    scores = as.matrix(read.csv(file = paste(test_folder, "cbsem-interaction-", approach, "-factor_scores.csv", sep = "")))
  )
}

# Test cases
## Simple case
# Creating our measurement model
mobi_partial_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  single_item("CUEX3")),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

# Structural model
#  note: interactions should be the names of its main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation")
  )
)

# PRODUCT INDICATOR APPROACH
intxn_pi <- interaction_term(iv = "Image", moderator = "Expectation", method = product_indicator)
mobi_mm <- append(mobi_partial_mm, c(scaled_interaction=intxn_pi))
mobi_cbsem <- estimate_cbsem(data = mobi, measurement_model = mobi_mm, structural_model = mobi_sm)

cbsem_summary <- summary(mobi_cbsem)

# write_controls(mobi_cbsem, cbsem_summary, "pi")
controls <- load_controls(test_folder, "pi")

test_that("Seminr estimates PI interaction paths correctly\n", {
  expect_equal(cbsem_summary$paths$coefficients, controls$betas, tolerance = 0.00001)
})

test_that("Seminr estimates PI interaction AVE, rhoC (reliability) correctly\n", {
  expect_equal(cbsem_summary$quality$reliability, controls$reliability, tolerance = 0.00001)
})

test_that("Seminr estimates PI ten Berge factor scores correctly\n", {
  expect_equal(mobi_cbsem$construct_scores, controls$scores, tolerance = 0.00001)
})


# TWO-STAGE INDICATOR APPROACH
intxn_pi <- interaction_term(iv = "Image", moderator = "Expectation", method = two_stage)
mobi_mm <- append(mobi_partial_mm, c(scaled_interaction=intxn_pi))
mobi_cbsem <- estimate_cbsem(data = mobi, measurement_model = mobi_mm, structural_model = mobi_sm)

cbsem_summary <- summary(mobi_cbsem)

# write_controls(mobi_cbsem,cbsem_summary, "2stage")
controls <- load_controls(test_folder, "2stage")

test_that("Seminr estimates TWO-STAGE interaction paths correctly\n", {
  expect_equal(cbsem_summary$paths$coefficients, controls$betas, tolerance = 0.00001)
})

test_that("Seminr estimates TWO-STAGE interaction AVE, rhoC (reliability) correctly\n", {
  expect_equal(cbsem_summary$quality$reliability, controls$reliability, tolerance = 0.00001)
})

test_that("Seminr estimates TWO-STAGE ten Berge factor scores correctly\n", {
  expect_equal(mobi_cbsem$construct_scores, controls$scores, tolerance = 0.00001)
})

