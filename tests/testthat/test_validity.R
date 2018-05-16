context("SEMinR correctly returns validity metrics in summary for class seminr_model\n")

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_weighting)
summary_object <- summary(seminr_model)

flatten_vifs <- function(vif_results) {
  as.data.frame(t(unlist(vif_results)))
}

flat_vif_items <- flatten_vifs(summary_object$vif_items)
flat_vif_antecedents <- flatten_vifs(summary_object$vif_antecedents)

## Create Fixtures
# write.csv(flat_vif_items, "tests/fixtures/vifs/flat_item_vifs.csv", row.names = FALSE)
# write.csv(flat_vif_antecedents, "tests/fixtures/vifs/flat_vif_antecedents.csv", row.names = FALSE)

# Read Fixtures
correct_item_vifs <- read.csv("../fixtures/vifs/flat_item_vifs.csv")
correct_vif_antecedents <- read.csv("../fixtures/vifs/flat_vif_antecedents.csv")


## Tests
test_that("Seminr computs the item VIFs correctly", {
  expect_equal(flat_vif_items, correct_item_vifs)
})

test_that("Seminr computs the item VIFs correctly", {
  expect_equal(flat_vif_items, correct_item_vifs)
})
