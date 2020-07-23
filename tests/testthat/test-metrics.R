context("SEMinR correctly computes metrics")

mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2))
)

mobi_cfa <- estimate_cfa(mobi, mobi_mm)
scores <- mobi_cfa$construct_scores

cfa_summary <- summary(mobi_cfa)
cors <- cfa_summary$descriptives$correlations$constructs

# cor_vifs
test_that("Computes cor_vifs correctly\n", {
  lm_vif <- function(data, model) {
    1/(1-summary(lm(model, as.data.frame(data)))$r.squared)
  }

  img <- lm_vif(scores, Image ~ Expectation + Value)
  exp <- lm_vif(scores, Expectation ~ Image + Value)
  val <- lm_vif(scores, Value ~ Image + Expectation)
  cvifs <- cor_vifs(cors, c("Image", "Expectation", "Value"))

  expect_equal(c(Image=img, Expectation=exp, Value=val), cvifs, 
               tolerance = 0.00001)
})