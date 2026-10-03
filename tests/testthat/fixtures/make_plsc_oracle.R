# Generates plsc_oracle.rds: oracle predictions for the PLSc prediction tests (#425).
# Independent of seminr's prediction code: the model-implied indicator correlation
# matrix comes from cSEM::fit() on the same PLSc model, and predictions are the
# conditional expectation mu_y + Sigma_yx Sigma_xx^-1 (x - mu_x) (de Rooij et al., 2023).
# Run from the package root:  Rscript tests/testthat/fixtures/make_plsc_oracle.R
# Generated with cSEM 0.6.1.9000; mixed_mode_A added with cSEM 0.7.1 (the other
# designs reproduce exactly under 0.7.1).
suppressMessages(library(cSEM))
d <- seminr::mobi
train <- d[1:200, ]; test <- d[201:250, ]
it <- function(stem, k) paste0(stem, k)
blocks <- list(Image = it("IMAG", 1:5), Expectation = it("CUEX", 1:3),
               Quality = it("PERQ", 1:7), Value = it("PERV", 1:2),
               Satisfaction = it("CUSA", 1:3), Complaints = "CUSCO")
implied_predict <- function(model_syntax, x_constructs_by_y, ...) {
  fit <- csem(train, model_syntax, .disattenuate = TRUE, ...)
  S <- cSEM::fit(fit)
  paths <- fit$Estimates$Path_estimates
  items <- rownames(S)
  mu <- colMeans(train[, items]); s <- apply(train[, items], 2, stats::sd)
  preds <- lapply(names(x_constructs_by_y), function(y) {
    xi <- unlist(blocks[x_constructs_by_y[[y]]]); yi <- blocks[[y]]
    Z <- scale(as.matrix(test[, xi]), mu[xi], s[xi])
    P <- Z %*% solve(S[xi, xi], S[xi, yi, drop = FALSE])
    sweep(sweep(P, 2, s[yi], "*"), 2, mu[yi], "+")
  })
  list(items = do.call(cbind, preds), paths = paths)
}
reflective_two <- implied_predict("
  Image =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Expectation =~ CUEX1 + CUEX2 + CUEX3
  Satisfaction =~ CUSA1 + CUSA2 + CUSA3
  Satisfaction ~ Image + Expectation",
  list(Satisfaction = c("Image", "Expectation")))
chain_syntax <- "
  Image =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Expectation =~ CUEX1 + CUEX2 + CUEX3
  Quality =~ PERQ1 + PERQ2 + PERQ3 + PERQ4 + PERQ5 + PERQ6 + PERQ7
  Satisfaction =~ CUSA1 + CUSA2 + CUSA3
  Expectation ~ Image
  Quality ~ Expectation
  Satisfaction ~ Image + Expectation + Quality"
chain_DA <- implied_predict(chain_syntax, list(Expectation = "Image",
  Quality = "Expectation", Satisfaction = c("Image", "Expectation", "Quality")))
chain_EA <- implied_predict(chain_syntax, list(Expectation = "Image",
  Quality = "Image", Satisfaction = "Image"))
mixed <- implied_predict("
  Image =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Value <~ PERV1 + PERV2
  Satisfaction =~ CUSA1 + CUSA2 + CUSA3
  Satisfaction ~ Image + Value",
  list(Satisfaction = c("Image", "Value")))
# Mode A composite in a PLSc model: cSEM treats composites as fully reliable
# (rho = 1), as in Dijkstra & Henseler (2015). <~ is Mode B in cSEM by default.
mixed_mode_A <- implied_predict("
  Image =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Quality <~ PERQ1 + PERQ2 + PERQ3 + PERQ4 + PERQ5 + PERQ6 + PERQ7
  Value <~ PERV1 + PERV2
  Satisfaction =~ CUSA1 + CUSA2 + CUSA3
  Quality ~ Image
  Satisfaction ~ Image + Quality + Value",
  list(Quality = "Image", Satisfaction = c("Image", "Quality", "Value")),
  .PLS_modes = list(Quality = "modeA"))
single_item <- implied_predict("
  Image =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Satisfaction =~ CUSA1 + CUSA2 + CUSA3
  Complaints =~ CUSCO
  Satisfaction ~ Image
  Complaints ~ Satisfaction",
  list(Satisfaction = "Image", Complaints = "Satisfaction"))
saveRDS(list(reflective_two = reflective_two, chain_DA = chain_DA,
             chain_EA = chain_EA, mixed = mixed, mixed_mode_A = mixed_mode_A,
             single_item = single_item),
        "tests/testthat/fixtures/plsc_oracle.rds", version = 2)
