# Thanks to @PascalHessler for raising issue #357 about the Wald test and
# providing starter code to arrive at these functions.

vcov.boot_seminr_model <- function(boot_obj, endogenous = NULL) {
  sm <- boot_obj$smMatrix
  if (!is.null(endogenous)) {
    sm <- sm[sm[, "target"] %in% endogenous, ]
  }

  boot_paths <- boot_obj$boot_paths

  boot_path_matrix <- apply(
    boot_paths,
    MARGIN = 3,
    FUN = \(boot_i) apply(sm, MARGIN = 1, FUN = \(path) boot_i[path['source'], path['target']])
  ) |> t()

  colnames(boot_path_matrix) <- apply(sm, MARGIN=1, \(path) paste(path['source'], path['target'], sep = '->'))
  cov(boot_path_matrix)
}

coef.boot_seminr_model <- function(boot_obj, endogenous = NULL) {
  sm <- boot_obj$smMatrix
  if (!is.null(endogenous)) {
    sm <- sm[sm[, "target"] %in% endogenous, ]
  } else {
    endogenous <- unique(sm[, "target"])
  }

  exogenous <- unique(sm[, "source"])

  boot_obj$path_coef[exogenous, endogenous]
}

# library(aod)

# lm_model <- lm(CUSL1 ~ IMAG1 + CUSA1 + CUSCO, data=mobi)
# wald.test(b=coef(lm_model), Sigma=vcov(lm_model), Terms=c(2,3))


# library(seminr)

# mobi_mm <- constructs(
#   composite("IMG",        multi_items("IMAG", 1:5)),
#   composite("EXP",  multi_items("CUEX", 1:3)),
#   composite("QLT",      multi_items("PERQ", 1:7)),
#   composite("VAL",        multi_items("PERV", 1:2)),
#   composite("SAT", multi_items("CUSA", 1:3)),
#   composite("COM",   single_item("CUSCO")),
#   composite("LOY",      multi_items("CUSL", 1:3))
# )

# mobi_sm <- relationships(
#   paths(from = "IMG", to = c("EXP", "SAT", "LOY")),
#   paths(from = "EXP", to = c("QLT", "VAL", "SAT")),
#   paths(from = "QLT", to = c("VAL", "SAT")),
#   paths(from = "VAL", to = c("SAT")),
#   paths(from = "SAT", to = c("COM", "LOY")),
#   paths(from = "COM", to = "LOY")
# )

# mobi_pls <- estimate_pls(
#   data = mobi,
#   measurement_model = mobi_mm,
#   structural_model = mobi_sm
# )

# boot_pls <- bootstrap_model(seminr_model = mobi_pls, nboot = 1000)

# b <- coef.boot_seminr_model(boot_obj = boot_pls, endogenous = "LOY")
# S <- vcov.boot_seminr_model(boot_obj = boot_pls, endogenous = "LOY")

# # Tests the joined hypotheses of 'SAT->LOY' == 0 and 'COM->LOY' == 0
# # Corresponding L Matrix = [0 1 0, 0 0 1]
# wald.test(b = b, Sigma = S, Terms = c(2, 3))

# # If you want to test if to pathes are equal to each other you need to provide the L argument representing the linear combinations
# # of the coefficients to be tested
# # For example: 'SAT->LOY'=='COM->LOY'  would correspond to a Matrix [0  1 -1]
# wald.test(b = b, Sigma = S, L = matrix(c(0, 1, -1), nrow = 1))

# # More on L matrix at: https://cran.r-project.org/web/packages/clubSandwich/vignettes/Wald-tests-in-clubSandwich.html
