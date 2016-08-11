source("R/syntax.R")

data("mobi")

# modelr syntax for creating measurement model

img <- construct("Image", "IMAG", 1:5)
exp <- construct("Expectation", "CUEX", 1:3)
qly <- construct("Quality", "PERQ", 1:7)
val <- construct("Value", "PERV", 1:2)
sat <- construct("Satisfaction", "CUSA", 1:3)
com <- single_item("Complaints", "CUSCO")
loy <- construct("Loyalty", "CUSL", 1:3)

mobi_mm <- measure(img, exp, qly, val, sat, com, loy)

# modelr syntax for creating structural model

mobi_sm <- structure(
  paths(from = "Image", to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation", to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality", to = c("Value", "Satisfaction")),
  paths(from = "Value", to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints", to = "Loyalty")
)

# Regular semPLS functions to create andestimate model, and report estimates
mobi_pls <- plsm(data = mobi, strucmod = mobi_sm, measuremod = mobi_mm)
mobi_pls_fitted <- sempls(model = mobi_pls, data = mobi)
pathCoeff(mobi_pls_fitted)
rSquared(mobi_pls_fitted)

# modelr function to see scatterplot of scores
plot_scores(mobi_pls_fitted)
