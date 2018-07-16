## ----echo=FALSE, out.width='25%'-----------------------------------------
knitr::include_graphics('SEMinR_logo.jpg')

## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(seminr)

## ---- eval=FALSE---------------------------------------------------------
#  # Distinguish and mix composite or reflective (common-factor) measurement models
#  measurements <- constructs(
#    composite("Image",       multi_items("IMAG", 1:5), weights = mode_B),
#    composite("Expectation", multi_items("CUEX", 1:3), weights = mode_A),
#    reflective("Loyalty",    multi_items("CUSL", 1:3))
#  )

## ---- eval=FALSE---------------------------------------------------------
#  # Easily create orthogonalized or scaled interactions between constructs
#  intxns <- interactions(
#    interaction_ortho("Image", "Expectation")
#  )

## ---- eval=FALSE---------------------------------------------------------
#  # Quickly create multiple paths "from" and "to" sets of constructs
#  structure <- relationships(
#    paths(from = c("Image", "Expectation", "Image*Expectation"),
#          to = "Loyalty")
#  )

## ---- eval=FALSE---------------------------------------------------------
#  # Dynamically compose SEM models from individual parts
#  pls_model <- estimate_pls(data = mobi, measurements, intxns, structure)
#  summary(pls_model)
#  
#  # Use multi-core parallel processing to speed up bootstraps
#  boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
#  summary(boot_estimates)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("seminr")

## ---- eval=FALSE---------------------------------------------------------
#  library(seminr)

## ------------------------------------------------------------------------
dim(mobi)
head(mobi)

## ------------------------------------------------------------------------
mobi_mm <- constructs(
  composite("Image",         multi_items("IMAG", 1:5), weights = mode_B),
  composite("Expectation",   multi_items("CUEX", 1:3), weights = regression_weights),
  composite("Quality",       multi_items("PERQ", 1:7), weights = mode_A),
  composite("Value",         multi_items("PERV", 1:2), weights = correlation_weights),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Complaints",   single_item("CUSCO")),
  reflective("Loyalty",      multi_items("CUSL", 1:3))
)

## ---- eval=FALSE---------------------------------------------------------
#  composite("Expectation", multi_items("CUEX", 1:3), weights = mode_A)
#  # is equivalent to:
#  composite("Expectation", multi_items("CUEX", 1:3), weights = correlation_weights)

## ---- eval=FALSE---------------------------------------------------------
#  composite("Image", multi_items("IMAG", 1:5), weights = mode_B)
#  # is equivalent to:
#  composite("Image", multi_items("IMAG", 1:5), weights = regression_weights)

## ---- eval = FALSE-------------------------------------------------------
#  reflective("Satisfaction", multi_items("CUSA", 1:3))

## ---- eval=FALSE---------------------------------------------------------
#  multi_items("IMAG", 1:5)
#  # which is equivalent to the R vector:
#  c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5")

## ---- eval=FALSE---------------------------------------------------------
#  single_item("CUSCO")
#  # which is equivalent to the R character string:
#  "CUSCO"

## ------------------------------------------------------------------------
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

## ------------------------------------------------------------------------
mobi_xm

## ---- eval = FALSE-------------------------------------------------------
#  # Orgthogonalized interaction between "Image" x "Expectation"
#  interaction_ortho("Image", "Expectation")
#  
#  # Scaled (mean-centered, standardized) interaction between "Image" x "Value"
#  interaction_scaled("Image", "Value")

## ------------------------------------------------------------------------
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

## ---- eval=FALSE---------------------------------------------------------
#  # "Image" -> "Expectation"
#  paths(from = "Image", to = "Expectation")

## ---- eval=FALSE---------------------------------------------------------
#  # "Image" -> "Expectation"
#  # "Image" -> "Satisfaction"
#  paths(from = "Image", to = c("Expectation", "Satisfaction"))

## ---- eval=FALSE---------------------------------------------------------
#  # "Image" -> "Satisfaction"
#  # "Expectation" -> "Satisfaction"
#  paths(from = c("Image", "Expectation"), to = "Satisfaction")

## ---- eval=FALSE---------------------------------------------------------
#  # "Expectation" -> "Value"
#  # "Expectation" -> "Satisfaction"
#  # "Quality" -> "Value"
#  # "Quality" -> "Satisfaction"
#  paths(from = c("Expectation", "Quality"), to = c("Value", "Satisfaction"))

## ------------------------------------------------------------------------
# define measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# specify interactions among constructs
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

# define structural model
# note: interactions cobnstruct should be named by its main constructs joined by a '*'
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         interactions = mobi_xm,
                         structural_model = mobi_sm,
                         inner_weights = path_weighting)

## ------------------------------------------------------------------------
# use 1000 bootstraps and utilize 2 parallel cores
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 1000,
                                 cores = 2)

## ------------------------------------------------------------------------
summary(mobi_pls)

## ------------------------------------------------------------------------
summary(boot_mobi_pls)

