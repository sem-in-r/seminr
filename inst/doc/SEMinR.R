## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(seminr)

## ------------------------------------------------------------------------
dim(mobi)
head(mobi)

## ---- eval=FALSE---------------------------------------------------------
#  multi_items("IMAG", 1:5)

## ---- eval=FALSE---------------------------------------------------------
#  single_item("CUSCO")

## ---- eval=FALSE---------------------------------------------------------
#  composite("Image", multi_items("IMAG", 1:5), weights = "A")

## ---- eval=FALSE---------------------------------------------------------
#  composite("Image", multi_items("IMAG", 1:5), weights = "correlation")

## ---- eval=FALSE---------------------------------------------------------
#  composite("Image", multi_items("IMAG", 1:5), weights = "B")

## ---- eval=FALSE---------------------------------------------------------
#  composite("Image", multi_items("IMAG", 1:5), weights = "regression")

## ---- eval = FALSE-------------------------------------------------------
#  reflective("Image", multi_items("IMAG", 1:5))

## ------------------------------------------------------------------------
mobi_mm <- constructs(
  composite("Image",         multi_items("IMAG", 1:5), weights = "B"),
  composite("Expectation",   multi_items("CUEX", 1:3), weights = "regression"),
  composite("Quality",       multi_items("PERQ", 1:7), weights = "A"),
  composite("Value",         multi_items("PERV", 1:2), weights = "correlation"),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Complaints",   single_item("CUSCO")),
  reflective("Loyalty",      multi_items("CUSL", 1:3))
)
mobi_mm

## ---- eval=FALSE---------------------------------------------------------
#  paths(from = "Image",                     to = c("Expectation", "Satisfaction"))
#  paths(from = "Value",                     to = "Satisfaction")
#  paths(from = c("Satisfaction","Loyalty"), to = "Value")

## ------------------------------------------------------------------------
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)
mobi_sm

## ---- eval = FALSE-------------------------------------------------------
#  interaction_ortho("Image", "Expectation")
#  interaction_scaled("Image", "Value")

## ------------------------------------------------------------------------
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

## ------------------------------------------------------------------------
mobi_xm

## ------------------------------------------------------------------------
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         interactions = mobi_xm,
                         structural_model = mobi_sm,
                         inner_weights = path_weighting)

## ------------------------------------------------------------------------
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 2000,
                                 cores = 4)

## ------------------------------------------------------------------------
summary(mobi_pls)

## ------------------------------------------------------------------------
summary(boot_mobi_pls)

