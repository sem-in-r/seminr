## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(seminr)
data("mobi", package = "semPLS")

## ------------------------------------------------------------------------
data("mobi", package = "semPLS")
dim(mobi)
head(mobi)

## ------------------------------------------------------------------------
multi_items("IMAG", 1:5)

## ---- eval = FALSE-------------------------------------------------------
#  c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5")

## ------------------------------------------------------------------------
reflect("Image", multi_items("IMAG", 1:5))

## ------------------------------------------------------------------------
form("Image", multi_items("IMAG", 1:5))

## ---- eval = FALSE-------------------------------------------------------
#  c("Image", "IMAG1", "Image", "IMAG2", "Image", "IMAG3", "Image", "IMAG4", "Image", "IMAG5")

## ---- eval = FALSE-------------------------------------------------------
#  c("IMAG1", "Image", "IMAG2", "Image", "IMAG3", "Image", "IMAG4", "Image", "IMAG5", "Image")

## ------------------------------------------------------------------------
mobi_mm <- measure(
  reflect("Image",        multi_items("IMAG", 1:5)),
  reflect("Expectation",  multi_items("CUEX", 1:3)),
  reflect("Quality",      multi_items("PERQ", 1:7)),
  reflect("Value",        multi_items("PERV", 1:2)),
  form("Satisfaction",    multi_items("CUSA", 1:3)),
  form("Complaints",      single_item("CUSCO")),
  form("Loyalty",         multi_items("CUSL", 1:3))
)

## ------------------------------------------------------------------------
mobi_mm

## ---- eval = FALSE-------------------------------------------------------
#  mobi_mm <- matrix(c("Image","IMAG1",
#                      "Image","IMAG2",
#                      "Image","IMAG3",
#                      "Image","IMAG4",
#                      "Image","IMAG5",
#                      "Expectation","CUEX1",
#                      "Expectation","CUEX2",
#                      "Expectation","CUEX3",
#                      "Quality", "PERQ1",
#                      "Quality", "PERQ2",
#                      "Quality", "PERQ3",
#                      "Quality", "PERQ4",
#                      "Quality", "PERQ5",
#                      "Quality", "PERQ6",
#                      "Quality", "PERQ7",
#                      "Value","PERV1",
#                      "Value","PERV2",
#                      "CUSA1", "Satisfaction",
#                      "CUSA2", "Satisfaction",
#                      "CUSA3", "Satisfaction",
#                      "CUSCO", "Complaints",
#                      "CUSL1", "Loyalty",
#                      "CUSL2", "Loyalty",
#                      "CUSL3", "Loyalty"),nrow=24,ncol=2,byrow =TRUE,
#                     dimnames = list(1:24,c("source","target")))

## ------------------------------------------------------------------------
paths(from = "Image", to = c("Expectation", "Satisfaction", "Loyalty"))
paths(from = "Value", to = c("Satisfaction"))

## ---- eval = FALSE-------------------------------------------------------
#  c("Image", "Expectation", "Image", "Satisfaction", "Image", "Loyalty")
#  c("Value","Satisfaction")

## ------------------------------------------------------------------------
mobi_sm <- structure(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

## ------------------------------------------------------------------------
mobi_sm

## ---- eval = FALSE-------------------------------------------------------
#  mobi_sm <- matrix(c("Image","Expectation",
#                      "Image","Satisfaction",
#                      "Image","Loyalty",
#                      "Expectation","Quality",
#                      "Expectation","Value",
#                      "Expectation","Satisfaction",
#                      "Quality","Value",
#                      "Quality","Satisfaction",
#                      "Value", "Satisfaction",
#                      "Satisfaction", "Complaints",
#                      "Satisfaction", "Loyalty",
#                      "Complaints", "Loyalty"),nrow=12,ncol=2,byrow =TRUE,
#                     dimnames = list(1:12,c("source","target")))

## ---- eval = FALSE-------------------------------------------------------
#  interaction_combo("Image", "Expectation")
#  interaction_combo("Image", "Value")

## ------------------------------------------------------------------------
mobi_xm <- interact(
  interaction_combo("Image", "Expectation"),
  interaction_combo("Image", "Value")
)

## ------------------------------------------------------------------------
mobi_xm

## ------------------------------------------------------------------------
# seminr syntax for creating measurement model
mobi_mm <- measure(
  reflect("Image",        multi_items("IMAG", 1:5)),
  reflect("Expectation",  multi_items("CUEX", 1:3)),
  reflect("Value",        multi_items("PERV", 1:2)),
  reflect("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interact(
  interaction_combo("Image", "Expectation"),
  interaction_combo("Image", "Value")
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- structure(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)
plsm_model <- model(data = mobi,
                    measurement_model = mobi_mm,
                    interactions = mobi_xm,
                    structural_model = mobi_sm)

mobi_pls <- estimate(plsm_model)

## ------------------------------------------------------------------------
plsm_model <- model(data = mobi,
                    measurement_model = mobi_mm,
                    interactions = mobi_xm,
                    structural_model = mobi_sm)
mobi_pls <- estimate(plsm_model)
mobi_pls
print_paths(mobi_pls)
plot_scores(mobi_pls)

