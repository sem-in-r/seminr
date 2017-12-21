## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(seminr)
mobi <- mobi

## ------------------------------------------------------------------------
mobi <- mobi
dim(mobi)
head(mobi)

## ---- eval = FALSE-------------------------------------------------------
#  multi_items("IMAG", 1:5)

## ---- eval = FALSE-------------------------------------------------------
#  c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5")

## ---- eval=FALSE---------------------------------------------------------
#  reflective("Image", multi_items("IMAG", 1:5))

## ---- eval=FALSE---------------------------------------------------------
#  composite("Image", multi_items("IMAG", 1:5), weights = "B")

## ---- eval = FALSE-------------------------------------------------------
#  c("Image", "IMAG1", "Image", "IMAG2", "Image", "IMAG3", "Image", "IMAG4", "Image", "IMAG5")

## ---- eval = FALSE-------------------------------------------------------
#  c("IMAG1", "Image", "IMAG2", "Image", "IMAG3", "Image", "IMAG4", "Image", "IMAG5", "Image")

## ------------------------------------------------------------------------
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction",    multi_items("CUSA", 1:3), weights = "B"),
  composite("Complaints",      single_item("CUSCO"), weights = "B"),
  composite("Loyalty",         multi_items("CUSL", 1:3), weights = "B")
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
mobi_sm <- relationships(
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
#  interaction_scaled("Image", "Expectation")
#  interaction_scaled("Image", "Value")

## ---- eval = FALSE-------------------------------------------------------
#  interaction_ortho("Image", "Expectation")
#  interaction_ortho("Image", "Value")

## ------------------------------------------------------------------------
mobi_xm <- interactions(
  interaction_scaled("Image", "Expectation"),
  interaction_scaled("Image", "Value")
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
  interaction_scaled("Image", "Expectation"),
  interaction_scaled("Image", "Value")
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
                           structural_model = mobi_sm)

