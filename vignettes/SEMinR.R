## ----echo=FALSE, out.width='25%'----------------------------------------------
knitr::include_graphics('SEMinR_logo.jpg')

## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(seminr)

## ---- eval=FALSE--------------------------------------------------------------
#  # Distinguish and mix composite measurement (used in PLS-PM)
#  # or reflective (common-factor) measurement (used in CBSEM, CFA, and PLSc)
#  # - We will first use composites in PLS-PM analysis
#  # - Later we will convert the omposites into reflectives for CFA/CBSEM (step 3)
#  measurements <- constructs(
#    composite("Image",        multi_items("IMAG", 1:5)),
#    composite("Expectation",  multi_items("CUEX", 1:3)),
#    composite("Value",        multi_items("PERV", 1:2)),
#    composite("Satisfaction", multi_items("CUSA", 1:3)),
#    interaction_term(iv = "Image", moderator = "Expectation")
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  # Quickly create multiple paths "from" and "to" sets of constructs
#  structure <- relationships(
#    paths(from = c("Image", "Expectation", "Image*Expectation"), to = "Value"),
#    paths(from = "Value", to = "Satisfaction")
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  # Estimate using PLS-PM from model parts defined earlier
#  pls_model <- estimate_pls(data = mobi,
#                            measurement_model = measurements,
#                            structural_model = structure)
#  summary(pls_model)
#  
#  # note: PLS requires seperate bootstrapping for PLS path estimates
#  # SEMinR uses multi-core parallel processing to speed up bootstrapping
#  boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
#  summary(boot_estimates)
#  
#  # Alternatively, we could estimate our model using CBSEM, which uses the Lavaan package
#  # We often wish to conduct a CFA of our measurement model prior to CBSEM
#  # note: we must convert composites in our measurement model into reflective constructs for CFA/CBSEM
#  cfa_model <- estimate_cfa(data = mobi, as.reflective(measurements))
#  summary(cfa_model)
#  
#  cbsem_model <- estimate_cbsem(data = mobi, as.reflective(measurements), structure)
#  summary(cbsem_model)
#  
#  # note: the Lavaan syntax and Lavaan fitted model can be extracted for your own specific needs
#  cbsem_model$lavaan_syntax
#  cbsem_model$lavaan_model

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("seminr")

## ---- eval=FALSE--------------------------------------------------------------
#  library(seminr)

## ---- eval=FALSE--------------------------------------------------------------
#  survey_data <- read.csv("mobi_survey_data.csv")

## -----------------------------------------------------------------------------
dim(mobi)
head(mobi)

## ---- eval=FALSE--------------------------------------------------------------
#  measurements <- constructs(
#    composite("Image",         multi_items("IMAG", 1:5), weights = mode_B),
#    composite("Expectation",   multi_items("CUEX", 1:3), weights = regression_weights),
#    composite("Quality",       multi_items("PERQ", 1:7), weights = mode_A),
#    composite("Value",         multi_items("PERV", 1:2), weights = correlation_weights),
#    reflective("Satisfaction", multi_items("CUSA", 1:3)),
#    reflective("Complaints",   single_item("CUSCO")),
#    higher_composite("HOC", c("Value", "Satisfaction"), orthogonal, mode_A),
#    interaction_term(iv = "Image", moderator = "Expectation", method =  orthogonal, weights = mode_A),
#    reflective("Loyalty",      multi_items("CUSL", 1:3))
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  composite("Expectation", multi_items("CUEX", 1:3), weights = mode_A)
#  # is equivalent to:
#  composite("Expectation", multi_items("CUEX", 1:3), weights = correlation_weights)

## ---- eval=FALSE--------------------------------------------------------------
#  composite("Image", multi_items("IMAG", 1:5), weights = mode_B)
#  # is equivalent to:
#  composite("Image", multi_items("IMAG", 1:5), weights = regression_weights)

## ---- eval = FALSE------------------------------------------------------------
#  reflective("Satisfaction", multi_items("CUSA", 1:3))

## ---- eval = FALSE------------------------------------------------------------
#  # Coerce a composite into reflective form
#  img_composite <- composite("Image", multi_items("IMAG", 1:5))
#  img_reflective <- as.reflective(img_composite)
#  
#  # Coerce all constructs of a measurement model into composite form
#  mobi_composites <- constructs(
#    composite("Image",         multi_items("IMAG", 1:5)),
#    composite("Expectation",   multi_items("CUEX", 1:3)),
#    reflective("Complaints",   single_item("CUSCO"))
#  )
#  mobi_reflective <- as.reflective(mobi_composites)

## ---- eval=FALSE--------------------------------------------------------------
#  multi_items("IMAG", 1:5)
#  # which is equivalent to the R vector:
#  c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5")

## ---- eval=FALSE--------------------------------------------------------------
#  multi_items("IMAG", c(1, 3:5))
#  # which is equivalent to the R vector:
#  c("IMAG1", "IMAG3", "IMAG4", "IMAG5")

## ---- eval=FALSE--------------------------------------------------------------
#  single_item("CUSCO")
#  # which is equivalent to the R character string:
#  "CUSCO"

## ---- eval=FALSE--------------------------------------------------------------
#  # The following specifies that items PERQ1 and PERQ2 covary with each other, both covary with IMAG1
#  mobi_am <- associations(
#    item_errors("PERQ1", "PERQ2"),
#    item_errors(c("PERQ1", "PERQ2"), "IMAG1")
#  )

## ---- eval = FALSE------------------------------------------------------------
#  # By default, interaction terms are computed using two stage procedures
#  interaction_term(iv = "Image", moderator = "Expectation")
#  
#  # You can also explicitly specify how to create the interaction term
#  interaction_term(iv = "Image", moderator = "Expectation", method =  two_stage)
#  interaction_term(iv = "Image", moderator = "Expectation", method =  product_indicator)
#  interaction_term(iv = "Image", moderator = "Expectation", method =  orthogonal)

## ---- eval=FALSE--------------------------------------------------------------
#  mobi_sm <- relationships(
#    paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#    paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#    paths(from = "Quality",      to = c("Value", "Satisfaction")),
#    paths(from = "Value",        to = c("Satisfaction")),
#    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#    paths(from = "Complaints",   to = "Loyalty")
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  # "Image" -> "Expectation"
#  paths(from = "Image", to = "Expectation")

## ---- eval=FALSE--------------------------------------------------------------
#  # "Image" -> "Expectation"
#  # "Image" -> "Satisfaction"
#  paths(from = "Image", to = c("Expectation", "Satisfaction"))

## ---- eval=FALSE--------------------------------------------------------------
#  # "Image" -> "Satisfaction"
#  # "Expectation" -> "Satisfaction"
#  paths(from = c("Image", "Expectation"), to = "Satisfaction")

## ---- eval=FALSE--------------------------------------------------------------
#  # "Expectation" -> "Value"
#  # "Expectation" -> "Satisfaction"
#  # "Quality" -> "Value"
#  # "Quality" -> "Satisfaction"
#  paths(from = c("Expectation", "Quality"), to = c("Value", "Satisfaction"))

