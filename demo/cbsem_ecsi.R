library(seminr)

# Get data from file or elsewhere.
# For this demo, we will use the included mobi dataset.
mobi <- mobi

#seminr syntax for creating measurement model
mobi_mm <- constructs(
             reflective("Image",        multi_items("IMAG", 1:5)),
             reflective("Expectation",  multi_items("CUEX", 1:3)),
             reflective("Quality",      multi_items("PERQ", 1:7)),
             reflective("Value",        multi_items("PERV", 1:2)),
             reflective("Satisfaction", multi_items("CUSA", 1:3)),
             reflective("Complaints",   single_item("CUSCO")),
             reflective("Loyalty",      multi_items("CUSL", 1:3))
           )

#seminr syntax for freeing up item-item covariances
mobi_am <- associations(
             item_errors(c("PERQ1", "PERQ2"), "CUEX3"),
             item_errors("IMAG1", "CUEX2")
           )

#seminr syntax for creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

# Estimate model and get results
# Note: if your measurement model contains composites,
#       use `all.reflective(mobi_mm) to convert all
#       constructs to reflective measurement`
cbsem <- estimate_cbsem(mobi, mobi_mm, mobi_sm, mobi_am)

