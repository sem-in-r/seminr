library(seminr)
library(DiagrammeR)

mobi <- mobi

#seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  higher_composite("Quality",  c("Image","Expectation"), method = two_stage),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_B)
)
#seminr syntax for creating structural model
mobi_sm <- relationships(
  paths(from = "Quality",        to = c("Value"))
  #paths(from = "Expectation",  to = c("Loyalty"))
)

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)
seminr_theme_set(seminr_theme_smartpls())
mobi_pls %>% dot_graph() %>% grViz()
