library(seminr)
library(DiagrammeR)

mobi <- mobi

#seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3))
)
#seminr syntax for creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation"))
)

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)
seminr_theme_set(seminr_theme_smartpls())
mobi_pls %>% dot_graph() %>% grViz()
