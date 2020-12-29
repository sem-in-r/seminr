library(seminr)
library(DiagrammeR)

mobi <- mobi

#seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2))
)
#seminr syntax for creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation")),
  paths(from = "Expectation",  to = c("Value"))
)

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)
seminr_theme_set(seminr_theme_smartpls())

# Plot Measurement model
mobi_mm %>% dot_graph() %>% grViz()
# plot structural model
mobi_sm %>% dot_graph() %>% grViz()

# Plot PLS model
mobi_pls %>% dot_graph() %>% grViz()

mobi_boot <- bootstrap_model(mobi_pls)

# Plot bootstrapped PLS model
mobi_boot %>% dot_graph() %>% grViz()

# Plot bootstrapped PLS model and show CI
thm <- seminr_theme_get()
thm$sm.edge.boot.show_ci <- TRUE
seminr_theme_set(thm)
mobi_boot %>% dot_graph() %>% grViz()

