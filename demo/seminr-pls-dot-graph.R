library(seminr)

# Load the Data
mobi <- mobi

# SEMinR syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2))
)

# SEMinR syntax for creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation")),
  paths(from = "Expectation",  to = c("Value"))
)

# Estimate the model
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

# Plot measurement model
plot(mobi_mm)

# Plot structural model
plot(mobi_sm)

# Plot PLS model
plot(mobi_pls)

# Bootstrap the model
mobi_boot <- bootstrap_model(mobi_pls)

# Plot bootstrapped PLS model
plot(mobi_boot)

# Plot bootstrapped PLS model and add the T-Value for structural paths
thm <- seminr_theme_get()
thm$sm.edge.boot.show_t_value <- TRUE
seminr_theme_set(thm)

# Replot the bootstrapped model
plot(mobi_boot)
