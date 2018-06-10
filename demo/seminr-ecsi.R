# Simple Style: Seperate declaration of measurement and structural model, no interactions. Estimated
# using simplePLS.
library(seminr)

# Creating measurement mode
# - note: composite() has a default parameter setting of mode A
# - note: items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#         which can be constructed quickly as: multi_items("CUEX", 1:3)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

# Estimating the model
# - note, the mobi dataset is bundled with seminr
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

# Reporting the results
summary(mobi_pls)
plot_scores(mobi_pls)

# Getting more detailed output from estimated model:
loadings <- mobi_pls$outer_loadings

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls, nboot = 1000)

summary(boot_mobi_pls)
