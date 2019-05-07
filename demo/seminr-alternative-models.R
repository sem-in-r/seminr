# Alternative Models: Demonstration of how to reuse measurement model in different structural models
library(seminr)

# Creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Organize constructs into vectors by name
antecedents <- c("Image", "Expectation", "Quality")
mediators <- c("Satisfaction", "Value")
outcomes <- c("Loyalty", "Complaints")

# Created multiple structural models
mediated_sm <- relationships(
  paths(from = antecedents, to = mediators),
  paths(from = mediators, to = outcomes)
)

non_mediated_sm <- relationships(
  paths(from = antecedents, to = outcomes)
)

overriding_effects_sm <- relationships(
  paths(from = antecedents, to = mediators),
  paths(from = c(antecedents, mediators), to = outcomes)
)

# Estimating the different structural models from same measurement model
mediated_model <- estimate_pls(data = mobi,
                               measurement_model = mobi_mm,
                               structural_model = mediated_sm)

non_mediated_model <- estimate_pls(data = mobi,
                                   measurement_model = mobi_mm,
                                   structural_model = non_mediated_sm)

overriding_effects_model <- estimate_pls(data = mobi,
                                         measurement_model = mobi_mm,
                                         structural_model = overriding_effects_sm)

# Reporting the results of different models
summary(mediated_model)
summary(non_mediated_model)
summary(overriding_effects_model)

