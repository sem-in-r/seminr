# Demonstration of concise, contained model style

library(seminr)

mobi_pls <- estimate_pls(
  data = mobi,

  constructs(
    composite("Image", multi_items("IMAG", 1:5)),
    composite("Expectation", multi_items("CUEX", 1:3)),
    composite("Value", multi_items("PERV", 1:2)),
    composite("Satisfaction", multi_items("CUSA", 1:3)),
    interaction_term("Image*Expectation", dimensions = c("Image","Expectation"), method = orthogonal),
    interaction_term("Image*Value", dimensions = c("Image","Value"), method = orthogonal)
  ),

  relationships(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation", "Value",
                   "Image*Expectation", "Image*Value"))
  )
)

summary(mobi_pls)

# Bootstrapping the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls, nboot = 1000)
summary(boot_mobi_pls)

