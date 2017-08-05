# This example recreates the ECSI model on mobile users found at:
#  https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf

# Shows an example of contained model style
library(seminr)
# Contained Style: all model specifications are put together and estimated using
# simplePLS.

mobi <- mobi

mobi_pls <- estimate_pls(
  data = mobi,

  constructs(
    reflective("Image", multi_items("IMAG", 1:5)),
    reflective("Expectation", multi_items("CUEX", 1:3)),
    reflective("Value", multi_items("PERV", 1:2)),
    reflective("Satisfaction", multi_items("CUSA", 1:3))
  ),

  interactions(
    interaction_ortho("Image", "Expectation"),
    interaction_ortho("Image", "Value")
  ),

  relationships(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation", "Value",
                   "Image.Expectation", "Image.Value"))
  )
)

print_paths(mobi_pls)

mobi_pls <- bootstrap_model(
  seminr_model = mobi_pls,
  nboot = 500
)
print_paths(mobi_pls)

