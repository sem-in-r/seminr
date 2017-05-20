# This example recreates the ECSI model on mobile users found at:
#  https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf

# Shows an example of contained model style
library(seminr)
# Contained Style: all model specifications are put together and estimated using
# simplePLS.

data("mobi", package = "semPLS")

mobi_pls <- estimate_model(
    data = mobi,

    measure(
      reflect("Image", multi_items("IMAG", 1:5)),
      reflect("Expectation", multi_items("CUEX", 1:3)),
      reflect("Value", multi_items("PERV", 1:2)),
      reflect("Satisfaction", multi_items("CUSA", 1:3))
    ),

    interact(
      interaction_ortho("Image", "Expectation"),
      interaction_ortho("Image", "Value")
    ),

    structure(
      paths(to = "Satisfaction",
            from = c("Image", "Expectation", "Value",
                     "Image.Expectation", "Image.Value"))
    )
  )
)

print_paths(mobi_pls)

mobi_pls <- bootstrap_model(
  data = mobi,

  measure(
    reflect("Image", multi_items("IMAG", 1:5)),
    reflect("Expectation", multi_items("CUEX", 1:3)),
    reflect("Value", multi_items("PERV", 1:2)),
    reflect("Satisfaction", multi_items("CUSA", 1:3))
  ),

  interact(
    interaction_ortho("Image", "Expectation"),
    interaction_ortho("Image", "Value")
  ),

  structure(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation", "Value",
                   "Image.Expectation", "Image.Value"))
  ),
  nboot = 500
)
print_paths(mobi_pls)

