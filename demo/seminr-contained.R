# Shows an example of contained model style
library(seminr)
# Contained Style: all model specifications are put together

data("mobi", package = "semPLS")

mobi_pls <- seminr::estimate_model(
  seminr::create_model(
    data = mobi,

    measure(
      reflect("Image", multi_items("IMAG", 1:5)),
      reflect("Expectation", multi_items("CUEX", 1:3)),
      reflect("Value", multi_items("PERV", 1:2)),
      reflect("Satisfaction", multi_items("CUSA", 1:3))
    ),

    interact(
      interaction_combo("Image", "Expectation"),
      interaction_combo("Image", "Value")
    ),

    structure(
      paths(to = "Satisfaction",
            from = c("Image", "Expectation", "Value",
                     "Image.Expectation", "Image.Value"))
    )
  )
)

print_paths(mobi_pls)
