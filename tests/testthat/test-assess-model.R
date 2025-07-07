context("SEMinR correctly identifies the model syntax errors\n")

## misspelt construct in mm
set.seed(123)

mobi_mm <- constructs(
  composite("Imag",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

expect_error(assess_model_specification(mobi_mm,
                                    mobi_sm,
                                    mobi),
             regexp = "names of your constructs")

## misspelt construct in sm
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectatin", "Value"))
)
expect_error(assess_model_specification(mobi_mm,
                                    mobi_sm,
                                    mobi),
             regexp = "names of your constructs")

## missing construct in interaction
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Quality",      multi_items("PERQ", 1:3)),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A),
  interaction_term("Image", "Expectation")
)

mobi_sm <- relationships(
  paths(to = c("Quality"),
        from = c("Image","Expectation", "Image*Expectation","Value")),
  paths(to = c("Satisfaction"),
        from = c("Image","Image*Expectation","Value"))
)

expect_error(assess_model_specification(measurement_model = mobi_mm,
                                    structural_model = mobi_sm,
                                    mobi),
             "IV and MV")

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:6), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)
expect_error(assess_model_specification(mobi_mm,
                                    mobi_sm,
                                    mobi),
             regexp = "colnames")
