
<!-- README.md is generated from README.Rmd. Please edit that file -->
SEMinR
======

The `seminr` package provides a natural syntax for researchers to describe PLS structural equation models.

Documentation
-------------

The vignette for Seminr can be found in the [seminr/inst/doc/](https://github.com/ISS-Analytics/seminr/blob/master/inst/doc/SEMinR.html) folder or by running the `vignette("SEMinR")` command after installation.

Demo code for use of Seminr can be found in the [seminr/demo/](https://github.com/ISS-Analytics/seminr/tree/master/demo) folder or by running the `demo("seminr-contained")`, `demo("seminr-ecsi")` or `demo("seminr-interaction")` commands after installation.

Installation
------------

You can install seminr from github with:

``` r
# install.packages("devtools")
devtools::install_github("ISS-Analytics/seminr")
```

Usage
-----

Seminr can be used to create a plsm model, to estimate the model and to perform bootstrapping.

``` r
# seminr syntax for creating measurement model
mobi_mm <- measure(
  reflect("Image",        multi_items("IMAG", 1:5)),
  reflect("Expectation",  multi_items("CUEX", 1:3)),
  reflect("Value",        multi_items("PERV", 1:2)),
  reflect("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interact(
  interaction_combo("Image", "Expectation"),
  interaction_combo("Image", "Value")
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- structure(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)

# Load data, assemble model, and estimate using semPLS
data("mobi", package = "semPLS")
seminr_model <- create_model(data = mobi,
                             measurement_model = mobi_mm,
                             interaction = mobi_xm,
                             structural_model = mobi_sm)
#> Generating the plsm model

mobi_pls <- estimate_model(seminr_model, nboot = 200)
#> Estimating model using semPLS::sempls...
print_paths(mobi_pls)
#>                   Satisfaction
#> R^2                       0.60
#> Expectation               0.47
#> Image                     0.80
#> Image.Expectation        -0.52
#> Image.Value              -0.16
#> Value                     0.43
#>                                   Estimate Bootstrapped Estimate
#> Expectation -> Satisfaction           0.47                  0.40
#> Image -> Satisfaction                 0.80                  0.77
#> Image.Expectation -> Satisfaction    -0.52                 -0.40
#> Image.Value -> Satisfaction          -0.16                 -0.25
#> Value -> Satisfaction                 0.43                  0.51
#>                                   Standard Error
#> Expectation -> Satisfaction                 0.31
#> Image -> Satisfaction                       0.19
#> Image.Expectation -> Satisfaction           0.51
#> Image.Value -> Satisfaction                 0.47
#> Value -> Satisfaction                       0.33
```

Testing
-------

To test:

``` r
require(devtools)
devtools::test()
```
