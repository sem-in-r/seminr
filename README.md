
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="vignettes/SEMinR_logo.jpg" width="25%" />

![Build Status](https://travis-ci.org/sem-in-r/seminr.svg?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/seminr)](https://cran.r-project.org/package=seminr)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/seminr)](https://cran.r-project.org/package=seminr)

SEMinR brings many advancements to creating and estimating structural
equation models (SEM) using Partial Least Squares Path Modeling
(PLS-PM):

  - A *natural* feeling, *domain-specific* language to build and
    estimate structural equation models in R
  - Uses *variance-based PLS estimation* to model both *composite* and
    *common-factor* constructs
  - *High-level functions* to quickly specify interactions and
    complicated structural models

SEMinR follows the latest best-practices in methodological literature:

  - Automatically *adjusts PLS estimates to ensure consistency (PLSc)*
    wherever common factors are involved
  - Ajusts for known biases in interaction terms in PLS models
  - Continuously tested against leading PLSPM software to ensure parity
    of outcomes: SmartPLS (Ringle et al., 2015) and ADANCO (Henseler and
    Dijkstra, 2015), as well as other R packages such as semPLS (Monecke
    and Leisch, 2012) and matrixpls (Rönkkö, 2016)
  - *High performance, multi-core* bootstrapping function

## Documentation

The vignette for Seminr can be found in the
[CRAN](https://cran.r-project.org/package=seminr/vignettes/SEMinR.html)
folder or by running the `vignette("SEMinR")` command after
installation.

Demo code for use of Seminr can be found in the
[seminr/demo/](https://github.com/sem-in-r/seminr/tree/master/demo)
folder or by running the `demo("seminr-contained")`,
`demo("seminr-ecsi")` or `demo("seminr-interaction")` commands after
installation.

## Installation

You can install SEMinR with:

``` r
install.packages("seminr")
```

## Usage

Briefly, there are four steps to specifying and estimating a structural
equation model using SEMinR:

1 Describe measurement model for each construct and its items including
any interactions or higher order
constructs:

``` r
# Distinguish and mix composite or reflective (common-factor) measurement models
measurements <- constructs(
  composite("Image",       multi_items("IMAG", 1:5), weights = mode_B),
  composite("Expectation", multi_items("CUEX", 1:3), weights = mode_A),
  reflective("Loyalty",    multi_items("CUSL", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Complaints",   single_item("CUSCO")),
  interaction_term(iv = "Image", moderator = "Expectation", method = orthogonal),
  interaction_term(iv = "Image", moderator = "Value", method = orthogonal),
  higher_composite("Value", dimensions = c("Quality","Complaints"), method = two_stage, weights = mode_B)
)
```

2 Describe the structural model of causal relationships between
constructs (and interactions):

``` r
# Quickly create multiple paths "from" and "to" sets of constructs
structure <- relationships(
  paths(from = c("Image", "Expectation", "Image*Expectation","Image*Value"), 
        to = "Loyalty")
)
```

3 Put the above elements together to estimate and bootstrap the model:

``` r
# Dynamically compose SEM models from individual parts
pls_model <- estimate_pls(data = mobi, measurements, structure)
summary(pls_model)

# Use multi-core parallel processing to speed up bootstraps
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
summary(boot_estimates)
```

## Authors

  - Soumya Ray
  - Nicholas Danks
