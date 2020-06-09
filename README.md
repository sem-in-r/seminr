
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="vignettes/SEMinR_logo.jpg" width="25%" />

![Build Status](https://travis-ci.org/sem-in-r/seminr.svg?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/seminr)](https://cran.r-project.org/package=seminr)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/seminr)](https://cran.r-project.org/package=seminr)

SEMinR allows users to easily create and modify structural equation
models (SEM):

  - A *natural* feeling, *domain-specific* language to build and
    estimate structural equation models in R
  - *High-level functions* to quickly specify interactions and
    complicated structural models
  - *Modular design* of models that promotes reuse of model components
  - Encourages *best practices* by use of smart defaults and warnings

SEMinR allows various estimation methods for constructs and SEMs:

  - Covariance-based Structural Equation Modeling (CBSEM)
      - *Covariance-based estimation* of SEM using the popular
        [Lavaan](https://github.com/yrosseel/lavaan) package
      - Easily *specify interactions* between constructs: e.g.,
        \`IMAGE\*
      - Adds ten Berge *factor score extraction*
      - Adds *VIF* and other validity assessments
  - Confirmatory Factor Analysis (CFA) using Lavaan
      - Uses [Lavaan](https://github.com/yrosseel/lavaan) package and
        returns syntax
      - Adds ten Berge *factor score extraction*
  - Partial Least Squares Path Modeling (PLS-PM)
      - Uses non-parametric *variance-based estimation* to construct
        *composites* and *common-factors*
      - Automatically *adjusts PLS estimates to ensure consistency
        (PLSc)* when emulating reflective common factors
      - Adjusts for known biases in interaction terms in PLS models
      - Continuously tested against leading PLSPM software to ensure
        parity of outcomes: SmartPLS (Ringle et al., 2015), ADANCO
        (Henseler and Dijkstra, 2015), semPLS (Monecke and Leisch, 2012)
        and matrixpls (Rönkkö, 2016)
      - *High performance, multi-core* bootstrapping function

This means that researchers can create an SEM and reuse it under
different estimation techniques (PLS-PM, CBSEM).

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

**1. Describe measurement model for each construct, interaction, or
higher order
construct:**

``` r
# Distinguish and mix composite or reflective (common-factor) measurement models
# - composite measurements will have to be converted into reflective ones for CBSEM (see below)
measurements <- constructs(
  composite("Image",       multi_items("IMAG", 1:5), weights = mode_B),
  composite("Expectation", multi_items("CUEX", 1:3), weights = mode_A),
  reflective("Loyalty",    multi_items("CUSL", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  interaction_term(iv = "Image", moderator = "Expectation", method = orthogonal)
)
```

**2. Describe the structural model of causal relationships between
constructs (and interactions):**

``` r
# Quickly create multiple paths "from" and "to" sets of constructs
structure <- relationships(
  paths(from = c("Image", "Expectation", "Image*Expectation", "Complaints"), 
        to = "Loyalty")
)
```

**3. Put the above elements together to estimate and bootstrap the
model:**

PLS Path Model Estimation:

``` r
# Dynamically compose SEM models from individual parts
pls_model <- estimate_pls(data = mobi, measurements, structure)
summary(pls_model)

# Use multi-core parallel processing to speed up bootstraps
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
summary(boot_estimates)
```

Covariance-based SEM Estimation using Lavaan:

``` r
# Dynamically compose SEM models from individual parts
# - measurement model includes composites, that we will convert into reflective common factors
cbsem_mdoel <- estimate_pls(data = mobi, as.reflective(measurements), structure)
summary(cbsem_model)
```

## Authors

  - Soumya Ray
  - Nicholas Danks

## Contributors

  - James Uanhoro (ten Berge factor extraction, advice on
    covariance-based methods)
  - Arturo Heynar Cano Bejar (evaluation and testing of PLS and CBSEM
    models)
