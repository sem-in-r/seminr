
<!-- README.md is generated from README.Rmd. Please edit that file -->
SEMinR
======

SEMinR brings many advancements to creating and estimating structural equation models (SEM) using Partial Least Squares Path Modeling (PLS-PM):

-   A *natural* feeling, *domain-specific* language to build and estimate structural equation models in R
-   Uses *variance-based PLS estimation* to model both *composite* and *common-factor* constructs
-   *High-level functions* to quickly specify interactions and complicated structural models

SEMinR follows the latest best-practices in methodological literature:

-   Automatically *adjusts PLS estimates to ensure consistency (PLSc)* wherever common factors are involved
-   Ajusts for known biases in interaction terms in PLS models
-   Continuously tested against leading PLSPM software to ensure parity of outcomes: SmartPLS (Ringle et al., 2015) and ADANCO (Henseler and Dijkstra, 2015), as well as other R packages such as semPLS (Monecke and Leisch, 2012) and matrixpls (Rönkkö, 2016)
-   *High performance, multi-core* bootstrapping function

Documentation
-------------

The vignette for Seminr can be found in the [seminr/inst/doc/](https://github.com/ISS-Analytics/seminr/blob/master/inst/doc/SEMinR.html) folder or by running the `vignette("SEMinR")` command after installation.

Demo code for use of Seminr can be found in the [seminr/demo/](https://github.com/ISS-Analytics/seminr/tree/master/demo) folder or by running the `demo("seminr-contained")`, `demo("seminr-ecsi")` or `demo("seminr-interaction")` commands after installation.

Installation
------------

You can install SEMinR with:

``` r
install.packages("seminr")
```

Usage
-----

Briefly, there are four steps to specifying and estimating a structural equation model using SEMinR:

1.  Describe measurement model for each construct and its items:

``` r
# Distinguish and mix composite or reflective (common-factor) measurement models
measurements <- constructs(
  composite("Image",       multi_items("IMAG", 1:5), weights = mode_B),
  composite("Expectation", multi_items("CUEX", 1:3), weights = mode_A),
  reflective("Loyalty",    multi_items("CUSL", 1:3))
)
```

1.  Specify any interactions between constructs:

``` r
# Easily create orthogonalized or scaled interactions between constructs
intxns <- interactions(
  interaction_ortho("Image", "Expectation")
)
```

1.  Describe the structural model of causal relationships between constructs (and interactions):

``` r
# Quickly create multiple paths "from" and "to" sets of constructs
structure <- relationships(
  paths(from = c("Image", "Expectation", "Image.Expectation"), 
        to = "Loyalty")
)
```

1.  Put the above elements together to estimate and bootstrap the model:

``` r
# Dynamically compose SEM models from individual parts
pls_model <- estimate_pls(data = mobi, measurements, intxns, structure)
summary(pls_model)

# Use multi-core parallel processing to speed up bootstraps
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
summary(boot_estimates)
```

Authors
-------

-   Soumya Ray
-   Nicholas Danks
