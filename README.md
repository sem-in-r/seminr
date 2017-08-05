
<!-- README.md is generated from README.Rmd. Please edit that file -->
SEMinR
======

SEMinR provides a natural syntax for researchers to describe PLS structural equation models.

-   [Goals of SEMinR](#goals-of-seminr)
-   [Installation](#installation)
-   [Usage Example](#usage-example)
-   [Measurement Models](#measurement-models)
-   [Documentation](#documentation)
-   [Contributing](#contributing)

Goals of SEMinR \[goals\]
-------------------------

1.  Powerful syntax for model definition:
    -   Easily create complex measurement and structural models
    -   Interaction factors created automatically: choose from orthogonalized, scaled, etc.

2.  Advanced estimation methods:
    -   Latest estimation advances: consistent PLS, adjustments for interactions, etc.
    -   Fast bootstrapping: multicore parallel processing bootstrap

3.  Free and open-source research test-bed:
    -   Reference implementation for PLS estimation methods
    -   Contribute to or experiment with under-the-hood implementation

Installation
------------

You can install SEMinR from its Github repo with:

``` r
# First, install `devtools` once so that you can install SEMinR using it
install.packages("devtools")

# Now `devtools` can install SEMinR from its Github repo
library(devtools)
devtools::install_github("sem-in-r/seminr")
```

Usage Example
-------------

Consider the following starter example that uses data from the ECSI dataset.

Define your constructs and their measurement mode (see details in \[measurement\_model\]\[\]):

``` r
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  composite( "Satisfaction", multi_items("CUSA", 1:3))
)
```

Interaction factors must be created after the measurement model is defined:

``` r
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)
```

Define structural model (note the default names of interaction factors):

``` r
mobi_sm <- structure(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)
```

Load data frame (could also use `read.csv()`, `read.table()`, etc.):

``` r
data("mobi", package = "semPLS")
```

Assemble model, and estimate using built-in simplePLS algorithm:

``` r
seminr_model <- estimate_pls(data = mobi,
                             measurement_model = mobi_mm,
                             interactions = mobi_xm,
                             structural_model = mobi_sm)
```

Show reports and figures (more coming soon):

``` r
print_paths(mobi_pls)
plot_scores(mobi_pls)
```

Bootstrap the estimated model:

``` r
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

print_paths(boot_mobi_pls)
plot_scores(boot_mobi_pls)
```

Measurement Models
------------------

Constructs can be modeled in three ways for PLS estimation:

1.  Reflective constructs (PLS consistent, mode A)

    ``` r
      reflective("Expectation",  multi_items("CUEX", 1:3))
    ```

2.  Composites with *correlation* weights (PLS, mode A)

    ``` r
    composite("Expectation",  multi_items("CUEX", 1:3), weights="correlation")
    # or
    composite("Expectation",  multi_items("CUEX", 1:3), weights="A")
    ```

3.  Composites with *regression* weights (PLS, mode B)

    ``` r
    composite("Expectation",  multi_items("CUEX", 1:3), weights="regression")
    # or
    composite("Expectation",  multi_items("CUEX", 1:3), weights="B")
    ```

Documentation
-------------

The vignette for Seminr can be found in the [seminr/inst/doc/](https://github.com/sem-in-r/seminr/blob/master/inst/doc/SEMinR.html) folder or by running the `vignette("SEMinR")` command after installation.

Demo code for use of Seminr can be found in the [seminr/demo/](https://github.com/sem-in-r/seminr/tree/master/demo) folder or by running the `demo("seminr-contained")`, `demo("seminr-ecsi")` or `demo("seminr-interaction")` commands after installation.

Contributing
------------

-   Fork this repo to your own Github profile
-   Create a new branch and work in it
-   Test your code:

``` r
require(devtools)
devtools::test()
```

-   Push your branch to your own forked repo
-   Issue a PR for your new branch to this upstream repo
