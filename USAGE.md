---
title: "Different Use Cases of Modelr Syntax"
output: html_document
---

This document highlights the diffent types of `modelr` syntax for particular use cases in measurement and structural definitions.

## Measurement

### Measurement mode: reflective vs. formative

You can specify formative or reflective factors:
``` r
simple_mm <- measure(
  reflect("Expectation", multi_items("CUEX", 1:3)),
  form("Quality", multi_items("PERQ", 1:7))
)
```

### Measurement items: multiple vs. single

You may specify multiple items or a single item to measure a construct. `multi_items(...)` specifies multiple items, and a single item can simply be named.

``` r
simple_mm <- measure(
  reflect("Quality", multi_items("PERQ", 1:7)),         # seven-item measure
  reflect("Satisfaction", multi_items("CUSA", c(1,3))), # two-item measure
  reflect("Expectation", "CUEX1")                       # single-item measure
)
```

## Structure

Notice there are multiple ways to specify the same strucural model.

### Structural style:

There are many ways to specify the same structure, depending on how you perceive the model. The following example shows three ways to specify the same structural model.

*Output Structure Style*:
``` r
mobi_sm <- structure(
  paths(from = "Image", to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation", to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality", to = c("Value", "Satisfaction")),
  paths(from = "Value", to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints", to = "Loyalty")
)
```

*Regression Structure Style*:
``` r
mobi_sm2 <- structure(
  paths(to = "Expectation", from = "Image"),
  paths(to = "Satisfaction", from = c("Image", "Expectation", "Quality", "Value")),
  paths(to = "Loyalty", from = c("Image", "Satisfaction", "Complaints")),
  paths(to = "Quality", from = "Image"),
  paths(to = "Value", from = c("Expectation", "Quality")),
  paths(to = "Complaints", from = "Satisfaction")
)
```

*Mixed Structure Style*:
``` r
mobi_sm3 <- structure(
  paths(from = "Image", to = "Expectation"),
  paths(from = "Expectation", to = "Quality"),
  paths(from = c("Expectation", "Quality"), to = c("Satisfaction", "Value")),
  paths(from = c("Image", "Value"), to = "Satisfaction"),
  paths(from = c("Image", "Satisfaction", "Complaints"), to = "Loyalty"),
  paths(from = "Satisfaction", to = "Complaints")
)
```

