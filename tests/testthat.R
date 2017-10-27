library(testthat)
library(seminr)
Sys.unsetenv("R_TESTS")
test_check("seminr")
