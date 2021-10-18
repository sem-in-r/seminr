### Accompanying Code for:
## Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R - A Workbook (2021)
## Hair, J.F. (Jr), Hult, T.M., Ringle, C.M., Sarstedt, M., Danks, N.P., and Ray, S.

## Chapter 2: Introduction to R and R Studio

# Create a vector of integers
vector <- c(1, 2, 3, 4, 5)

# Install the Swirl package
install.packages(pkgs = "swirl")

# Load the Swirl library into the environment
library(swirl)

# Begin learning with Swirl
swirl()

# Searching for help using the ? operator
?read.csv

# Check all vignettes available in R
vignette()

# Load the SEMinR vignette
vignette("SEMinR")

# Check all demos available in R
demo()

# Load the SEMinR ECSI demo
demo("seminr-pls-ecsi")
