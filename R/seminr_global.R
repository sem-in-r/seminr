# Environment that holds various global variables and settings for seminr,
# such as the current theme for plotting. It is not exported and should not be directly
# manipulated by other packages.
seminr_global <- new.env(parent = emptyenv())


# The current theme. Defined here only as placeholder, and defined properly
# in file "theme-current.R". This setup avoids circular dependencies among
# the various source files.
seminr_global$theme_current <- list()
seminr_global$seminr_theme_current <- list()
