# This file does basic package loading things

.onLoad <- function(...) {

  # set the current theme to default
  seminr_global$theme_current <- create_theme()

}


