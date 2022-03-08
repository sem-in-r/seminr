# This file helps with setting and getting the current theme
# The environment for this is created in theme_current.R.
# This helps against circular dependencies.


#' Get and set the active theme
#'
#' The current/active theme (see [seminr_theme()]) is automatically applied to every
#' graph you draw. Use `seminr_theme_get()` to get the current theme, and `seminr_theme_set()` to
#' completely override it.
#' @export
seminr_theme_get <- function() {
  seminr_global$theme_current
}

#' @rdname seminr_theme_get
#' @param new new theme (a list of theme elements)
#' @export
seminr_theme_set <- function(new) {
  old <- seminr_global$theme_current
  seminr_global$theme_current <- new
  invisible(old)
}


