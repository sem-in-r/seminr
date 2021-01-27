# Utilities ----

glue_dot <- function(x) {
  glue::glue(x, .open = "<<", .close = ">>", .envir = parent.frame())
}

#' Wrap a text in single quotes
#'
#' @param x a character string
esc_node <- function(x){
  paste0("\"", x ,"\"")
}



#' Format p values for the output and removes trailing numbers when p > .10
#' @keywords internal
#' @param pvals A vector with p-values
#' @param sig.limit The lowest threshold for full reporting
#' @param digits the amount of digits to report when sig.limit < p < .10
#' @param html whether to use html coded output
#'
#' @return A string formatted p-value including equal and less-than sign
# @export
#'
# @examples
#' pvalr(c(0.432, 0.05, 0.00001))
pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {

  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x) #generate sprintf string
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    paste0("= ",res)
  }

  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2))
    else
      return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}


#' Format p values for the output and removes trailing numbers when p > .10
#' @keywords internal
#' @param pvals A vector with p-values
#' @param sig.limit The thresholds for adding an additional asterisk
#' @param html whether to use html coded output
#'
#' @return A string of asterisks
# @export
#'
# @examples
#' pvalr(c(0.432, 0.05, 0.00001))
psignr <- function(pvals, sig.limit = c(0.05, 0.01, 0.001), html = FALSE){
  sapply(pvals, function(x, sig.limit){
    res <- ""
    if (is.na(x)) {
      return("")
    }
    if (html) {
      for (i in 1:length(sig.limit)) {
        if (x < sig.limit[i]) {
          res <- paste0(res, "*")
        }
      }
      return(res)
    } else {
      for (i in 1:length(sig.limit)) {
        if (x < sig.limit[i]) {
          res <- paste0(res, "*")
        }
      }
      return(res)
    }

  }, sig.limit = sig.limit)
}


#' Open Edotor graphViz Website with the preloaded in the Browser
#'
#' @param model A SEMinR Model
#'
#' @export
#'
#' @examples
#' \dontrun{
#' browse_plot(model)
#' }
browse_plot <- function(model){
  if (!interactive()) {
    stop("This only works in interactive environments")
  }


  utils::browseURL(
    utils::URLencode(paste0("http://edotor.net/#", dot_graph(model)))
    )
}

