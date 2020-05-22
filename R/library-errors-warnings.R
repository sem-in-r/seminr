#' expr: expression to try
#' context_msg: context of warning or error (e.g., "run CFA in Lavaan")
#'              use a verb to start the message (e.g., "run...", "execute...")
try_or_stop <- function(expr, context_msg=NULL) {
  result <- tryCatch(
    list(
      value = expr,
      success = TRUE,
      message = NULL
    ),
    error=function(e) {
      list(success = FALSE, message=e$message)
    }, 
    warning=function(w) {
      list(success = FALSE, message=w$message)
    }
  )

  if (!result$success) {
    stop(paste("Could not ", context_msg, ":\n", result$message, sep=""))
  }
  result$value
}