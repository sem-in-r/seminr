# Structural functions
paths <- function(from, to) {
  return(as.vector(t(as.matrix(expand.grid(from, to)))))
}

structure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}
