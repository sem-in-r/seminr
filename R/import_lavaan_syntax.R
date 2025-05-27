#' seminr csem2seminr() function
#'
#' Converts lavaan syntax for composite models used by \code{cSEM} package to \code{SEMinR} model specifications
#'
#' @usage
#' csem2seminr(lav_syntax)
#'
#' @seealso \code{\link{estimate_pls}}
#'
#' @param lav_syntax A \code{string} specifying the composite model measurement and structure using lavaan syntax
#'
#' @return A SEMinR model.
#'
#' @examples
#' lav_syntax <- '
#'   # Composite model
#'   Image <~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
#'   Expectation <~ CUEX1 + CUEX2 + CUEX3
#'   Value  <~ PERV1  + PERV2
#'   Satisfaction <~ CUSA1 + CUSA2 + CUSA3
#'
#'   # Structural model
#'   Satisfaction ~ Image + Expectation + Value
#' '
#'
#' csem_model <- estimate_pls(mobi, model = csem2seminr(lav_syntax))
#'
#' @export
csem2seminr <- function(lav_syntax) {
  message("NOTE: Importing lavaan syntax is currently experimental -- some features might not be supported\n",
          "      Please verify imported model features carefully\n")

  lav_model <- lavaan::lavaanify(model = lav_syntax)

  lav_relationships <- subset(lav_model, lav_model$op == "~")
  lav_composites    <- subset(lav_model, lav_model$op == "<~")
  lav_reflectives   <- subset(lav_model, lav_model$op == "=~")

  composites  <- lav_constructs(lav_composites, csem_composite)
  reflectives <- lav_constructs(lav_reflectives, reflective)
  structural  <- lav_paths(lav_relationships)

  seminr_constructs <- do.call(constructs, c(composites, reflectives))
  seminr_relationships <- do.call(relationships, structural)

  specify_model(measurement_model = seminr_constructs,
                structural_model  = seminr_relationships)
}

# Temporary alias to implement lavaan2seminr
# WARNING: does not parse all lavaan syntax correctly
# Works: structural model works reasonably
# Does not work: parameter constraints; item associations
lavaan2seminr <- csem2seminr

lav_constructs <- function(lav_constructs, construct_func) {
  all_constructs <- unique(lav_constructs$lhs)
  all_items <- lapply(all_constructs, lav_items, lav_constructs)

  mapply(lav_construct, all_constructs, all_items,
         MoreArgs = list(construct_func = construct_func), SIMPLIFY = FALSE)
}

lav_construct <- function(construct_name, item_names, construct_func) {
  construct_func(construct_name, item_names)
}

csem_composite <- function(construct_name, item_names) {
  composite(construct_name, item_names, weights = mode_B)
}

lav_items <- function(constr, lav_constructs) {
  lav_constructs[lav_constructs$lhs == constr,]$rhs
}

# convert lavaan construct row
lav_composite <- function(lav_constructs, constr) {
  composite(
    construct_name = constr,
    item_names     = subset(lav_constructs, lav_constructs$lhs==constr)
  )
}

# converts rows of lavaan relationships into seminr paths
lav_paths <- function(lav_relationships) {
  lapply(as.data.frame(t(lav_relationships[c("rhs", "lhs")])), lav_path)
}

# converts vector of two elements into a path
lav_path <- function(lav_relationship) {
  paths(from = lav_relationship[1],
        to   = lav_relationship[2])
}
