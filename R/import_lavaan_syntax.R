# lav_syntax <- '
#   # Structural model
#   EXPE ~ IMAG
#   QUAL ~ EXPE
#   VAL  ~ EXPE + QUAL
#   SAT  ~ IMAG + EXPE + QUAL + VAL
#   LOY  ~ IMAG + SAT
#
#   # Composite model
#   IMAG <~ imag1 + imag2 + imag3
#   EXPE <~ expe1 + expe2 + expe3
#   QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5
#   VAL  <~ val1  + val2  + val3
#
#   # Reflective measurement model
#   SAT  =~ sat1  + sat2  + sat3  + sat4
#   LOY  =~ loy1  + loy2  + loy3  + loy4
# '

lavaan2seminr <- csem2seminr <- function(lav_syntax) {
  lav_model <- lavaan::lavaanify(model = lav_syntax)

  lav_relationships <- subset(lav_model, op == "~")
  lav_composites    <- subset(lav_model, op == "<~")
  lav_reflectives   <- subset(lav_model, op == "=~")

  composites  <- lav_constructs(lav_composites, csem_composite)
  reflectives <- lav_constructs(lav_reflectives, reflective)
  structural  <- lav_paths(lav_relationships)

  seminr_constructs <- do.call(constructs, c(composites, reflectives))
  seminr_relationships <- do.call(relationships, structural)

  specify_model(measurement_model = seminr_constructs,
                structural_model  = seminr_relationships)
}

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

lav_composite <- function(lav_constructs, constr) {
  composite(
    construct_name = constr,
    item_names     = subset(lav_constructs, lhs==constr)
  )
}

lav_paths <- function(lav_relationships) {
  lapply(as.data.frame(t(lav_relationships)), lav_path)
}

lav_path <- function(lav_relationship) {
  paths(from = lav_relationship["rhs"],
        to   = lav_relationship["lhs"])
}
