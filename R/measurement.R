# FUNCTIONS
# Measurement functions
measure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}

reflect <- function(construct_name, item_names) {
  construct_names <- rep(construct_name, length(item_names))
  return(c(rbind(construct_names, item_names)))
}

form <- function(construct_name, item_names) {
  construct_names <- rep(construct_name, length(item_names))
  return(c(rbind(item_names, construct_names)))
}

# Creates list of measurement items using root name, numbers, and affixes
#
# arguments:
#   item_name: root name of all items
#   item_numbers: vector of item numbers
#   ...: optional affix arguments
#     prefix: prefix before each item name
#     mid: insert between item name and numbers
#     suffix: suffix after each ite name
#
# e.g.> multi_items("item", 0:3, prefix="X_", mid=".", suffix="_")
#
multi_items <- function(item_name, item_numbers, ...) {
  affix <- as.data.frame(list(...))
  paste(affix$prefix, item_name, affix$mid, item_numbers, affix$suffix, sep = "")
}

single_item <- function(item) {
  return(item)
}
