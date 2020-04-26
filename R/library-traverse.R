#' Traverses tree structures of nested lists to create
#' string representation of structure. Useful to compare list structures
#' in test cases. (see cbsem summary tests for example)
#' @export
traverse_names <- function(list_tree, level = 0)  {
  if (!("list" %in% class(list_tree))) return("")

  branches = sort(names(list_tree))
  return_str <- ""
  for (branch in branches) {
    sub_branches = traverse_names(get(branch, list_tree), level + 1)
    tabs = paste(rep("  ", level), collapse="")
    return_str <- paste(return_str, tabs, "-", branch, "\n", sub_branches, sep="")
  }
  return(return_str)
}
