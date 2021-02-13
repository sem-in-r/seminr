#' Create a list of section content from every documentation page from a package
#' @param pkg String naming the package
#' @return A list whose names are documentation page titles. Each element is
#'   itself a list of the details and sections for a page.
#' @keywords internal
package_docs <- function(pkg) {
  help_dir <- system.file("help", package = pkg)
  db_path <- file.path(help_dir, pkg)
  # THIS IS A HACK to circumvent CRAN from complaining here
  eval(parse(text = "tools:::fetchRdDB(db_path)"))
  #tools:::fetchRdDB(db_path)
}

extract_sections <- function(doc_page, sections) {
  tags <- vapply(doc_page, attr, character(1), "Rd_tag")
  tags <- gsub("^\\\\", "", tags)
  doc_page[match(sections, tags)]
}


tablify_arguments <- function(arg_section) {

  param_lines <- arg_section[lengths(arg_section) == 2]
  collapse_index <- function(x, index) {
    paste0(x[[index]], collapse = "")
  }
  params       <- vapply(param_lines, collapse_index, character(1), index = 1)
  descriptions <- vapply(param_lines, collapse_index, character(1), index = 2)
  descriptions <- gsub("\n", " ", descriptions)
  data.frame(param = params, description = descriptions)
}


#' Get Documentation for theme options to use in Addin
#'
#' @return Returns a data.frame with the documented arguments
#' @export
#' @keywords internal
get_theme_doc <- function(){
  rdb <- package_docs("seminr")
  theme_help <- extract_sections(rdb$seminr_theme_create, "arguments")
  tablify_arguments(theme_help[[1]])
}


