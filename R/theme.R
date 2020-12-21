# helps with alignment of text
indent <- function(x, depth = 3) {
  istring <- paste0(rep("\t", depth), collapse = "")
  stringr::str_split(x, "\n") %>% unlist() %>%
    stringr::str_squish() %>%
    paste0(istring, .) %>%
    paste0(., collapse = "\n")

}


# generic method
to_dot <- function(x, indentation, ...) {
  UseMethod("to_dot")
}


# class seminr_theme_node ----
# settings for this class
node_theme_class <- "seminr_theme_node"

shape_types <- c("box", "ellipse", "oval")
font_names <- c("helvetica", "times-roman", "courier")
color_options <- grDevices::colors()

# private constructor for node themes
new_seminr_theme_node <- function(x = list(),
                                shape = "box", color = "dimgray", fill = "white", fontsize = 8, fontname = "helvetica") {
  stopifnot(is.list(x))
  # default size
  height <- 0.2
  width <- 0.4
  shape <- match.arg(shape, shape_types)
  color <- match.arg(color, color_options)
  fill <- match.arg(fill, color_options)
  fontname <- match.arg(fontname, font_names)

  stopifnot(is.numeric(fontsize))
  stopifnot(is.numeric(height))
  stopifnot(is.numeric(width))

  structure(x, class = node_theme_class)
  x$shape = shape
  x$color = color
  x$fill = fill
  x$fontsize = fontsize
  x$fontname = fontname
  x$height = height
  x$width = width

  class(x) <- node_theme_class
  x
}

# private complex validator
validate_seminr_theme_node <- function(x) {
  # TODO: create helpful theme_node validation with user-friendly output
  x
}

#public contructor?
seminr_theme_node <- function(x = list(), shape = "box", color = "dimgray", fill = "white", fontsize = 8, fontname = "helvetica") {
  x <- as.list(x)
  y <- new_seminr_theme_node(x, shape, color, fill, fontsize,fontname)
  validate_seminr_theme_node(y)
}


#
to_dot.seminr_theme_node <- function(x, indentation = 1) {
  style <- glue::glue("\n",
                      "shape = {x$shape},\n",
                      "color = {x$color},\n",
                      "fillcolor = {x$fill},\n",
                      "style = filled,\n",
                      "fontsize = {x$fontsize},\n",
                      "height = {x$height},\n",
                      "width = {x$width},\n",
                      "fontname = {x$fontname},\n",
                      "fixedsize = true\n"
  )
  indent(style, indentation)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# seminr_theme_edge ----

edge_theme_class <- "seminr_theme_edge"

new_seminr_theme_edge <- function(x = list(),
                                color = "black", fontsize = 7, fontname = "helvetica", minlen = NULL) {
  stopifnot(is.list(x))
  color <- match.arg(color, color_options)
  fontname <- match.arg(fontname, font_names)
  stopifnot(is.numeric(fontsize))

  structure(x, class = edge_theme_class)
  x$color = color
  x$fontsize = fontsize
  x$fontname = fontname
  x$minlen = minlen

  class(x) <- edge_theme_class
  x
}

# private complex validator
validate_seminr_theme_edge <- function(x) {
  if (!is.null(x$minlen) & !is.numeric(x$minlen)) {
    stop(
      "Parameter minlen must be either Null or numeric.",
      call. = FALSE
    )
  }
  x
}

# (c) seminr_theme_edge ----
seminr_theme_edge <- function(x = list(), color = "black", fontsize = 7, fontname = "helvetica", minlen = NULL) {
  x <- as.list(x)
  y <- new_seminr_theme_edge(x, color, fontsize, fontname, minlen)
  validate_seminr_theme_edge(y)
}



to_dot.seminr_theme_edge <- function(x, indentation = 1){
  ind <- paste0(rep("\t", indentation), collapse = "")
  arrowdir <-
    paste0("dir = both,\n",
           "arrowhead = normal,\n",
           "arrowtail = none\n")

  minlen_str <- "// minlen is not used"
  if (!is.null(x$minlen)) {
    minlen_str <- glue::glue("minlen = {x$minlen},")
  }

  indent(glue::glue(
    "color = {x$color},\n",
    "fontsize = {x$fontsize},\n",
    "fontname = {x$fontname},\n",
    "{minlen_str}\n",
    "{arrowdir}\n"
  ),
  indentation)

}





# default options for themes ----

construct_nodes <- function() {
  seminr_theme_node(shape = "ellipse", color = "black", fill = "white", fontsize = 12)
}

item_nodes <- function() {
  seminr_theme_node()
}

inner_edges <- function(){
  seminr_theme_edge(color = "black", fontsize = 9)
}

outer_edges <- function() {
  seminr_theme_edge(color = "dimgray", fontsize = 7, minlen = 1)
}

# THEMES -----
theme_class <- "seminr_theme"
# private constructor to use locally
new_seminr_theme <- function(x = list(),
                            construct_nodes = NULL,
                            item_nodes = NULL,
                            inner_edges = NULL,
                            outer_edges = NULL,
                            title_font = "helvetica",
                            title_size = 24,
                            splines = FALSE,
                            rounding = 3,
                            adjusted = TRUE
){

  if (is.null(construct_nodes)) {
    construct_nodes <- construct_nodes()
  }
  if (is.null(item_nodes)) {
    item_nodes <- item_nodes()
  }
  if (is.null(inner_edges)) {
    inner_edges <- inner_edges()
  }
  if (is.null(outer_edges)) {
    outer_edges <- outer_edges()
  }

  stopifnot(is.list(x))
  structure(x, class = theme_class)

  x$title_font = title_font
  x$title_size = title_size
  x$construct_nodes <- construct_nodes
  x$item_nodes <- item_nodes
  x$inner_edges <- inner_edges
  x$outer_edges <- outer_edges
  x$splines <- splines
  x$rounding <- rounding
  x$adjusted <- adjusted
  class(x) <- theme_class
  x
}

# actual printing
to_dot.seminr_theme <- function(x, indentation = 1) {
  construct_dot <- x$construct_nodes %>% to_dot(indentation + 1)
  item_dot <- x$item_nodes %>% to_dot(indentation + 1)
  sm_dot <- x$inner_edges %>% to_dot(indentation + 1)
  mm_dot <- x$outer_edges %>% to_dot(indentation + 1)

  graph_settings <- paste0(
    "// general graph settings\n",
    "graph [\n",
    "\tcharset = 'UTF-8',\n",
    "\tlayout = dot,\n",
    "\tlabel = '{title}',\n",
    "\tfontsize = ",x$title_size, ",\n",
    "\tfontname = ",x$title_font, ",\n",
    "\trankdir = LR,\n",
    "\tlabelloc = t,\n",
    "\tsplines = ",x$splines,"\n",
    "]\n"
  )


  paste0("digraph G {{\n",
         graph_settings,
         "// The structural model
      subgraph sm {{
        rankdir = LR;
        node [\n",
      construct_dot,
      "]
        {sm_nodes}

        // How constructs are connected
        edge [\n",
      sm_dot,
      "]
        {sm_edges}
      }

      // ---------------------
      // The measurement model
      // ---------------------
      subgraph mm {{
        node [\n",
      item_dot,
      "]
        {mm_nodes}

        // How items are connected with nodes
        edge [\n",
      mm_dot,
      "]
        {mm_edges}
      }
  }"
  )

}


#' @export
print.seminr_theme <- function(x, ...) utils::str(x)

#' Reports whether x is a seminr_theme object
#' @param x An object to test
#' @export
#' @keywords internal
is.seminr_theme <- function(x) inherits(x, "seminr_theme")




