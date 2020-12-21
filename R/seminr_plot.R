# This file contains the theme-based plotting system for models



#' Generates the dot_code for a node
#'
#' @param construct Name of the construct to generate the dot code for
#' @param model the model object that contains the r^2 values
#' @param theme the theme that is being used for plotting
#'
#' @return Dot-code for the construct
# @export
#'
# @examples
getFormattedNode <- function(construct, model, theme){

  adjusted <- theme$adjusted
  rounding <- theme$rounding
  # this is the unicode symbol for ^2
  squared_symbol <- "\U00B2"

  rs <- ""
  if (construct %in% (model$rSquared %>% colnames() )) {
    rs <- paste0(construct, " [label='", construct,
                 "\nr",squared_symbol,"=", round(model$rSquared[1,construct],rounding),
                 "']")
  } else {
    rs <- paste0(construct)
  }
  rs
}


# gets structural nodes description
getSMnodes <- function(model, theme) {
  model$constructs %>%
    stringr::str_replace("\\*", "_x_") %>%
    sapply(., getFormattedNode, model, theme) %>%
    paste0(collapse = "\n")
}

globalVariables(c("value", "."))

# gets measurement nodes description
getMMnodes <- function(model, theme) {
  model$mmVariables %>%
    dplyr::as_tibble() %>%
    dplyr::filter(!stringr::str_detect(value, "\\*")) %>%
    dplyr::pull(value) %>%
    #str_replace("\\*", "_x_") %>%
    paste0(collapse = "; ")
}



# get edges for structural model
getSMedges <- function(model, theme, weights = 1) {

  rounding <- theme$rounding

  nr <- nrow(model$smMatrix)
  nc <- ncol(model$smMatrix)
  sm <- model$smMatrix

  sm_edges <- ""

  for (i in 1:nrow(sm)) {
    #print(sm[i,1])
    # Unicode for small mathematical symbols
    beta <- "\U0001D6FD"
    #print(beta)
    gamma <- "\U0001D6FE"
    #print(gamma)

    letter <- beta
    if ( !(sm[i,1] %in% colnames(model$rSquared))) {
      letter <- gamma
    }
    coef <- round(model$path_coef[sm[i, 1], sm[i,2]], rounding)
    sm_edges <- paste0(sm_edges, sm[i, 1], " -> {", sm[i,2], "} [weight = ", weights,
                       ", label = '",letter," = ", coef,
                       "', penwidth = ", abs(coef * 5),

                       "]\n")
  }
  sm_edges %>% stringr::str_replace_all("\\*", "_x_")
}


# get edges for measurement model
# weights for dot (high value recommended for mm)
getMMedges <- function(model, theme, weights = 1000) {

  rounding <- theme$rounding
  mm <- model$mmMatrix
  #%>% as_tibble()
  #mm <- mm %>% filter(!str_detect(measurement, "\\*")) %>%


  mm_edges <- ""

  # use_outer_weights <- FALSE
  for (i in 1:nrow(mm)) {
    # i <- 1

    # default case are composite scores # TODO Switch maybe?
    use_outer_weights <- TRUE
    arrowdir <-
      paste0(
        "dir = both, arrowhead = normal, arrowtail = none"
      )

    # determine letter to use (What is with A and B type constructs?)
    # Small mathematical lamda
    lamda <- "\U0001D706"
    #print(lamda)
    letter <- "w"
    if ( mm[i,3] == "C") {
      letter <- lamda
      arrowdir <-
        "dir = both, arrowhead = none, arrowtail = normal"
      use_outer_weights <- FALSE
    }


    # TODO: This fails for higher order constructs and for interactions

    # k <- 0
    # for (j in 1:length(model$measurement_model)) {
    #   if ( mm[i, 1] == model$measurement_model[[j]][[1]] ) {
    #     k <- j
    #   }
    # }
    #
    # if ("reflective" %in% class( model$measurement_model[[k]] ) )  {
    #   arrowdir <-
    #     "dir = both,
    #      arrowhead = none,
    #      arrowtail = normal"
    #   use_outer_weights <- FALSE
    # }

    if (stringr::str_detect(mm[i, 2], "\\*")) {
      # show interaction indicators? are these real?
    } else {
      if (use_outer_weights) {
        loading = round(model$outer_weights[mm[i,2], mm[i, 1]], rounding)
      } else {
        loading = round(model$outer_loadings[mm[i,2], mm[i, 1]], rounding)
      }

      mm_edges <- paste0(mm_edges, mm[i, 2], " -> {", mm[i,1], "} [weight = ", weights,
                         ", label = '", letter, " = ", loading ,
                         "', penwidth = ", loading * 3, ", ",
                         arrowdir,
                         "]\n")
    }
  }
  mm_edges %>% stringr::str_replace_all("\\*", "_x_")
}




# new theme based plot function ----



#' Generates dot-Code from a model.
#'
#' With the help of the \code{DiagrammeR} package this code can then be plotted in
#' various contexts.
#'
#' Current limitations:
#' - Only plots PLS Models
#' - no higher order constructs
#' - No interaction terms
#'
#' @param model The PLS-model used for plotting.
#' @param title an optional title for the plot
#' @param theme an optional theme object
#'
#' @return dot-Code
#' @export
#'
#' @examples
#' mobi <- mobi
#'
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' # generate dot-Notation
#' res <- seminr_plot(mobi_pls)
#'
#' \dontrun{
#' DiagrammeR::grViz(res)
#' }
seminr_plot <- function(model, title = "", theme = NULL) {

  # adatp when necessary
  if (!c("pls_model" %in% class(model))) {
    stop(
      paste("Currently only pls_models are supported. You supplied", paste(class(model), collapse = ",")),
      call. = FALSE
    )
  }

  if (is.null(theme)) {
    thm <- seminr_theme_get()
  } else {
    thm <- theme
  }

  #rewrite construct size
  c_width_offst <- 0.1
  if (thm$construct_nodes$shape %in% c("ellipse", "oval")) {
    c_width_offst <- 0.4
  }
  construct_width <- model$constructs %>% graphics::strwidth(.,font = thm$construct_nodes$fontsize, units = "in") %>% max() + c_width_offst
  construct_height <- model$constructs %>% graphics::strheight(.,font = thm$construct_nodes$fontsize, units = "in") %>% max() + c_width_offst

  thm$construct_nodes$width <- construct_width
  thm$construct_nodes$height <- construct_height * 2

  # rewrite item size
  i_width_offst <- 0.1
  if (thm$item_nodes$shape %in% c("ellipse", "oval")) {
    i_width_offst <- 0.4
  }
  item_width <- model$mmVariables %>% graphics::strwidth(.,font = thm$item_nodes$fontsize, units = "in") %>% max() + i_width_offst
  item_height <- model$mmVariables %>% graphics::strheight(.,font = thm$item_nodes$fontsize, units = "in") %>% max() + i_width_offst

  thm$item_nodes$width <- item_width
  thm$item_nodes$height <- item_height


  # Get model parts in dot-notation
  sm_nodes <- getSMnodes(model, thm)
  mm_nodes <- getMMnodes(model, thm)
  sm_edges <- getSMedges(model, thm)
  mm_edges <- getMMedges(model, thm)


  thm %>% to_dot() %>% glue::glue()
}


