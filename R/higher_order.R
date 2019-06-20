# Takes a HOC name and replaces that constructs relationships with the dimensions of the HOC
substitute_dimensions_for_HOC <- function(construct, sm,mm) {
  # Identify dimensions of HOCs
  dimensions <- mm[mm[,"type"] == "HOCA" | mm[,"type"] == "HOCB",][mm[mm[,"type"] == "HOCB" | mm[,"type"] == "HOCA",][,"construct"] == construct,"measurement"]
  # identify antecedent relationships to HOC
  antecedents <- sm[which(sm[,"target"] == construct),"source"]
  # change antecedent relationship to first order constructs in structural model
  if (!length(antecedents) == 0) {
    sm <- rbind(sm,
                relationships(paths(from = antecedents,
                                    to = dimensions)))
    sm <- sm[-which(sm[,"target"] == construct),]
  }

  # identify outcomes
  outcomes <- sm[which(sm[,"source"] == construct),"target"]
  if (!length(outcomes) == 0) {
    sm <- rbind(sm,
                relationships(paths(from = dimensions,
                                    to = outcomes)))
    sm <- sm[-which(sm[,"source"] == construct),]
  }
  return(sm)
}

remove_HOC_in_measurement_model <- function(construct,mm) {
  mm[!mm[,"construct"] == construct,]
}

# Function to parse measurement and structural model and create the higher order model with complete information
prepare_higher_order_model <- function(data,sm , mm, ints, inners) {
  #retain the mm and sm
  orig_mm <- mm
  orig_sm <- sm
  # Identify HOCs
  HOCs <- unique(mm[which(mm[,"type"] == "HOCA" | mm[,"type"] == "HOCB"),"construct"])

  # Rebuild model for first stage
  # Add new HOC paths to SM
  for (construct in HOCs) {
    sm <- substitute_dimensions_for_HOC(construct, sm,mm)
    #mm <- remove_HOC_in_measurement_model(construct,mm)
  }
  # Identify all the dimensions
  dimensions <- orig_mm[which(orig_mm[,"construct"] == HOCs),"measurement"]
  # remove HOCs from mm
  new_mm <- mm[-which(mm[,"construct"] == HOCs),]

  # Run first stage
  new_model <- estimate_pls(data = data,
                            measurement_model = new_mm,
                            interactions = ints,
                            structural_model = sm,
                            inner_weights = inners)

  # Add the construct scores to data
  data <- cbind(data, new_model$construct_scores[,dimensions])

  # Update the mm to include the type of the new data and item
  mm[mm[,"type"] == "HOCA","type"] <- "A"
  mm[mm[,"type"] == "HOCB","type"] <- "B"


  # pass the updated mm, sm and data back to estimate_model()
  return(list(data = data,
              sm = orig_sm,
              mm = mm))
}



#' HOC construct
#'
#' \code{two_stage_HOC} creates the constructs from further constructs using the two-stage method (Becker et al., 2012).
#'
#' This function conveniently maps first order constructs onto second order constructs using
#' construct names.
#'
#' @param construct_name of second order construct
#' @param dimensions the first order constructs
#' @param weights is the relationship between the second order construct and first order constructs. This can be
#' specified as \code{correlation_weights} or \code{mode_A} for correlation weights (Mode A) or as
#' \code{regression_weights} or \code{mode_B} for regression weights (Mode B). Default is correlation weights.
#'
#' @usage
#'  two_stage_HOC(construct_name, dimensions,weights = correlation_weights)
#'
#' @seealso See \code{\link{constructs}}, \code{\link{reflective}}
#'
#' @examples
#'   mobi_mm <- constructs(
#'     composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
#'     composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
#'     two_stage_HOC("Quality",      c("Image","Expectation"), weights = regression_weights),
#'     composite("Value",        multi_items("PERV", 1:2), weights = mode_B)
#'   )
#' @export
two_stage_HOC <- function(construct_name, dimensions, weights = correlation_weights) {
  construct_names <- rep(construct_name, length(dimensions))
  # TODO remove the duplicated conditional
  # TODO possibly remove the construct_names object as the construct name should be coerced to fitr the matrix
  if(identical(weights,correlation_weights) | identical(weights,mode_A)) {
    return(c(rbind(construct_names,dimensions,"HOCA")))
  }
  if(identical(weights, regression_weights) | identical(weights, mode_B)) {
    return(c(rbind(construct_names,dimensions,"HOCB")))
  }
}
