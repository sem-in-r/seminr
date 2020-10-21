#' seminr fSquared Function
#'
#' The \code{fSquared} function calculates f^2 effect size for a given IV and DV
#'
#' @param seminr_model A \code{seminr_model} containing the estimated seminr model.
#' @param iv An independent variable in the model.
#' @param dv A dependent variable in the model.
#'
#' @usage
#' fSquared(seminr_model, iv, dv)
#'
#' @references Cohen, J. (2013). Statistical power analysis for the behavioral sciences. Routledge.
#'
#' @examples
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#'
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
#' fSquared(mobi_pls, "Image", "Satisfaction")
#' @export
fSquared <- function(seminr_model, iv, dv) {
  if (length(seminr_model$constructs) == 2) {
    rsq <- (seminr_model$rSquared["Rsq", dv])
    return((rsq - 0) / (1 - rsq))
  }
  with_sm <- seminr_model$smMatrix
  without_sm <- subset(with_sm, !((with_sm[, "source"] == iv) & (with_sm[, "target"] == dv)))

  # Calculate fSquared using LM of constructs instead of re-estiating the model (this is probably incorrect, but might serve for interaction models)
  # dvs <- unique(seminr_model$smMatrix[, "target"])
  # path_matrix <- seminr_model$path_coef
  # for (dv in dvs) {
  #   ivs <- names(path_matrix[(path_matrix[,dv] != 0),dv])
  #   sub("\\*", "x", ivs)
  #   frmla <- stats::as.formula(paste(dv,paste(sub("\\*", "x", ivs), collapse ="+"), sep = " ~ "))
  #   data <- as.data.frame(seminr_model$construct_scores)
  #   colnames(data) <- sub("\\*", "x",colnames(data))
  #   lm <- stats::lm(formula = frmla, data = data)
  #   summary(lm)
  #   }
  utils::capture.output(
    without_pls <- estimate_pls(data = seminr_model$rawdata,
                                measurement_model = seminr_model$measurement_model,
                                structural_model = without_sm)
  )
  with_r2 <- seminr_model$rSquared["Rsq", dv]
  ifelse(any(without_sm[,"target"] == dv),
         without_r2 <- without_pls$rSquared["Rsq", dv],
         without_r2 <- 0)

  return((with_r2 - without_r2) / (1 - with_r2))
}

model_fsquares <- function(seminr_model) {
  if (any(names(seminr_model$measurement_model) == "orthogonal_interaction")
      | any(names(seminr_model$measurement_model) == "two_stage_interaction")
      | any(names(seminr_model$measurement_model) == "scaled_interaction" )) {
    return("The fSquare cannot be calculated as the model contains an interaction term and omitting either the antecedent or moderator in the interaction term will cause model estimation to fail")
  }

  path_matrix <- seminr_model$path_coef
  fsquared_matrix <- path_matrix

  for (dv in all_endogenous(seminr_model$smMatrix)) {
    for (iv in all_exogenous(seminr_model$smMatrix)) {
      fsquared_matrix[iv, dv] <- fSquared(seminr_model = seminr_model,
                                          iv = iv,
                                          dv = dv)
    }
  }
  class(fsquared_matrix) <- append(class(fsquared_matrix), "table_output")
  return(fsquared_matrix)
}
