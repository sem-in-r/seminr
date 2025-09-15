# prediction function for a two-stage estimated seminr model ----
estimate_actual_star <- function(pls_model, train_data, testData) {
  no_int_mmvars <- pls_model$mmVariables[!grepl("\\*", pls_model$mmVariables)]
  actual_star <- estimate_pls(data = rbind(train_data[,no_int_mmvars], testData[,no_int_mmvars]),
                                       measurement_model = pls_model$measurement_model,
                                       structural_model = pls_model$structural_model)$construct_scores[,all_endogenous(pls_model$smMatrix),drop = F] |> suppressMessages()

  actual_star_out <- actual_star[(nrow(train_data)+1):(nrow(testData) + nrow(train_data)),,drop = F]
  actual_star_in <- actual_star[1:(nrow(train_data)),,drop = F]
  return(list(actual_oos = actual_star_out,
              actual_is = actual_star_in))
}

parse_interactions <- function(x) {
  ind1 <- regexpr("\\*", x)
  return(list(interaction = x,
              antecedent = substr(x,0,ind1-1),
              moderator = substr(x, ind1+1,nchar(x)),
              inter_ind = paste(x,"_intxn",sep = "")))
}
return_mod_scores <- function(OOS_composite_scores,
                              testData,
                              x) {
  Moderator_score <- matrix(OOS_composite_scores[, x$antecedent] * OOS_composite_scores[,x$moderator], ncol = 1)
  colnames(Moderator_score) <- x$inter_ind
  return(Moderator_score)
}

one_stage_predict <- function(pls_model, testData, technique) {

  # First reappend the testData to the trainData to get fulldata (do not duplicate rows)
  no_int_mmvars <- pls_model$mmVariables[!grepl("\\*", pls_model$mmVariables)]
  fulldata <- pls_model$data[,no_int_mmvars]
  fulldata[rownames(testData),no_int_mmvars] <- testData[,no_int_mmvars]

  suppressMessages(fullmodel <- seminr::estimate_pls(data =fulldata,
                                                     measurement_model = pls_model$measurement_model,
                                                     structural_model = pls_model$structural_model,
                                                     inner_weights = pls_model$inner_weights,
                                                     missing = pls_model$settings$missing,
                                                     missing_value = pls_model$settings$missing_value,
                                                     maxIt = pls_model$settings$maxIt,
                                                     stopCriterion = pls_model$settings$stopCriterion))
  actual_star <- fullmodel$construct_scores

  #Extract Measurements needed for Predictions
  normData <- testData[,pls_model$mmVariables]

  # Standardize data
  normData[,pls_model$mmVariables] <- standardize_data(normData[,pls_model$mmVariables],pls_model$meanData[pls_model$mmVariables],pls_model$sdData[pls_model$mmVariables])

  #Convert dataset to matrix
  normData<-data.matrix(normData)

  #Estimate Factor Scores from Outter Path
  predicted_construct_scores <- normData%*%pls_model$outer_weights

  #Estimate Factor Scores from Inner Path and complete Matrix
  predicted_construct_scores <- technique(pls_model$smMatrix, pls_model$path_coef, predicted_construct_scores)

  #Predict Measurements with loadings
  predictedMeasurements<-predicted_construct_scores%*% t(pls_model$outer_loadings)

  # Unstandardize data
  predictedMeasurements[,pls_model$mmVariables] <- unstandardize_data(predictedMeasurements[,pls_model$mmVariables],pls_model$meanData[pls_model$mmVariables],pls_model$sdData[pls_model$mmVariables])

  #Calculating the residuals
  residuals <- testData[,pls_model$mmVariables] - predictedMeasurements[,pls_model$mmVariables]

  #Prepare return pls_model
  predictResults <- list(testData = testData[,pls_model$mmVariables],
                         predicted_items = predictedMeasurements[,pls_model$mmVariables],
                         item_residuals = residuals,
                         predicted_composite_scores = predicted_construct_scores,
                         composite_residuals = (actual_star[rownames(testData),] - predicted_construct_scores),
                         actual_star = actual_star[rownames(testData),])

  class(predictResults) <- "predicted_seminr_model"
  return(predictResults)
}

two_stage_predict <- function(pls_model, testData, technique) {
  # First reappend the testData to the trainData to get fulldata (do not duplicate rows)
  no_int_mmvars <- pls_model$mmVariables[!grepl("\\*", pls_model$mmVariables)]
  fulldata <- pls_model$data[,no_int_mmvars]
  fulldata[rownames(testData),no_int_mmvars] <- testData[,no_int_mmvars]
  suppressMessages(fullmodel <- seminr::estimate_pls(data =fulldata,
                                                     measurement_model = pls_model$measurement_model,
                                                     structural_model = pls_model$structural_model,
                                                     inner_weights = pls_model$inner_weights,
                                                     missing = pls_model$settings$missing,
                                                     missing_value = pls_model$settings$missing_value,
                                                     maxIt = pls_model$settings$maxIt,
                                                     stopCriterion = pls_model$settings$stopCriterion))
  actual_star <- fullmodel$construct_scores

  # collect all interactions
  interactions <- pls_model$constructs[grepl("\\*", pls_model$constructs)]

  # parse interactions to get the iv, mv, and indicator name
  int_list <- lapply(interactions,parse_interactions)

  # identify the training data
  train_data <- pls_model$rawdata

  # recreate the first stage measurement model
  first_stage_mm <- pls_model$measurement_model[!(unique(pls_model$mmMatrix[,1]) %in% interactions)]

  # recreate the first stage structural model
  first_stage_sm <- pls_model$structural_model[ !(pls_model$structural_model[,"source"] %in% interactions),]
  first_stage_model <- estimate_pls(data = train_data,
                                    measurement_model = first_stage_mm,
                                    structural_model = first_stage_sm) |> suppressMessages()

  scaled_data <-
    standardize_data(testData[,no_int_mmvars,drop=F],
                              first_stage_model$meanData[no_int_mmvars],
                              first_stage_model$sdData[no_int_mmvars])

  # 1.2. Calculate the composite scores
  OOS_composite_scores <- as.matrix(scaled_data[,no_int_mmvars,drop=F]) %*% first_stage_model$outer_weights[no_int_mmvars,,drop=F]

  # 1.3. Calculate moderator and add to the rawdata
  mod_scores <- lapply(int_list, function(x) {return_mod_scores(OOS_composite_scores, testData, x)})

  # return_mod_scores(OOS_composite_scores, testData, int_list)
  out_data_mod <- cbind(testData, do.call("cbind", mod_scores))


  # standardize the data using second stage model
  scaled_out_data_mod <-
    standardize_data(out_data_mod[,pls_model$mmVariables],
                              pls_model$meanData[pls_model$mmVariables],
                              pls_model$sdData[pls_model$mmVariables])

  # OOS_construct_scores <- scaled_out_data_mod[,pls_model$mmVariables] %*% pls_model$outer_weights[pls_model$mmVariables,]
  # pred_OOS_construct_scores <-  (OOS_construct_scores[,colnames(pls_model$path_coef)] %*% pls_model$path_coef[colnames(pls_model$path_coef),])[,all_endogenous(pls_model$smMatrix),drop=F]
  #
  #
  #
  # pred_OOS_indicator_scores <- pred_OOS_construct_scores %*% pls_model$outer_loadings
  #
  #
  #
  # fit_IS_construct_scores <- (pls_model$construct_scores[,colnames(pls_model$path_coef)] %*% pls_model$path_coef[colnames(pls_model$path_coef),])[,all_endogenous(pls_model$smMatrix),drop=F]
  # construct_pred_error <- actual_star$actual_oos - pred_OOS_construct_scores
  # construct_fit_error <- actual_star$actual_is - fit_IS_construct_scores
  # construct_is_mse <-
  #   apply(construct_fit_error,
  #         2,
  #         function(x) mean(x^2))
  # construct_oos_mse <-
  #   apply(construct_pred_error,
  #         2,
  #         function(x) mean(x^2))

  #Estimate Factor Scores from Outter Path
  predicted_construct_scores <- scaled_out_data_mod[,pls_model$mmVariables] %*% pls_model$outer_weights[pls_model$mmVariables,]

  #Estimate Factor Scores from Inner Path and complete Matrix
  predicted_construct_scores <- technique(pls_model$smMatrix, pls_model$path_coef, predicted_construct_scores)

  #Predict Measurements with loadings
  predictedMeasurements <- predicted_construct_scores%*% t(pls_model$outer_loadings)

  # Unstandardize data
  predictedMeasurements <- unstandardize_data(predictedMeasurements[,no_int_mmvars],pls_model$meanData[no_int_mmvars],pls_model$sdData[no_int_mmvars])

  colnames(predictedMeasurements) <- no_int_mmvars
  #Calculating the residuals
  residuals <- testData[,no_int_mmvars] - predictedMeasurements[,no_int_mmvars]

  #Prepare return Object
  predictResults <- list(testData = testData[,no_int_mmvars],
                         predicted_items = predictedMeasurements[,no_int_mmvars],
                         item_residuals = residuals[,no_int_mmvars],
                         predicted_composite_scores = predicted_construct_scores,
                         composite_residuals = (actual_star[rownames(testData),] - predicted_construct_scores),
                         actual_star = actual_star[rownames(testData),])
  class(predictResults) <- "predicted_seminr_model"
  return(predictResults)
}


# Predict function for SEMinR PLS models
#' @export
predict.seminr_model <- function(object, testData, technique = predict_DA, na.print=".", digits=3, ...){
  stopifnot(inherits(object, "seminr_model"))

  # Abort if received a higher-order-model or moderated model
  if (!is.null(object$hoc)) {
    message("There is no published solution for applying PLSpredict to higher-order-models")
    return()
  }
  if (!is.null(object$interaction)) {
    if (all(grepl("two_stage",names(object$measurement_model[grepl("interaction",names(object$measurement_model))])))) {
      return(two_stage_predict(object, testData, technique) )
    }
    stop("Please use two-stage estimation for all interaction terms for generating predictions from moderated models.
    e.g.: interaction_term(IV, MV, method = two_stage). Two stage yields the leasst biased estimate and best predictive performance
         as per Danks & Ray (2023)")
  }
return(one_stage_predict(object, testData, technique))
}

#' Predict_pls performs either k-fold or LOOCV on a SEMinR PLS model and generates predictions
#'
#' \code{predict_pls} uses cross-validation to generate in-sample and out-sample predictions for PLS models generated by SEMinR.
#'
#' This function generates cross-validated in-sample and out-sample predictions for PLS models generated by SEMinR. The
#' cross validation technique can be k-fold if a number of folds are specified, or leave-one-out-cross-validation (LOOCV) if no folds
#' arew specified. LOOCV is recommended for small datasets.
#'
#' @param model A SEMinR model that has been estimated on the FULL dataset.
#'
#' @param technique The predictive technique to be employed, Earliest Antecedents (EA) \code{predict_EA} or
#' Direct Antecedents (DA) \code{predict_DA}
#'
#' @param noFolds The required number of folds to use in k-fold cross validation. If NULL, then parallel LOOCV will be executed.
#' Default is NULL.
#'
#' @param reps The number of times the cross-validation will be repeated. Default is NULL.
#'
#' @param cores The number of cores to use for parallel LOOCV processing. If k-fold is used, the process will not be parallelized.
#'
#' @return A list of the estimated PLS and LM prediction results:
#'  \item{PLS_out_of_sample}{A matrix of the out-of-sample indicator predictions generated by the SEMinR model.}
#'  \item{PLS_in_sample}{A matrix of the in-sample indicator predictions generated by the SEMinR model.}
#'  \item{lm_out_of_sample}{A matrix of the out-of-sample indicator predictions generated by a linear regression model.}
#'  \item{lm_in_sample}{A matrix of the in-sample indicator predictions generated by a linear regression model.}
#'  \item{item_actuals}{A matrix of the actual indicator scores.}
#'  \item{PLS_out_of_sample_residuals}{A matrix of the out-of-sample indicator PLS prediction residuals.}
#'  \item{PLS_in_sample_residuals}{A matrix of the in-sample indicator PLS prediction residuals.}
#'  \item{lm_out_of_sample_residuals}{A matrix of the out-of-sample LM indicator prediction residuals.}
#'  \item{lm_in_sample_residuals}{A matrix of the in-sample LM indicator prediction residuals.}
#'  \item{mmMatrix}{A Matrix of the measurement model relations.}
#'  \item{smMatrix}{A Matrix of the structural model relations.}
#'  \item{constructs}{A vector of the construct names.}
#'  \item{mmVariables}{A vector of the indicator names.}
#'  \item{outer_loadings}{The matrix of estimated indicator loadings.}
#'  \item{outer_weights}{The matrix of estimated indicator weights.}
#'  \item{path_coef}{The matrix of estimated structural model relationships.}
#'  \item{iterations}{A numeric indicating the number of iterations required before the algorithm converged.}
#'  \item{weightDiff}{A numeric indicating the minimum weight difference between iterations of the algorithm.}
#'  \item{construct_scores}{A matrix of the estimated construct scores for the PLS model.}
#'  \item{rSquared}{A matrix of the estimated R Squared for each construct.}
#'  \item{inner_weights}{The inner weight estimation function.}
#'  \item{data}{A matrix of the data upon which the model was estimated (INcluding interactions.}
#'  \item{rawdata}{A matrix of the data upon which the model was estimated (EXcluding interactions.}
#'  \item{measurement_model}{The SEMinR measurement model specification.}
#'
#' @usage
#'
#' predict_pls(model, technique, noFolds, reps, cores)
#'
#' @examples
#' data(mobi)
#'
#' # seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2)),
#'   composite("Satisfaction", multi_items("CUSA", 1:3))
#' )
#'
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value"))
#' )
#'
#' mobi_pls <- estimate_pls(mobi, mobi_mm, mobi_sm)
#' cross_validated_predictions <- predict_pls(model = mobi_pls,
#'                                            technique = predict_DA,
#'                                            noFolds = 10,
#'                                            cores = NULL)
#'
#' @export
predict_pls <- function(model, technique = predict_DA, noFolds = NULL, reps = NULL, cores = NULL) {

  stopifnot(inherits(model, "seminr_model"))
  # Abort if received a higher-order-model or moderated model
  if (!is.null(model$hoc)) {
    message("There is no published solution for applying PLSpredict to higher-order-models")
    return()
  }
  # Get endogenous item names
  endogenous_items <- c(unlist(sapply(unique(model$smMatrix[,2]), function(x) model$mmMatrix[model$mmMatrix[, "construct"] == x,"measurement"]), use.names = FALSE))

  # shuffle data
  order <- sample(nrow(model$data),nrow(model$data), replace = FALSE)
  ordered_data <- model$data[order,]

  # collect in-sample and out-sample prediction matrices and sort everything to original row indexes
  if(is.null(reps)) {

    pred_matrices <- prediction_matrices( noFolds, ordered_data, model,technique, cores)
    PLS_predicted_outsample_item <- pred_matrices$out_of_sample_item[rownames(model$data),]
    PLS_predicted_insample_item <- pred_matrices$in_sample_item[rownames(model$data),]
    LM_predicted_outsample_item <- pred_matrices$out_of_sample_lm_item[rownames(model$data),]
    LM_predicted_insample_item <- pred_matrices$in_sample_lm_item[rownames(model$data),]
  } else {
    no_int_mmvars <- model$mmVariables[!grepl("\\*", model$mmVariables)]
    pls_pred_oos_array <- array(,dim = c(nrow(ordered_data), length(no_int_mmvars), reps))
    pls_pred_is_array <- array(,dim = c(nrow(ordered_data), length(no_int_mmvars), reps))
    lm_pred_oos_array <- array(,dim = c(nrow(ordered_data), length(endogenous_items), reps))
    lm_pred_is_array <- array(,dim = c(nrow(ordered_data), length(endogenous_items), reps))
    for (i in 1:reps) {
      pred_matrices <- prediction_matrices( noFolds, ordered_data, model,technique, cores)
      pls_pred_oos_array[,,i] <- pred_matrices$out_of_sample_item[rownames(model$data),]
      pls_pred_is_array[,,i] <- pred_matrices$in_sample_item[rownames(model$data),]
      lm_pred_oos_array[,,i] <- pred_matrices$out_of_sample_lm_item[rownames(model$data),]
      lm_pred_is_array[,,i] <- pred_matrices$in_sample_lm_item[rownames(model$data),]
    }
    PLS_predicted_outsample_item <- apply(pls_pred_oos_array,c(1,2),mean)
    PLS_predicted_insample_item <- apply(pls_pred_is_array,c(1,2),mean)
    LM_predicted_outsample_item <- apply(lm_pred_oos_array,c(1,2),mean)
    LM_predicted_insample_item <- apply(lm_pred_is_array,c(1,2),mean)
    colnames(PLS_predicted_outsample_item) <- no_int_mmvars
    colnames(PLS_predicted_insample_item) <- no_int_mmvars
    colnames(LM_predicted_outsample_item) <- endogenous_items
    colnames(LM_predicted_insample_item) <- endogenous_items
  }

  # Allocate results
  results <- list(
    composites = list(
      composite_out_of_sample = pred_matrices$out_of_sample_construct[rownames(model$data),],
      composite_in_sample = pred_matrices$in_sample_construct[rownames(model$data),],
      actuals_star = model$construct_scores[rownames(model$data),]),
    items = list(PLS_out_of_sample = PLS_predicted_outsample_item[,endogenous_items],
                  PLS_in_sample = PLS_predicted_insample_item[,endogenous_items],
                  lm_out_of_sample = LM_predicted_outsample_item,
                  lm_in_sample = LM_predicted_insample_item,
                  item_actuals = ordered_data[rownames(model$data),model$mmVariables],
                  PLS_out_of_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - PLS_predicted_outsample_item[,endogenous_items]),
                  PLS_in_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - PLS_predicted_insample_item[,endogenous_items]),
                  lm_out_of_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - LM_predicted_outsample_item),
                  lm_in_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - LM_predicted_insample_item)),
    model = model)
  class(results) <- "predict_pls_model"
  return(results)
}

# Function to calculate item metrics
item_metrics <- function(pls_prediction_kfold) {

  # Genereate IS PLS metrics
  PLS_item_prediction_metrics_IS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$PLS_in_sample_residuals, 2, prediction_metrics))

  # Generate OOS PLS metrics
  PLS_item_prediction_metrics_OOS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$PLS_out_of_sample_residuals, 2, prediction_metrics))

  # Generate IS LM metrics
  LM_item_prediction_metrics_IS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$lm_in_sample_residuals, 2, prediction_metrics))

  # Generate OOS LM metrics
  LM_item_prediction_metrics_OOS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$lm_out_of_sample_residuals, 2, prediction_metrics))

  # Assign rownames to matrices
  rownames(PLS_item_prediction_metrics_IS) <- rownames(PLS_item_prediction_metrics_OOS) <- rownames(LM_item_prediction_metrics_OOS) <- c("RMSE","MAE")
  rownames(LM_item_prediction_metrics_OOS) <- rownames(LM_item_prediction_metrics_IS) <- c("RMSE","MAE")

  return(list(PLS_item_prediction_metrics_IS = PLS_item_prediction_metrics_IS,
              PLS_item_prediction_metrics_OOS = PLS_item_prediction_metrics_OOS,
              LM_item_prediction_metrics_IS = LM_item_prediction_metrics_IS,
              LM_item_prediction_metrics_OOS = LM_item_prediction_metrics_OOS))
}

# Check these lib functions ----
# function to subset a smMatrix by construct(x)
subset_by_construct <- function(x, smMatrix) {
  smMatrix[smMatrix[,"source"] == c(x), "target"]
}

# Function to check whether a named construct's antecedents occur in a list
construct_antecedent_in_list <- function(x,list, smMatrix) {
  all(smMatrix[smMatrix[,"target"]==x,"source"] %in% list)
}

# Function to iterate over a vector of constructs and return the antecedents each construct depends on
depends_on <- function(constructs_vector, smMatrix) {
  return(unique(unlist(sapply(constructs_vector, subset_by_construct, smMatrix = smMatrix), use.names = FALSE)))
}

# Function to iterate over a vector of constructs and check whether their antecedents occur in a list
antecedents_in_list <- function(constructs_vector, list,smMatrix) {
  as.logical(sapply(constructs_vector, construct_antecedent_in_list, list = list, smMatrix = smMatrix))
}

# Function to organize order of endogenous constructs from most exogenous forwards
construct_order <- function(smMatrix) {

  # get purely endogenous and purely exogenous
  only_endogenous <- setdiff(unique(smMatrix[,2]), unique(smMatrix[,1]))
  only_exogenous <- setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))

  # get construct names
  construct_names <- unique(c(smMatrix[,1],smMatrix[,2]))

  # get all exogenous constructs
  all_exogenous_constructs <- setdiff(construct_names, only_endogenous)

  # initialize construct order with first purely exogenous construct
  construct_order <- only_exogenous

  # Iterate over constructs to generate construct_order
  while (!setequal(all_exogenous_constructs,construct_order)) {
    construct_order <- c(construct_order,setdiff(depends_on(construct_order,smMatrix)[antecedents_in_list(depends_on(construct_order,smMatrix), construct_order, smMatrix)], construct_order))
  }

  # return the order of endogenous constructs to be predicted
  final_list <- setdiff(construct_order,only_exogenous)
  return(c(final_list,only_endogenous))

}

# Function to standardize a matrix by sd vector and mean vector
standardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(t(t(sweep(data_matrix,2,means_vector)) / sd_vector))
}

# Function to un-standardize a matrix by sd vector and mean vector
unstandardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(sweep((data_matrix %*% diag(sd_vector)),2,means_vector,"+"))
}

#$ Function to sum rows of a matrix
sum_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x]))
}

#$ Function to mean rows of a matrix
mean_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x])/(noFolds-1))
}

#### Check ----

# Function to return train and test predictions for a model
in_and_out_sample_predictions <- function(x, folds, ordered_data, model,technique) {
  testIndexes <- which(folds==x,arr.ind=TRUE)
  trainIndexes <- which(folds!=x,arr.ind=TRUE)
  testingData <- ordered_data[testIndexes, ]
  trainingData <- ordered_data[-testIndexes, ]
  no_int_mmvars <- model$mmVariables[!grepl("\\*", model$mmVariables)]

  # Create matrices for return data
  PLS_predicted_outsample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(rownames(ordered_data),model$constructs))
  PLS_predicted_insample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(rownames(ordered_data),model$constructs))
  PLS_predicted_outsample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(no_int_mmvars),dimnames = list(rownames(ordered_data),no_int_mmvars))
  PLS_predicted_insample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(no_int_mmvars),dimnames = list(rownames(ordered_data),no_int_mmvars))
  PLS_predicted_insample_item_residuals <- matrix(0,nrow = nrow(ordered_data),ncol = length(no_int_mmvars),dimnames = list(rownames(ordered_data),no_int_mmvars))
  #PLS prediction on testset model
  suppressMessages(train_model <- estimate_pls(data = trainingData,
                                               measurement_model = model$measurement_model,
                                               structural_model = model$smMatrix,
                                               inner_weights = model$inner_weights,
                                               missing = model$settings$missing,
                                               missing_value = model$settings$missing_value,
                                               maxIt = model$settings$maxIt,
                                               stopCriterion = model$settings$stopCriterion))
  test_predictions <- stats::predict(object = train_model,
                                     testData = testingData,
                                     technique = technique)

  PLS_predicted_outsample_construct[testIndexes,] <-  test_predictions$predicted_composite_scores
  PLS_predicted_outsample_item[testIndexes,] <- test_predictions$predicted_items


  #PLS prediction on trainset model
  train_predictions <- stats::predict(object = train_model,
                                      testData = trainingData,
                                      technique = technique)

  PLS_predicted_insample_construct[trainIndexes,] <- train_predictions$predicted_composite_scores
  PLS_predicted_insample_item[trainIndexes,] <- train_predictions$predicted_items
  PLS_predicted_insample_item_residuals[trainIndexes,] <- as.matrix(train_predictions$item_residuals)

  ## Perform prediction on LM models for benchmark
  # Identify endogenous items
  endogenous_items <- unlist(sapply(unique(model$smMatrix[,2]), function(x) model$mmMatrix[model$mmMatrix[, "construct"] == x,"measurement"]), use.names = FALSE)

  #LM Matrices
  lm_holder <- sapply(unique(model$smMatrix[,2]), generate_lm_predictions, model = model,
                      ordered_data = ordered_data[,model$mmVariables],
                      testIndexes = testIndexes,
                      endogenous_items = endogenous_items,
                      trainIndexes = trainIndexes,
                      technique = technique)

  lmprediction_in_sample <- matrix(0, ncol = 0 , nrow = length(trainIndexes))
  lmprediction_out_sample <- matrix(0, ncol = 0 , nrow = length(testIndexes))
  lmprediction_in_sample_residuals <- matrix(0,nrow=nrow(ordered_data),ncol=length(endogenous_items),byrow =TRUE,dimnames = list(rownames(ordered_data),endogenous_items))

  # collect the odd and even numbered matrices from the matrices return object
  lmprediction_in_sample <- do.call(cbind, lm_holder[((1:(length(unique(model$smMatrix[,2]))*2))[1:(length(unique(model$smMatrix[,2]))*2)%%2==1])])
  lmprediction_out_sample <- do.call(cbind, lm_holder[((1:(length(unique(model$smMatrix[,2]))*2))[1:(length(unique(model$smMatrix[,2]))*2)%%2==0])])
  lmprediction_in_sample_residuals[trainIndexes,] <- as.matrix(ordered_data[trainIndexes,as.vector(endogenous_items)]) - lmprediction_in_sample[trainIndexes,as.vector(endogenous_items)]

  return(list(PLS_predicted_insample = PLS_predicted_insample_construct,
              PLS_predicted_outsample = PLS_predicted_outsample_construct,
              PLS_predicted_insample_item = PLS_predicted_insample_item,
              PLS_predicted_outsample_item = PLS_predicted_outsample_item,
              LM_predicted_insample_item = lmprediction_in_sample,
              LM_predicted_outsample_item = lmprediction_out_sample,
              PLS_predicted_insample_item_residuals = PLS_predicted_insample_item_residuals,
              LM_predicted_insample_item_residuals = lmprediction_in_sample_residuals))
}

# Function to collect and parse prediction matrices
prediction_matrices <- function(noFolds, ordered_data, model,technique, cores) {
  out <- tryCatch(
    {
      # If noFolds is NULL, perform parallel LOOCV
      if (is.null(noFolds)) {
        # Automatically perform LOOCV if number of folds not specified
        noFolds = nrow(ordered_data)
        #Create noFolds equally sized folds
        folds <- cut(seq(1,nrow(ordered_data)),breaks=noFolds,labels=FALSE)

        # Create cluster
        suppressWarnings(ifelse(is.null(cores), cl <- parallel::makeCluster(parallel::detectCores()), cl <- parallel::makeCluster(cores)))

        # Export variables and functions to cluster
        parallel::clusterExport(cl=cl, varlist=c("generate_lm_predictions",
                                                 "predict_lm_matrices",
                                                 "standardize_data",
                                                 "unstandardize_data"), envir=environment())
        #parallel::clusterEvalQ(cl=cl,expr = "library(PLSpredict)")

                # Execute the bootstrap
        utils::capture.output(matrices <- parallel::parSapply(cl,1:noFolds,in_and_out_sample_predictions,folds = folds,
                                                              ordered_data = ordered_data,
                                                              model = model,
                                                              technique = technique))
        # Stop cluster
        parallel::stopCluster(cl)
      } else {
        #Create noFolds equally sized folds
        folds <- cut(seq(1,nrow(ordered_data)),breaks=noFolds,labels=FALSE)
        matrices <- sapply(1:noFolds, in_and_out_sample_predictions, folds = folds,ordered_data = ordered_data, model = model, technique = technique)
      }

      # collect the odd and even numbered matrices from the matrices return object
      no_int_mmvars <- model$mmVariables[!grepl("\\*", model$mmVariables)]
      in_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==1]])
      out_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==2]])
      in_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==3]])
      out_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==4]])
      in_sample_lm_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==5]])
      out_sample_lm_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==6]])
      PLS_in_sample_item_residuals <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==7]])
      LM_in_sample_item_residuals <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==0]])

      # mean the in-sample construct predictions by row
      average_insample_construct <- sapply(1:length(model$constructs), mean_rows, matrix = in_sample_construct_matrix,
                                           noFolds = noFolds,
                                           constructs = model$constructs)

      # mean the in-sample item predictions by row
      average_insample_item <- sapply(1:length(no_int_mmvars), mean_rows, matrix = in_sample_item_matrix,
                                      noFolds = noFolds,
                                      constructs = no_int_mmvars)

      # sum the out-sample construct predictions by row
      average_outsample_construct <- sapply(1:length(model$constructs), sum_rows, matrix = out_sample_construct_matrix,
                                            noFolds = noFolds,
                                            constructs = model$constructs)

      # sum the out-sample item predictions by row
      average_outsample_item <- sapply(1:length(no_int_mmvars), sum_rows, matrix = out_sample_item_matrix,
                                       noFolds = noFolds,
                                       constructs = no_int_mmvars)

      # square the out-sample pls residuals, mean them and take the square root
      average_insample_pls_item_residuals <- sqrt(sapply(1:length(no_int_mmvars), mean_rows, matrix = PLS_in_sample_item_residuals^2,
                                                         noFolds = noFolds,
                                                         constructs = no_int_mmvars))
      # Collect endogenous items
      endogenous_items <- unlist(sapply(unique(model$smMatrix[,2]), function(x) model$mmMatrix[model$mmMatrix[, "construct"] == x,"measurement"]), use.names = FALSE)

      # mean the in-sample lm predictions by row
      average_insample_lm <- sapply(1:length(endogenous_items), mean_rows, matrix = in_sample_lm_matrix,
                                    noFolds = noFolds,
                                    constructs = endogenous_items)

      # sum the out-sample item predictions by row
      average_outsample_lm <- sapply(1:length(endogenous_items), sum_rows, matrix = out_sample_lm_matrix,
                                     noFolds = noFolds,
                                     constructs = endogenous_items)

      # square the out-sample lm residuals, mean them, and take square root
      average_insample_lm_item_residuals <- sqrt(sapply(1:length(endogenous_items), mean_rows, matrix = LM_in_sample_item_residuals^2,
                                                        noFolds = noFolds,
                                                        constructs = endogenous_items))

      colnames(average_insample_construct) <- colnames(average_outsample_construct) <- model$constructs
      colnames(average_insample_item) <- colnames(average_insample_pls_item_residuals) <- colnames(average_outsample_item) <- no_int_mmvars
      colnames(average_insample_lm) <- colnames(average_outsample_lm) <- colnames(average_insample_lm_item_residuals) <- endogenous_items

      return(list(out_of_sample_construct = average_outsample_construct,
                  in_sample_construct = average_insample_construct,
                  out_of_sample_item = average_outsample_item,
                  in_sample_item = average_insample_item,
                  out_of_sample_lm_item = average_outsample_lm,
                  in_sample_lm_item = average_insample_lm,
                  pls_in_sample_item_residuals = average_insample_pls_item_residuals,
                  lm_in_sample_item_residuals = average_insample_lm_item_residuals))
    },
    error=function(cond) {
      message("Parallel encountered this ERROR: ")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    warning=function(cond) {
      message("Parallel encountered this WARNING:")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    finally={
      #
    }
  )
}

# Function to return the RMSE and MAE of a score
prediction_metrics <- function(residuals) {
  RMSE <- sqrt(mean(residuals^2))
  MAE <- mean(abs(residuals))
  return(matrix(c(RMSE,MAE), nrow = 2, ncol = 1, byrow = TRUE))
}

predict_lm_matrices <- function(x, depTrainData, indepTrainData,indepTestData, endogenous_items) {
  # Train LM
  trainLM <- stats::lm(depTrainData[,x] ~ ., indepTrainData)
  # Predict out of sample
  lmprediction_out_sample <- stats::predict(trainLM, newdata = indepTestData)
  # Predict in sample
  lmprediction_in_sample <- stats::predict(trainLM, newdata = indepTrainData)
  return(list(lm_prediction_in_sample = lmprediction_in_sample,
              lm_prediction_out_sample = lmprediction_out_sample))
}

generate_lm_predictions <- function(x, model, ordered_data, testIndexes, endogenous_items, trainIndexes, technique) {
  # Extract the target and non-target variables for Linear Model
  dependant_items <- model$mmMatrix[model$mmMatrix[,1] == x,2]

  # Create matrix return object holders
  in_sample_matrix <- matrix(0,nrow = nrow(ordered_data), ncol = length(dependant_items), dimnames = list(rownames(ordered_data),dependant_items))
  out_sample_matrix <- matrix(0,nrow = nrow(ordered_data), ncol = length(dependant_items), dimnames = list(rownames(ordered_data),dependant_items))

  # Select the correct independent variables to be included in independent matrix
  # for predict_DA this would be the indicators of the direct antecedents only
  # for predict_EA this would be the indicators of the earliest antecedents only
  if (identical(technique, predict_DA)) {
    focal_construct_antecedents <- antecedents_of(x, model$smMatrix)
    focal_construct_antecedent_items <- unlist(sapply(focal_construct_antecedents, function (focal) construct_indicators(focal, model$mmMatrix)))
  }
  else {
    focal_construct_antecedents <- only_exogenous(model$smMatrix)
    focal_construct_antecedent_items <- unlist(sapply(focal_construct_antecedents, function (focal) construct_indicators(focal, model$mmMatrix)))
  }
  independant_matrix <- ordered_data[ , focal_construct_antecedent_items,drop = F]
  dependant_matrix <- as.matrix(ordered_data[,dependant_items, drop = F])

  # Create independant items matrices - training and testing
  indepTestData <- independant_matrix[testIndexes, ,drop = F]
  indepTrainData <- independant_matrix[-testIndexes, ,drop = F]

  # Create dependant matrices - training and testing
  # if (length(testIndexes) == 1) {
    # depTestData <- t(as.matrix(dependant_matrix[testIndexes, ,drop = F]))
  # } else {
    depTestData <- as.matrix(dependant_matrix[testIndexes, ,drop = F])
  # }
  depTrainData <- as.matrix(dependant_matrix[-testIndexes, ])
  colnames(depTrainData) <- colnames(depTestData) <- dependant_items

  lm_prediction_list <- sapply(dependant_items, predict_lm_matrices, depTrainData = depTrainData,
                               indepTrainData = indepTrainData,
                               indepTestData = indepTestData,
                               endogenous_items = endogenous_items)
  in_sample_matrix[trainIndexes,] <- matrix(unlist(lm_prediction_list[(1:length(lm_prediction_list))[1:length(lm_prediction_list)%%2==1]]), ncol = length(dependant_items), nrow = nrow(depTrainData), dimnames = list(rownames(depTrainData),dependant_items))
  out_sample_matrix[testIndexes,] <- matrix(unlist(lm_prediction_list[(1:length(lm_prediction_list))[1:length(lm_prediction_list)%%2==0]]), ncol = length(dependant_items), nrow = nrow(depTestData), dimnames = list(rownames(depTestData),dependant_items))

  return(list(in_sample_matrix, out_sample_matrix))
}

#' Predictive Scheme
#'
#' \code{predict_EA} and \code{predict_DA} specify the predictive scheme to be used in the generation of the
#' predictions. EA refers to Earliest Antecedents nad DA to Direct Antecedents.
#'
#' @param smMatrix is the \code{structural_model} - a source-to-target matrix representing the inner/structural model,
#'  generated by \code{relationships} generated by SEMinR.
#'
#' @param path_coef is the Path Coefficients matrix from a SEMinR model.
#'
#' @param construct_scores is the matrix of construct scores generated by SEMinR.
#'
#' @usage
#'  predict_EA(smMatrix, path_coef, construct_scores)
#'
#' @export
predict_EA <- function(smMatrix, path_coef, construct_scores) {
  order <- construct_order(smMatrix)
  only_exogenous <- setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))
  return_matrix <- construct_scores
  return_matrix[,order] <- 0
  for (construct in order) {
    return_matrix[,construct] <- return_matrix %*% path_coef[,construct]

  }
  return(return_matrix)
}

#' Predictive Scheme
#'
#' \code{predict_EA} and \code{predict_DA} specify the predictive scheme to be used in the generation of the
#' predictions. EA refers to Earliest Antecedents nad DA to Direct Antecedents.
#'
#' @param smMatrix is the \code{structural_model} - a source-to-target matrix representing the inner/structural model,
#'  generated by \code{relationships} generated by SEMinR.
#'
#' @param path_coef is the Path Coefficients matrix from a SEMinR model.
#'
#' @param construct_scores is the matrix of construct scores generated by SEMinR.
#'
#' @usage
#'  predict_DA(smMatrix, path_coef, construct_scores)
#'
#' @export
predict_DA <- function(smMatrix, path_coef, construct_scores) {
  only_exogenous <- setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))
  return_matrix <- construct_scores%*%path_coef
  return_matrix[,only_exogenous] <- construct_scores[,only_exogenous]
  return(return_matrix)
}

return_predict_error <- function(object, indicator) {
  object$prediction_error[,indicator]
}
