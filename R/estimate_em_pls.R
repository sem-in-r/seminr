#' seminr estimate_em_pls() function
#' Estimates an existing model using EM-PLS-SEM
#' fixed implementation, based on: https://github.com/ShanLu92/EM_PLS-SEM.git
#' citation: Wang, H., Lu, S., & Liu, Y. (2022). Missing data imputation in PLS-SEM. Quality & Quantity, 56(6), 4777-4795.
#' 
#' orginal code:
#' rm(list=ls())
#'
#' library(seminr)
#' library(DMwR2)
#' library(mice) 
#' 
#' # define the model structure as Seminr suggested
#' # here we are using the European Customer Satisfaction Index model
#' dd_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
#'   composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
#'   composite("Quality",      multi_items("PERQ", 1:7), weights = mode_A),
#'   composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
#'   composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A),
#'   composite("Complaints",   single_item("CUSCO")),
#'   composite("Loyalty",      multi_items("CUSL", 1:3), weights = mode_A)
#' ) 
#' 
#' dd_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#' 
#' 
#' n = 100
#' cs_times = 501 # iteration for EM PLS-SEM
#' threshold = 1e-04
#' 
#' rdm_times = 200
#' 
#' set.seed(121)
#' 
#' for (na_pcent in c(0.01, 0.02, .04, 0.08, 0.12, 0.16)){
#'   result = data.frame(matrix(ncol = rdm_times, nrow = 7))
#'   print (na_pcent)
#'   for (rdm in 1:rdm_times){
#'     
#'     ##### simulate datasets with missing values #####
#'     mobi = read.csv(paste0(toString(rdm), '_simul_esci_n=', toString(n), '.csv'))
#'     mobi = mobi[,-1]
#'     dd = scale(mobi)
#'     # head(dd)
#'     
#'     suppressMessages(invisible(capture.output(plsm <- estimate_pls(data = dd,
#'                                                                    measurement_model = dd_mm,
#'                                                                    structural_model = dd_sm))))
#'     
#'     mmMatrix = plsm$mmMatrix
#'     smMatrix = plsm$smMatrix
#'     
#'     scorenames = c("Image", "Expectation", "Quality", 
#'                    "Value", "Satisfaction", "Loyalty")
#'     
#'     na_n = ceiling(nrow(dd)*ncol(dd)*na_pcent)
#'     na_loc = sample(nrow(dd)*ncol(dd), na_n)
#'     
#'     na_row = na_loc%/%ncol(dd)+1
#'     na_col = na_loc%%ncol(dd)+1 # start from 1
#'     
#'     dd = data.frame(scale(mobi))
#'     
#'     ##############  EM PLS-SEM ########################
#'     # randomly assign values to empty cells from the available values
#'     start_time <- Sys.time()
#'     
#'     na_value_real = c()
#'     for (i in 1:length(na_n)){
#'       na_value_real = append(na_value_real, dd[na_row[i], na_col[i]])
#'       dd[na_row[i], na_col[i]] = sample(dd[, na_col[i]], 1)
#'     }
#'     
#'     # inner_mae_old = 100 # if use two consecutive inner mae diff<1e-06, then it alway yeils small mae in 1501 iteration
#'     for (cs in 1:cs_times){
#'       
#'       suppressMessages(invisible(capture.output(
#'         nan_plsm <- estimate_pls(data = dd,
#'                                  measurement_model = dd_mm,
#'                                  structural_model = dd_sm)
#'       )))
#'       
#'       for (i in 1:length(na_n)){
#'         
#'         na_colname_now = colnames(dd)[na_col[i]]
#'         na_row_now = na_row[i]
#'         na_scorename_now = mmMatrix[mmMatrix[,2]==na_colname_now, 'construct']
#'         mvars_name = mmMatrix[mmMatrix[,1]==na_scorename_now, 'measurement']
#'         na_score = nan_plsm$construct_scores[, na_scorename_now]
#'         
#'         if (length(mvars_name)>1){
#'           # fit jth col first, fixed others
#'           # variable names related to the scores, not include the missing value variable
#'           mvars_name = setdiff(mvars_name, na_colname_now) 
#'           # variable outer weights related to the scores, not include the missing value variable
#'           mvar_weight = nan_plsm$outer_weights[mvars_name, na_scorename_now]
#'           # variable outer weights of the missing value 
#'           na_weight = nan_plsm$outer_weights[na_colname_now, na_scorename_now]
#'           # in composite design, the w_ij * x_ij = scores -sum(other x_ij * outer_weights)
#'           na_fill = as.numeric(na_score[na_row_now])
#'           
#'           for (j in 1:length(mvars_name)){
#'             na_fill = na_fill - as.numeric(mvar_weight[j] * dd[na_row_now, mvars_name[j]])
#'           }
#'           na_fill = na_fill/na_weight
#'           dd[na_row_now, na_colname_now] = na_fill
#'         }else{#some LV has only one MV: nan_plsm$outer_weights[na_colname_now, na_scorename_now]=1
#'           na_fill = as.numeric(na_score[na_row_now])
#'         }
#'       }
#'       outer_mae_new = (1/nrow(mmMatrix))*sum(abs(nan_plsm$outer_weights - plsm$outer_weights))
#'       inner_mae_new = (1/nrow(smMatrix))*sum(abs(nan_plsm$path_coef - plsm$path_coef))
#'       
#'       # this round and last round inner weights difference < 1e06, then stop
#'       if (length(inner_mae_new)>1){
#'         if (abs(inner_mae_new) < 1e-06 ) break
#'       }
#'       # inner_mae_old = inner_mae_new
#'     }
#'     
#'     end_time <- Sys.time()
#'     
#'     na_value_impute = c()
#'     for (i in 1:length(na_n)){
#'       na_value_impute = append(na_value_impute, dd[na_row[i], na_col[i]])
#'     }
#'     
#'     # the mae of outer weights
#'     result[1,rdm] = outer_mae_new
#'     # the mae of inner weights
#'     result[2,rdm]= inner_mae_new
#'     result[3,rdm] = sum(abs(nan_plsm$rSquared[1,]-plsm$rSquared[1,]))
#'     result[4,rdm] = sum(abs(nan_plsm$rSquared[2,]-plsm$rSquared[2,]))
#'     result[5,rdm] = cs
#'     result[6,rdm] = (1/length(na_value_impute))*sum(abs(na_value_impute-na_value_real))
#'     result[7,rdm] = end_time - start_time
#'   }
#'   
#'   rownames(result) = c('outer_mae', 'inner_mae',
#'                        'rsqua', 'rsqua_adjust',
#'                        'cs', 'real-impute_mae',
#'                        'time')
#'   
#'   write.csv(result, paste0('EM_n=',toString(n),'_cs=',toString(cs), 
#'                            '_threshold=', toString(threshold),
#'                            '_percent=',toString(na_pcent),'_result.csv'))
#'   
#' }
#'
#' @param pls_model
#'   An existing seminr PLS-SEM model
#'
#' @param seed
#'   A seed for the random number initialization. Default is 123
#'
#' @param threshold
#'.  If two consecutive iterations differ less than this threshold, abort
#'
#' @param cs_times
#'   At maximum cs_times iterations
#'
#' @return A list of the estimated parameters for the SEMinR model including:
#'  \item{meanData}{A vector of the indicator means.}
#'  \item{sdData}{A vector of the indicator standard deviations}
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
#' estimate_em_pls(pls_model,
#'              measurement_model = NULL, structural_model = NULL, model = NULL,
#'              inner_weights = path_weighting,
#'              missing = mean_replacement,
#'              missing_value = NA,
#'              maxIt = 300,
#'              stopCriterion = 7)
#'
#' @seealso \code{\link{specify_model}} \code{\link{relationships}} \code{\link{constructs}} \code{\link{paths}} \code{\link{interaction_term}}
#'          \code{\link{bootstrap_model}}
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
#'                          structural_model = mobi_sm,
#'                          missing = mean_replacement,
#'                          missing_value = NA)
#'                          
#' mobi_em_pls <- estimate_em_pls(mobi_pls,
#'                                seed = 123,
#'                                threshold = 1e-04,
#'                                cs_times = 501
#' )
#'
#' summary(mobi_em_pls)
#' plot_scores(mobi_em_pls)
#' @export
estimate_em_pls <- function(pls_model, seed, threshold, cs_times) {
  if(missing(pls_model)) {
    stop("Parameter pls_model is missing: A valid seminr model is necessary")
  }
  if(sum(is.na(pls_model$rawdata)) == 0) {
    stop("No missing values in model!")
  }
  if(missing(seed)) {
    seed = 123
  }
  if(missing(threshold)) {
    threshold = 1e-04
  }
  if(missing(cs_times)) {
    cs_times = 501
  }
  
  library(seminr)
  ##### simulate datasets with missing values #####
  dd <- scale(pls_model$rawdata[,-1])
  
  mmMatrix = pls_model$mmMatrix
  smMatrix = pls_model$smMatrix
  
  na_n = length(which(is.na(dd)))
  na_loc = which(is.na(dd))
  
  na_row = which(is.na(dd), arr.ind=TRUE)[,1]
  na_col = which(is.na(dd), arr.ind=TRUE)[,2]
  
  dd = data.frame(scale(dd))
  
  ##############  EM PLS-SEM ########################
  # randomly assign values to empty cells from the available values
  
  na_value_real = c()
  for (i in 1:na_n) {
    dd[na_row[i], na_col[i]] = sample(dd[, na_col[i]], 1)
  }
  # inner_mae_old = 100 # if use two consecutive inner mae diff<1e-06, then it alway yeils small mae in 1501 iteration
  for (cs in 1:cs_times) {
    
    suppressMessages(invisible(capture.output(
      nan_plsm <- estimate_pls(data = dd,
                               measurement_model = pls_model$measurement_model,
                               structural_model = pls_model$structural_model)
    )))
    
    for (i in 1:na_n) {
      
      na_colname_now = colnames(dd)[na_col[i]]
      na_row_now = na_row[i]
      na_scorename_now = mmMatrix[mmMatrix[,2]==na_colname_now, 'construct']
      mvars_name = mmMatrix[mmMatrix[,1]==na_scorename_now, 'measurement']
      na_score = nan_plsm$construct_scores[, na_scorename_now]
      
      if (length(mvars_name)>1){
        # fit jth col first, fixed others
        # variable names related to the scores, not include the missing value variable
        mvars_name = setdiff(mvars_name, na_colname_now) 
        # variable outer weights related to the scores, not include the missing value variable
        mvar_weight = nan_plsm$outer_weights[mvars_name, na_scorename_now]
        # variable outer weights of the missing value 
        na_weight = nan_plsm$outer_weights[na_colname_now, na_scorename_now]
        # in composite design, the w_ij * x_ij = scores -sum(other x_ij * outer_weights)
        na_fill = as.numeric(na_score[na_row_now])
        
        for (j in 1:length(mvars_name)){
          na_fill = na_fill - as.numeric(mvar_weight[j] * dd[na_row_now, mvars_name[j]])
        }
        na_fill = na_fill/na_weight
        dd[na_row_now, na_colname_now] = na_fill
      }else{#some LV has only one MV: nan_plsm$outer_weights[na_colname_now, na_scorename_now]=1
        na_fill = as.numeric(na_score[na_row_now])
      }
    }
    if (exists("pls_old")) {
      outer_mae_new = (1/nrow(mmMatrix))*sum(abs(nan_plsm$outer_weights - pls_old$outer_weights))
      inner_mae_new = (1/nrow(smMatrix))*sum(abs(nan_plsm$path_coef - pls_old$path_coef))
      pls_old <- nan_plsm
    }
    else {
      outer_mae_new = 1
      inner_mae_new = 1
      pls_old <- nan_plsm
    }
    if (cs %% 10 == 0) {
      print(paste("round", cs, "outer mae", outer_mae_new, "inner mae", inner_mae_new))
    }    
    # this round and last round inner weights difference < 1e06, then stop
    if (!is.na(inner_mae_new)) {
      if (abs(inner_mae_new) < 1e-04) { 
        break
      }
    }
    # inner_mae_old = inner_mae_new
  }
  return(nan_plsm)
}