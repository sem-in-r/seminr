# Takes a HOC name and replaces that constructs relationships with the dimensions of the HOC
substitute_dimensions_for_HOC <- function(construct, sm,mm) {
  # Identify dimensions of HOCs
  dimensions <- mm[mm[,"type"] == "HOCA" | mm[,"type"] == "HOCB",][mm[mm[,"type"] == "HOCB" | mm[,"type"] == "HOCA",][,"construct"] == construct,"measurement"]
  # identify antecedent relationships to HOC
  antecedents <- sm[which(sm[,"target"] == construct),"source"]
  # change antecedent relationship to first order constructs in structural model
  sm <- rbind(sm,
              relationships(paths(from = antecedents,
                                  to = dimensions)))
  sm <- sm[-which(sm[,"target"] == construct),]
  # identify outcomes
  outcomes <- sm[which(sm[,"source"] == construct),"target"]
  sm <- rbind(sm,
              relationships(paths(from = dimensions,
                                  to = outcomes)))
  sm <- sm[-which(sm[,"source"] == construct),]
  return(sm)
}

remove_HOC_in_measurement_model <- function(construct,mm) {
  mm[!mm[,"construct"] == construct,]
}

# Function to parse measurement and structural model and create the higher order model with complete information

prepare_higher_order_model <- function(data,sm , mm,
                                       ints = NULL,
                                       inners = NULL) {
  # Check for HOC
  if ("HOCA" %in% mm[,"type"] | "HOCB" %in% mm[,"type"] ) {

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
    new_model <- estimate_pls(data = mobi,
                              measurement_model = new_mm,
                              interactions = ints,
                              structural_model = sm,
                              inner_weights = inners)

    # Add the construct scores to data
    data <- cbind(data, new_model$construct_scores[,dimensions])

    # pass the updated mm, sm and data back to estimate_model()
    return(list(data = data,
                sm = sm,
                mm = mm))
  }
}

two_stage_hierarchical_construct <- function(construct_name, dimensions, weights = correlation_weights) {
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

estimate_pls <- function(data, measurement_model, interactions=NULL, structural_model, inner_weights = path_weighting) {
  cat("Generating the seminr model\n")
  warnings(measurement_model, data, structural_model)
  data <- stats::na.omit(data)
  rawdata <- data
  raw_measurement_model <- measurement_model
  if(!is.null(interactions)) {
    # update data with new interaction items
    intxns_list <- interactions(data, measurement_model)
    get_data <- function(intxn) { intxn$data }
    interaction_data <- do.call("cbind", lapply(intxns_list, get_data))

    # Append data with interaction data
    data <- cbind(data, interaction_data)

    # update measurement model with interaction constructs
    measure_interaction <- function(intxn) {
      composite(intxn$name, names(intxn$data),weights = mode_A)
    }
    intxns_mm <- constructs(do.call("c", lapply(intxns_list, measure_interaction)))
    measurement_model <- rbind(measurement_model, intxns_mm)
  }

  # warning if the model is incorrectly specified
  warning_struc_meas_model_complete(structural_model,measurement_model,data)

  # Make a named list of construct measurement_mode functions
  measurement_mode_scheme <- sapply(unique(c(structural_model[,1],structural_model[,2])), get_measure_mode, measurement_model, USE.NAMES = TRUE)

  # Run the model in simplePLS
  seminr_model = seminr::simplePLS(obsData = data, smMatrix = structural_model, mmMatrix = measurement_model, inner_weights = inner_weights, measurement_mode_scheme = measurement_mode_scheme)
  seminr_model$data <- data
  seminr_model$interactions <- interactions
  seminr_model$rawdata <- rawdata
  seminr_model$raw_measurement_model <- raw_measurement_model

  # Correct for Bias in Reflective models using PLS Consistent
  seminr_model <- model_consistent(seminr_model)

  class(seminr_model) <- "seminr_model"
  return(seminr_model)
}
