# Takes a HOC name and replaces that constructs relationships with the dimensions of the HOC
substitute_dimensions_for_HOC <- function(construct, sm, mm) {
  # Identify dimensions of HOCs
  dimensions <- matrix(construct, ncol = 3, byrow = TRUE)[,2]
  #dimensions <- mm[mm[, "type"] == "HOCA" | mm[, "type"] == "HOCB", ][mm[mm[, "type"] == "HOCB" | mm[, "type"] == "HOCA", ][, "construct"] == construct, "measurement"]
  # identify antecedent relationships to HOC
  antecedents <- sm[which(sm[, "target"] == construct[1]), "source"]
  # change antecedent relationship to first order constructs in structural model
  if (!length(antecedents) == 0) {
    sm <- rbind(sm,
                relationships(paths(from = antecedents,
                                    to = dimensions)))
    sm <- sm[-which(sm[, "target"] == construct[1]), ]
  }

  # identify outcomes
  outcomes <- sm[which(sm[, "source"] == construct[1]), "target"]
  if (!length(outcomes) == 0) {
    sm <- rbind(sm,
                relationships(paths(from = dimensions,
                                    to = outcomes)))
    sm <- sm[-which(sm[, "source"] == construct[1]), ]
  }
  return(list(sm = sm,
              dimensions = dimensions))
}

remove_HOC_in_measurement_model <- function(construct, mm) {
  mm[!mm[, "construct"] == construct, ]
}

# Function to parse measurement and structural model and create the higher order model with complete information
prepare_higher_order_model <- function(data, sm , mm, inners, HOCs, maxIt, stopCriterion) {
  #retain the mm and sm
  orig_mm <- mm
  new_mm <- matrix(unlist(mm[!(substr(names(mm), nchar(names(mm))-10, nchar(names(mm))) == "interaction") & !(names(mm) == "higher_order_composite")]), ncol = 3, byrow = TRUE,
         dimnames = list(NULL, c("construct", "measurement", "type")))
  orig_sm <- sm


  # Rebuild model for first stage
  # Add new HOC paths to SM
  dimensions <- c()
  for (construct in HOCs) {
    if (construct[[1]] %in% unique(as.vector(orig_sm))) {
      obj <- substitute_dimensions_for_HOC(construct, sm, new_mm)
      sm <- obj$sm
      dimensions <- c(dimensions, obj$dimensions)
    }
  }
  # Remove interactions from the sm
  sm <- sm[sm[, "source"] %in% unique(new_mm[, "construct"]),]


  # Identify all the dimensions
  # dimensions <- orig_mm[which(orig_mm[, "construct"] == HOCs), "measurement"]

  # Run first stage
  new_model <- estimate_pls(data = data,
                            measurement_model = mm[!(substr(names(mm), nchar(names(mm))-10, nchar(names(mm))) == "interaction") & !(names(mm) == "higher_order_composite")],
                            structural_model = sm,
                            inner_weights = inners,
                            maxIt = maxIt,
                            stopCriterion = stopCriterion)

  # Add the construct scores to data
  data <- cbind(data, new_model$construct_scores[, dimensions])

  # # Update the mm to include the type of the new data and item
  # mm[mm[,"type"] == "HOCA", "type"] <- "A"
  # mm[mm[,"type"] == "HOCB", "type"] <- "B"


  # pass the updated mm, sm and data back to estimate_model()
  return(list(data = data,
              sm = orig_sm,
              mm = mm,
              first_stage_model = new_model))
}

# Returns all Higher Order Constructs (HOCs) from provided model specifications
HOCs_in_model <- function(measurement_model, structural_model = NULL) {
  # Extract HOCs from measurement model
  HOCs <- measurement_model[grepl("higher_order_", names(measurement_model))]
  if (is.null(structural_model)) return(HOCs)

  # Get subset of HOCs also present in structural model, if one is provided
  if (length(HOCs) > 0) {
    output <- list()
    j <- 1
    for (i in 1:length(HOCs)) {
      if(HOCs[[i]][1] %in% construct_names(structural_model)) {
        output[[j]] <- HOCs[[i]]
        j <- j + 1
      }
    }
  } else {
    output <- c()
  }

  return(output)
}

# Function to parse first stage and second stage model and combine the measurement model matrices
combine_first_order_second_order_matrices <- function(model1, model2, mmMatrix) {

  # Generate a vector of indicators and constructs from both stages of HOC
  appended_mmVariables <- unique(c(model2$mmVariables, model1$mmVariables))
  appended_constructs <- unique(c(model2$constructs, model1$constructs))

  # Generate a vector of only HOC indicators and constructs
  HOC_items <- setdiff(model1$mmVariables, model2$mmVariables)
  HOC_constructs <- setdiff(model1$constructs, model2$constructs)

  # Initialize a new matrix for measurement model including both LOC and HOC items
  weights_matrix <- matrix(data=0,
                           nrow=length(appended_mmVariables),
                           ncol=length(appended_constructs),
                           dimnames = list(appended_mmVariables,appended_constructs))
  for (i in 1:length(appended_constructs))  {
    weights_matrix[mmMatrix[mmMatrix[, "construct"]==appended_constructs[i], "measurement"], appended_constructs[i]] =1
  }

  # Calculate new loadings matrix
  # Parse the old matrices from stage 1 and stage 2 models and assign the correct loadings
  new_loadings <- weights_matrix
  for (row_it in rownames(model2$outer_loadings)) {
    for (col_it in colnames(model2$outer_loadings)) {
      new_loadings[row_it, col_it] <- model2$outer_loadings[row_it, col_it]
    }
  }
  for (row_it in HOC_items) {
    for (col_it in HOC_constructs) {
      new_loadings[row_it, col_it] <- model1$outer_loadings[row_it, col_it]
    }
  }

  # Calculate new weights matrix
  # Parse the old matrices from stage 1 and stage 2 models and assign the correct weights
  new_weights <- weights_matrix
  for (row_it in rownames(model2$outer_weights)) {
    for (col_it in colnames(model2$outer_weights)) {
      new_weights[row_it, col_it] <- model2$outer_weights[row_it, col_it]
    }
  }
  for (row_it in HOC_items) {
    for (col_it in HOC_constructs) {
      new_weights[row_it, col_it] <- model1$outer_weights[row_it, col_it]
    }
  }
  return(list(new_outer_weights = new_weights,
              new_outer_loadings = new_loadings))
}

combine_first_order_second_order_loadings_cbsem <- function(mmMatrix, rawdata, lavaan_std) {
  # constructs used to measure HOCs
  hoc_measure_constructs <- setdiff(mmMatrix[,"measurement"], names(rawdata))

  HOCs <- mmMatrix[which(mmMatrix[,"measurement"] %in% hoc_measure_constructs),]
  HOC_names <- unique(HOCs[,"construct"])

  HOC_measures <- lapply(stats::setNames(HOC_names, HOC_names),
                         function(name) { HOCs[HOCs[, "construct"] == name, "measurement"] })

  loadings <- lavaan_std$lambda
  class(loadings) <- "matrix"

  new_loadings <- rbind(loadings,
                        matrix(data=0,
                               nrow=length(hoc_measure_constructs),
                               ncol=ncol(loadings)))

  rownames(new_loadings) <- c(rownames(loadings), hoc_measure_constructs)

  betas <- lavaan_std$beta
  class(betas) <- "matrix"

  for (hoc in HOC_names) {
    new_loadings[HOC_measures[[hoc]],hoc] <- betas[HOC_measures[[hoc]],hoc]
  }

  return(new_loadings)
}
