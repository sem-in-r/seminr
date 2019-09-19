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
