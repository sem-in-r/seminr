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
prepare_higher_order_model <- function(data, sm , mm, inners, HOCs) {
  #retain the mm and sm
  orig_mm <- mm
  new_mm <- matrix(unlist(mm[!(substr(names(mm), nchar(names(mm))-10, nchar(names(mm))) == "interaction") & !(names(mm) == "higher_order_composite")]), ncol = 3, byrow = TRUE,
         dimnames = list(NULL, c("construct", "measurement", "type")))
  orig_sm <- sm
  # Rebuild model for first stage
  # Add new HOC paths to SM
  dimensions <- c()
  for (construct in HOCs) {
    obj <- substitute_dimensions_for_HOC(construct, sm, new_mm)
    sm <- obj$sm
    dimensions <- c(dimensions, obj$dimensions)
  }
  # Remove interactions from the sm
  sm <- sm[sm[, "source"] %in% unique(new_mm[, "construct"]),]


  # Identify all the dimensions
  # dimensions <- orig_mm[which(orig_mm[, "construct"] == HOCs), "measurement"]

  # Run first stage
  new_model <- estimate_pls(data = data,
                            measurement_model = mm[!(substr(names(mm), nchar(names(mm))-10, nchar(names(mm))) == "interaction") & !(names(mm) == "higher_order_composite")],
                            structural_model = sm,
                            inner_weights = inners)

  # Add the construct scores to data
  data <- cbind(data, new_model$construct_scores[, dimensions])

  # # Update the mm to include the type of the new data and item
  # mm[mm[,"type"] == "HOCA", "type"] <- "A"
  # mm[mm[,"type"] == "HOCB", "type"] <- "B"


  # pass the updated mm, sm and data back to estimate_model()
  return(list(data = data,
              sm = orig_sm,
              mm = mm))
}
