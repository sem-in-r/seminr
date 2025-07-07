## Goodness-of-fit ----

# Calculate insample metrics ----
metrics_insample <- function(obsData, construct_scores, smMatrix, dependant, construct_score_cors) {
  # create matrix return item
  insample <- matrix(, nrow=2, ncol=length(dependant), byrow =TRUE, dimnames = list(c("Rsq", "AdjRsq"), dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant <- smMatrix[smMatrix[, "target"]==dependant[i], "source"]

    #Calculate insample for endogenous
    r_sq <- 1 - 1/solve(construct_score_cors[c(independant, dependant[i]), c(independant, dependant[i])])
    insample[1, i] <- r_sq[dependant[i], dependant[i]]
    insample[2, i] <- 1 - (1 - insample[1, i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
  }
  return(insample)
}


# ## Wetware errors ----
# # Load the data
# corp_rep_data <- corp_rep_data
#
# # Create measurement model ----
# corp_rep_mm_ext <- constructs(
#   composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
#   composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
#   composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
#   composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
#   composite("COMP", multi_items("comp_", 1:3)),
#   composite("LIKE", multi_items("like_", 1:3)),
#   interaction_term("COMP", "LIKE"),
#   composite("CUSA", single_item("cusa")),
#   composite("CUSL", multi_items("cusl_", 1:3))
# )
#
#
#
# ### SM constructs occur in the mm
# # Should trigger:
# corp_rep_sm_ext <- relationships(
#   paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
#   paths(from = c("COMP", "LIKE", "COMP*LIKE"), to = c("CUSA", "CUSL")),
#   paths(from = c("CUSA"),         to = c("CUSL"))
# )
#
# # Check the measurement and structural specification accuracy  ----
# measurement_model <- corp_rep_mm_ext
# structural_model <- corp_rep_sm_ext
# identical(all_construct_names(measurement_model), construct_names(structural_model))
#
#
# # Should not:
# corp_rep_sm_ext <- relationships(
#   paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
#   paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
#   paths(from = c("CUSA"),         to = c("CUSL"))
# )

### interaction includes all direct effects
### no "*"s or "."s in the data
# smMatrix <- structural_model
direct_effects_are_specified <- function(smMatrix) {
  log_vec <- c(FALSE)
  if (any(grepl("\\*", smMatrix))) {
    # first identify all the interaction terms
    ints <- construct_names(smMatrix)[grepl("\\*", construct_names(smMatrix))]
    for(con in ints) {
      outcomes <- smMatrix[smMatrix[,"source"] == con,"target"]
      for (outs in outcomes) {
        ants <- smMatrix[smMatrix[,"target"] == outs,"source"]
        end_lv_one <- regexpr("\\*", con)[1]
        lv_one <- substring(con,0,end_lv_one-1)
        lv_two <- substring(con,end_lv_one+1,nchar(con))
        output <- !all(c(lv_one, lv_two) %in% ants)
        log_vec <- c(log_vec, output)
      }
    }
  }
  return(any(log_vec))
}

### item names consistent throughout and with data
all_indicator_names_are_in_data <- function(measurement_model,
                                        data) {
  return(all(all_items(measurement_model) %in% colnames(data)))
}

### latent names constant throughout
### SM constructs occur in the mm
### constructs do not share name with items
construct_names_are_valid <- function(measurement_model,
                                  structural_model) {
  # remove interactions from the list (not created yet)
  sm_constructs <- construct_names(structural_model)
  sm_constructs <- sm_constructs[!grepl("\\*", sm_constructs)]
  mm_constructs <- all_construct_names(measurement_model)

  # construct names in sm DO occur in mm and are spelled correct
  construct_named_correcty <- all(sm_constructs %in% mm_constructs)

  # construct names do not occur in the indicator names
  construct_item_named_same <- any(sm_constructs %in% mm_constructs)

  return(!construct_named_correcty | !construct_item_named_same)
}

# Feature to automate model specification quality ----
assess_model_specification <- function(measurement_model,
                                   structural_model,
                                   data){

  # Check the model specification
  if (construct_names_are_valid(measurement_model,
                                structural_model)) {
    stop("There is a mismatch in the names of your constructs.
    Please confirm that:
      (1) the construct names in the measurement model are correcly spelled and specified;
      (2) the construct names in your structural model are correctly spelled and specified;
      (3) all the construct names in your structural model are specified in the measurement model.
      Please note that plot(measurement_model) or plot(structural_model) help in visualizing the problem.
      Model cannot be estimated.")
  }
  if(!all_indicator_names_are_in_data(measurement_model,
                                  data)) {
    stop("There is a mismatch in the names of your indicators and data.
    Please confirm that:
      (1) the indicator names in the measurement model are correcly spelled and specified;
      (2) the names of the items in your data (colnames) are correctly spelled and specified.
      (3) there are no strange characters in the indicator names (* not allowed).
      Please note that plot(measurement_model) or plot(structural_model) help in visualizing the problem.
      Model cannot be estimated.")
  }
  if(direct_effects_are_specified(structural_model)) {
    stop("It appears that you have not specified both IV and MV as direct effects in the structural model.
   Please confirm that:
      (1) the construct names in the measurement model are correcly spelled and specified;
      (2) the construct names in the structural model are correcly spelled and specified;
      (3) the IV and MV for the interaction are both specified as direct effects.
      Please note that plot(measurement_model) or plot(structural_model) help in visualizing the problem.
      Model cannot be estimated.")
  }
  return()
}
