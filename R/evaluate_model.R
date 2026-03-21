## Goodness-of-fit ----

# Calculate insample metrics ----
metrics_insample <- function(obsData, construct_scores, smMatrix, dependant, construct_score_cors) {
  # create matrix return item
  insample <- matrix(, nrow=2, ncol=length(dependant), byrow =TRUE, dimnames = list(c("Rsq", "AdjRsq"), dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant <- construct_antecedents(smMatrix, dependant[i])

    #Calculate insample for endogenous
    r_sq <- 1 - 1/solve(construct_score_cors[c(independant, dependant[i]), c(independant, dependant[i])])
    insample[1, i] <- r_sq[dependant[i], dependant[i]]
    insample[2, i] <- 1 - (1 - insample[1, i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
  }

  insample
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

# Feature to automate model specification quality ----
assess_model_specification <- function(measurement_model,
                                       structural_model,
                                       data) {

  # Check the model specification
  if (are_construct_names_misspelled(measurement_model, structural_model)) {
    stop("Some construct names in the structural model were not found in the measurement model.\n",
         "Please confirm that all construct names are correctly spelled and specified.\n",
         "Note: plot(measurement_model) or plot(structural_model) can help visualize the problem.\n",
         "Model cannot be estimated.")
  }
  if (are_construct_names_colliding(measurement_model, structural_model)) {
    stop("Some construct names are the same as indicator/item names.\n",
         "Construct names must not collide with indicator names.\n",
         "Note: plot(measurement_model) or plot(structural_model) can help visualize the problem.\n",
         "Model cannot be estimated.")
  }
  if (!are_indicators_in_data(measurement_model,
                              data)) {
    stop("There is a mismatch in the names of your indicators and data.
    Please confirm that:
      (1) the indicator names in the measurement model are correcly spelled and specified;
      (2) the names of the items in your data (colnames) are correctly spelled and specified.
      (3) there are no strange characters in the indicator names (* not allowed).
      Please note that plot(measurement_model) or plot(structural_model) help in visualizing the problem.
      Model cannot be estimated.")
  }
  if (has_direct_effects(structural_model)) {
    stop("It appears that you have not specified both IV and MV as direct effects in the structural model.
   Please confirm that:
      (1) the construct names in the measurement model are correcly spelled and specified;
      (2) the construct names in the structural model are correcly spelled and specified;
      (3) the IV and MV for the interaction are both specified as direct effects.
      Please note that plot(measurement_model) or plot(structural_model) help in visualizing the problem.
      Model cannot be estimated.")
  }

  return() # nolint: implicit_return_linter
}
