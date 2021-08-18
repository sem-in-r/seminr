library(seminr)

# Create the measurement model
influencer_mm <- constructs(
  composite("SIC",  multi_items("sic_", 1:7), weights = mode_B),
  composite("PL",   multi_items("pl_", 1:4)),
  composite("PQ",   multi_items("pq_", 1:4)),
  composite("PI",   multi_items("pi_", 1:5)),
  composite("WTP",  single_item("wtp")),
  composite("PIC",  multi_items("pic_", 1:5)),
  interaction_term("PQ", "PIC", method = two_stage)
)

# Creating structural model
influencer_sm <- relationships(
  paths(from = "SIC", to = c("PL", "PQ", "PI")),
  paths(from = c("PL", "PQ", "PIC", "PQ*PIC"),  to = c("PI")),
  paths(from = "PI", to = "WTP")
)

# Estimating the model
# - note, the influencer_data dataset is bundled with seminr
influencer_pls <- estimate_pls(data = influencer_data,
                               measurement_model = influencer_mm,
                               structural_model = influencer_sm)

summary(influencer_pls)
