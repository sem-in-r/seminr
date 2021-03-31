## Opening Data ----
threat <- read.csv("threat.csv")
colnames(threat)
threat <- threat[,c(15:52,56:59,63:89,97:102)]
summary(threat)
library(seminr)

## Vectoring (Defining) the Constructs ----

# Antecedents
scvendor_items <- c(multi_items("SUN", c(1,3)),
                    multi_items("BLC", 1:3))
self_items <- multi_items("SEFF", 1:4)
habit_items <- multi_items("HAB", 1:4)
scuser_items <- c(multi_items("TCO", 1:4),
                  multi_items("LCO", 1:4),
                  multi_items("SCO", 1:3))

# Mediators (second-order)
tid_items <- multi_items("TID", 1:4)
tim_items <- multi_items("TIM", 1:4)
tab_items <- multi_items("TAB", 1:4)

ico_items <- multi_items("ICO", 1:3)
ibe_items <- multi_items("IBE", 1:4)
iaf_items <- multi_items("IAF", 1:3)

# Outcomes
opp_items <- multi_items("OPP", 1:4)
upg_items <- multi_items("UPG", 1:4)

# Correlates
eualt_items <- multi_items("EU", 1:3)
rdalt_items <- multi_items("RAD", 1:3)
brandid_items <- multi_items("BID", 1:4)
sunk_item <- single_item("EXP3")
exp_items <- multi_items("EXP", 1:2)
fam_items <- multi_items("FAM", 1:3)
income_item <- single_item("INC")
age_item <- single_item("AGE")
gender_item <- single_item("GEN")

# Other Correlates
# effort_items <- multi_items("EFF", 1:2)
# try_item <- single_item("TRY")
# userid_items <- multi_items("UID", 1:3)
# altim_items <- multi_items("AIM", 1:3)
# rprice_item <- single_item("RPR")
# attentive_items <- multi_items("ATT", 1:3)
# ego_items <- multi_items("EGO", 1:3)
# freq_item <- single_item("FREQ")

## Defining Model in PLS ----

# Measurement model
mm <- constructs(
  reflective("SCVEND", scvendor_items),
  reflective("SCUSER", scuser_items),
  reflective("HAB", habit_items),
  reflective("SEFF", self_items),
  composite("ICO", ico_items),
  composite("IBE", ibe_items),
  composite("IAF", iaf_items),
  higher_composite("INER", dimensions = c("ICO","IBE","IAF"),
                   method = two_stage, weights = correlation_weights),
  composite("TID", tid_items, weights = regression_weights),
  composite("TIM", tim_items, weights = regression_weights),
  composite("TAB", tab_items, weights = regression_weights),
  higher_composite("TTSE", dimensions = c("TID","TIM","TAB"),
                   method = two_stage, weights = regression_weights),
  reflective("OPP", opp_items),
  reflective("UPG", upg_items),
  reflective("EUALT", eualt_items),
  reflective("RDALT", rdalt_items),
  reflective("BID", brandid_items),
  reflective("SUNK", sunk_item),
  reflective("EXP", exp_items),
  reflective("FAMALT", fam_items),
  reflective("INC", income_item),
  reflective("AGE", age_item),
  reflective("GEN", gender_item)
)

# Shortcuts to structural model
switching_costs <- c("SCVEND", "SCUSER")
dispositions <- c("SEFF", "HAB")
mediators <- c("TTSE", "INER")
outcomes <- c("OPP","UPG")
correlates_o <- c("EUALT", "RDALT")
correlates_m <- c("BID","SUNK","EXP","FAMALT","INC","AGE","GEN")

# Structural model
sm <- relationships(
  paths(from = dispositions,    to = mediators),
  paths(from = switching_costs, to = mediators),
  paths(from = mediators,       to = outcomes),
  paths(from = correlates_o,    to = outcomes),
  paths(from = correlates_m,    to = c(mediators,outcomes)))

# PLS model
pls_model <- estimate_pls(data = threat,
                          measurement_model = mm,
                          structural_model = sm)

plot(pls_model)
pls_model$path_coef

boot_pls <- bootstrap_model(seminr_model = pls_model,
                            nboot = 500)
