context("plot-basics\n")
test_that("test for issue 227", {

  mydata <- tibble::tribble(
    ~Brain_prev1, ~Brain_prev2, ~Brain_prev3, ~Brain_prev4, ~Brain_prev5, ~Brain_curr1, ~Brain_curr2, ~Brain_curr3, ~Brain_curr4, ~Brain_curr5, ~behavior1, ~behavior2, ~behavior3, ~behavior4,
    30L,          32L,          57L,          33L,          63L,          48L,          43L,          41L,          32L,          48L,       725L,       725L,       686L,       719L,
    50L,          55L,          27L,          27L,          53L,          64L,          58L,          31L,          30L,          43L,       637L,       663L,       722L,       735L,
    60L,          46L,          38L,          37L,          45L,          67L,          55L,          38L,          41L,          50L,       590L,       611L,       638L,       671L,
    59L,          60L,          41L,          26L,          43L,          58L,          52L,          53L,          41L,          39L,       629L,       595L,       672L,       747L,
    62L,          45L,          43L,          47L,          35L,          54L,          58L,          33L,          42L,          38L,       590L,       613L,       684L,       731L,
    44L,          47L,          44L,          46L,          50L,          55L,          53L,          38L,          45L,          39L,       621L,       632L,       659L,       623L,
    72L,          73L,          28L,          29L,          30L,          65L,          65L,          32L,          44L,          43L,       550L,       552L,       637L,       714L,
    53L,          48L,          42L,          47L,          42L,          60L,          57L,          39L,          34L,          42L,       649L,       661L,       734L,       741L,
    58L,          44L,          49L,          42L,          37L,          59L,          50L,          38L,          46L,          39L,       597L,       660L,       617L,       690L,
    65L,          55L,          53L,          25L,          34L,          60L,          54L,          41L,          32L,          45L,       564L,       598L,       581L,       657L,
    62L,          52L,          47L,          47L,          24L,          59L,          62L,          38L,          46L,          34L,       501L,       571L,       557L,       598L,
    67L,          61L,          38L,          30L,          36L,          80L,          69L,          29L,          29L,          35L,       466L,       502L,       533L,       636L,
    53L,          51L,          48L,          39L,          41L,          57L,          46L,          48L,          44L,          42L,       647L,       671L,       720L,       755L,
    45L,          41L,          50L,          45L,          50L,          55L,          45L,          58L,          40L,          34L,       644L,       699L,       610L,       674L,
    50L,          61L,          43L,          49L,          45L,          54L,          55L,          48L,          49L,          44L,       680L,       684L,       750L,       821L,
    60L,          57L,          40L,          39L,          35L,          68L,          57L,          53L,          25L,          32L,       572L,       592L,       612L,       654L,
    56L,          59L,          52L,          38L,          47L,          71L,          66L,          33L,          33L,          39L,       519L,       563L,       695L,       702L,
    56L,          56L,          36L,          42L,          42L,          57L,          58L,          31L,          47L,          36L,       545L,       591L,       565L,       599L,
    53L,          47L,          47L,          47L,          38L,          50L,          58L,          41L,          43L,          35L,       649L,       649L,       680L,       686L,
    66L,          48L,          41L,          38L,          35L,          63L,          55L,          51L,          56L,          37L,       614L,       547L,       610L,       614L,
    66L,          47L,          55L,          28L,          36L,          62L,          55L,          47L,          38L,          28L,       528L,       563L,       542L,       559L,
    53L,          44L,          52L,          41L,          41L,          58L,          52L,          41L,          38L,          49L,       544L,       605L,       633L,       641L,
    47L,          48L,          57L,          37L,          41L,          47L,          51L,          42L,          47L,          45L,       695L,       716L,       740L,       734L,
    48L,          44L,          33L,          35L,          62L,          59L,          56L,          39L,          33L,          48L,       669L,       689L,       750L,       754L
  )


  # Define measurement model
  measurements <- constructs(
    composite("X", multi_items("Brain_prev", 1:2), weights = mode_B),
    composite("Y", multi_items("Brain_curr", 1:2), weights = mode_B)
  )

  # structural model
  structure <- relationships(
    paths(from = c("X"), to = c("Y"))
  )

  # Estimate the model using PLS estimation scheme (Consistent PLS for reflectives)
  pls_model <- estimate_pls(data = mydata, measurements, structure)

  plot(pls_model, title = "PLS Model")

  # requires manual inspection here for arrow direction in Y Construct


})

