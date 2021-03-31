# Load global test variables
if (version$major <= 3 & version$minor <6.0) {
  test_folder <- "../fixtures/V_3_5_X/"
} else {
  test_folder <- "../fixtures/V_3_6_0/"
}

# Recognise if semPlot and rsvg available
# Check if semPlot present
if(!requireNamespace("semPlot", quietly = TRUE)){
  semPlot_present <- FALSE
}
# Check rsvg present
if(!requireNamespace("rsvg", quietly = TRUE)){
  rsvg_present <- FALSE
}
