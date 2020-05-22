# DEVS ONLY: Procedures to deal with updating references
# - Only manually change references.yml
# - Use `refs_yml_to_csv()` to create csv file
# - Read csv file in code to avoid yaml dependency for users

list2df <- function(datl) {
  representative <- datl[[1]]
  nrows <- length(datl)
  ncols <- length(representative)

  datm <- matrix(unlist(datl), nrow = nrows, ncol = ncols, byrow = TRUE)
  datdf <- data.frame(datm)
  names(datdf) <- names(representative)
  datdf
}

yml_to_csv <- function(yml_in, csv_out) {
  library(yaml) # assumes available in dev environment
  refs <- read_yaml(yml_in)
  ref_df <- list2df(refs)
  write.csv(ref_df, csv_out, row.names = FALSE)
}
