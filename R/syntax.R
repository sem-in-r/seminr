# LIBRARIES
library(semPLS)

Construct <- function(construct.name, item.name, item.nums=NULL, item.prefix=NULL, item.mid=NULL, item.suffix=NULL) {
  return(as.vector(rbind( rep(construct.name, length(items)),
                          paste(item.prefix, item.name, item.mid, item.nums, item.suffix, sep=""))))
}


Paths <- function(from, to) {
  return(as.vector(t(as.matrix(expand.grid(from, to)))))
}

MeasurementModel <-  function(...) {
  return(matrix(c(...), ncol=2, byrow=TRUE, dimnames=list(NULL,c("source", "target"))))
}

StructuralModel <- function(...) {
  return(matrix(c(...), ncol=2, byrow=TRUE, dimnames=list(NULL,c("source", "target"))))
}

PrintPaths <- function(modelEstimated) {
  paths = modelEstimated$path_coefficients
  paths[paths == 0] <- NA
  paths <- round(paths, 2)
  paths <- paths[, grep('^[mo]', colnames(paths))]
  paths <- paths[grep('^[amx]', rownames(paths)), ]
  print(paths)
}

PlotScores <- function(modelEstimated, typePrefixes) {
  regex = paste("^[", typePrefixes, "]", sep = "")
  scores = modelEstimated$factor_scores
  relevantScores = scores[, grep(regex, colnames(scores))]
  plot(as.data.frame(relevantScores), pch=16, col=rgb(0.5, 0.5, 0.5, alpha=0.6))
}

# DATA
survey = read.csv(file="RawData_0821.csv")
