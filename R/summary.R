# summary function for seminr
summary.seminr_model <- function(object, ...) {
  stopifnot(inherits(object, "seminr_model"))
  cat("\t\n",
#      sprintf("Weighting: %s\n", object$inner_weights),
      sprintf("Total Iterations: %s", object$iterations),
      sprintf("\nPath Coefficients:\n"))
  print_paths(mobi_pls)
  cat("\nLoadings:\n")
  print(object$outer_loadings, na.print = ".")
  cat("\nOuter Weights:\n")
  print(object$outer_weights, na.print = ".")


}


##### Not yet modified for usage in seminr ######
# Adaption of the path.diagram method in the 'sem' package (J. Fox)
# for 'sempls' objects
pathDiagram <- function(object, ...){
  UseMethod("pathDiagram")
}

pathDiagram.sempls <- function(object, file, min.rank=NULL, max.rank=NULL, same.rank=NULL,
                               edge.labels=c("names", "values", "both"), size=c(8,8), node.font=c("Helvetica", 14),
                               edge.font=c("Helvetica", 10), rank.direction=c("LR", "TB"), digits=2,
                               output.type = c("graphics", "dot"), graphics.fmt = "pdf",
                               dot.options = NULL, rSquared=NULL, full=TRUE, ...){

  latent <- object$model$latent
  variables <- c(object$model$manifest,latent)
  parameters <- rownames(object$coefficients)

  output.type <- match.arg(output.type)
  if(!missing(file)){
    dot.file <- paste(file, ".dot", sep = "")
    handle <- file(dot.file, "w")
    on.exit(close(handle))
    if (output.type == "graphics"){
      graph.file <- paste(file, ".", graphics.fmt, sep = "")
    }
  }
  else handle <- stdout()
  edge.labels <- match.arg(edge.labels)
  rank.direction <- match.arg(rank.direction)
  cat(file = handle, paste("digraph \"", deparse(substitute(object)),
                           "\" {\n", sep = ""))
  cat(file = handle, paste("  rankdir=", rank.direction, ";\n",
                           sep = ""))
  cat(file = handle, paste("  size=\"", size[1], ",", size[2],
                           "\";\n", sep = ""))
  cat(file = handle, paste("  node [fontname=\"", node.font[1],
                           "\" fontsize=", node.font[2], " shape=box];\n", sep = ""))
  cat(file = handle, paste("  edge [fontname=\"", edge.font[1],
                           "\" fontsize=", edge.font[2], "];\n", sep = ""))
  cat(file = handle, "  center=1;\n")
  if (!is.null(min.rank)) {
    min.rank <- paste("\"", min.rank, "\"", sep = "")
    min.rank <- gsub(",", "\" \"", gsub(" ", "", min.rank))
    cat(file = handle, paste("  {rank=min ", min.rank, "}\n",
                             sep = ""))
  }
  if (!is.null(max.rank)) {
    max.rank <- paste("\"", max.rank, "\"", sep = "")
    max.rank <- gsub(",", "\" \"", gsub(" ", "", max.rank))
    cat(file = handle, paste("  {rank=max ", max.rank, "}\n",
                             sep = ""))
  }
  if (!is.null(same.rank)) {
    for (s in 1:length(same.rank)) {
      same <- paste("\"", same.rank[s], "\"", sep = "")
      same <- gsub(",", "\" \"", gsub(" ", "", same))
      cat(file = handle, paste("  {rank=same ", same, "}\n",
                               sep = ""))
    }
  }
  if(!is.null(rSquared)) rSquared <- round(rSquared, digits)
  for(lat in latent){
    if(is.null(rSquared) || is.na(rSquared[lat,])){
      cat(file=handle, paste('  "', lat, '" [shape=ellipse]\n', sep=""))
    }
    else{cat(file=handle, paste('  "', lat, '" [shape=ellipse, label="', lat, '\\n',
                                rSquared[lat,], '"]\n', sep=""))}
  }

  values <- round(object$coefficients$Estimate, digits)
  labels <- if (edge.labels == "names")
    parameters
  else if (edge.labels == "values")
    values
  else paste(parameters, values, sep = "=")
  path <- sub(' -> ', '" -> "', object$coefficients[,1])
  cat(file=handle,
      if(full){
        paste(' "', path, '" [label="', labels, '"];\n', sep="")
      }
      else{paste(' "', path[-(1:length(object$model$manifest))],
                 '" [label="', labels[-(1:length(object$model$manifest))], '"];\n',
                 sep="")}
  )
  cat(file = handle, "}\n")
  if (output.type == "graphics" && !missing(file)) {
    cmd <- paste("dot -T", graphics.fmt, " -o ", graph.file,
                 " ", dot.options, " ", dot.file, sep = "")
    cat("Running ", cmd, "\n")
    result <- try(system(cmd))
  }
  invisible(NULL)
}
