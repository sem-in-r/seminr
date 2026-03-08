# Purpose: Generate SEMinR R code from model specifications
#
# Takes a model specification (measurement_model + structural_model) and
# produces clean, executable SEMinR R code that reproduces the analysis.
#
# See also: specify_constructs.R, specify_relationships.R

#' Generate SEMinR R code from a model specification
#'
#' Produces clean, executable R code that reproduces a SEMinR analysis
#' including library loading, measurement model, structural model,
#' estimation, and optionally bootstrap.
#'
#' @param measurement_model A measurement model from \code{\link{constructs}}
#' @param structural_model A structural model from \code{\link{relationships}}
#' @param data_name Character string: the name of the data object to use
#'   (default: \code{"data"})
#' @param estimation Character string: estimation method, either \code{"pls"}
#'   or \code{"cbsem"} (default: \code{"pls"})
#' @param bootstrap Logical: include bootstrap code (default: \code{TRUE})
#' @param nboot Number of bootstrap samples (default: 1000)
#' @param cores Number of cores for parallel bootstrap
#'   (default: \code{parallel::detectCores()})
#'
#' @return A character string of R code
#' @export
#'
#' @examples
#' mobi_mm <- constructs(
#'   reflective("Image", multi_items("IMAG", 1:5)),
#'   composite("Value",  multi_items("PERV", 1:2))
#' )
#' mobi_sm <- relationships(
#'   paths(from = "Image", to = "Value")
#' )
#' cat(generate_seminr_code(mobi_mm, mobi_sm, data_name = "mobi"))
generate_seminr_code <- function(measurement_model,
                                 structural_model,
                                 data_name = "data",
                                 estimation = c("pls", "cbsem"),
                                 bootstrap = TRUE,
                                 nboot = 1000,
                                 cores = NULL) {

  estimation <- match.arg(estimation)

  lines <- character()

  # --- Library ---
  lines <- c(lines, "library(seminr)", "")

  # --- Measurement Model ---
  lines <- c(lines, "# Measurement Model")
  mm_code <- codegen_measurement_model(measurement_model)
  lines <- c(lines, paste0("mm <- constructs("), mm_code, ")", "")

  # --- Structural Model ---
  lines <- c(lines, "# Structural Model")
  sm_code <- codegen_structural_model(structural_model)
  lines <- c(lines, paste0("sm <- relationships("), sm_code, ")", "")

  # --- Estimation ---
  lines <- c(lines, "# Estimation")
  if (estimation == "pls") {
    lines <- c(lines, paste0(
      "model <- estimate_pls(\n",
      "  data = ", data_name, ",\n",
      "  measurement_model = mm,\n",
      "  structural_model = sm\n",
      ")"
    ))
  } else {
    lines <- c(lines, paste0(
      "model <- estimate_cbsem(\n",
      "  data = ", data_name, ",\n",
      "  measurement_model = mm,\n",
      "  structural_model = sm\n",
      ")"
    ))
  }
  lines <- c(lines, "")

  # --- Bootstrap ---
  if (bootstrap && estimation == "pls") {
    lines <- c(lines, "# Bootstrap")
    cores_str <- if (is.null(cores)) "parallel::detectCores()" else as.character(cores)
    lines <- c(lines, paste0(
      "boot <- bootstrap_model(model, nboot = ", nboot,
      ", cores = ", cores_str, ")"
    ))
    lines <- c(lines, "")
    lines <- c(lines, "summary(boot)")
  } else {
    lines <- c(lines, "summary(model)")
  }

  paste(lines, collapse = "\n")
}

# -- Internal helpers for code generation --

# Generate code for a single construct
codegen_construct <- function(construct_spec) {
  if (inherits(construct_spec, "function")) {
    # Interaction terms are stored as closures
    return(NULL)
  }

  name <- construct_name(construct_spec)
  items <- construct_items(construct_spec)

  # Detect construct type from the specification
  c_class <- class(construct_spec)

  # Determine function name and extra args
  if ("higher_order_composite" %in% c_class) {
    # Higher-order constructs use construct names as dimensions
    items_code <- codegen_items_or_dimensions(items, is_dimensions = TRUE)
    mode_code <- codegen_weights_arg(construct_spec)
    return(paste0("  higher_composite(\"", name, "\", ", items_code, mode_code, ")"))
  }

  if ("higher_order_reflective" %in% c_class) {
    items_code <- codegen_items_or_dimensions(items, is_dimensions = TRUE)
    return(paste0("  higher_reflective(\"", name, "\", ", items_code, ")"))
  }

  # Get the type code from the raw spec
  type_code <- construct_spec[3]

  if (type_code == "C") {
    # Reflective
    items_code <- codegen_items_smart(items)
    return(paste0("  reflective(\"", name, "\", ", items_code, ")"))
  }

  # Composite (A or B)
  items_code <- codegen_items_smart(items)
  if (type_code == "B") {
    return(paste0("  composite(\"", name, "\", ", items_code, ", weights = mode_B)"))
  }

  # Default: composite mode A
  return(paste0("  composite(\"", name, "\", ", items_code, ")"))
}

# Generate code for interaction terms
codegen_interaction <- function(construct_spec) {
  if (!inherits(construct_spec, "function")) return(NULL)
  if (!inherits(construct_spec, "interaction")) return(NULL)

  # Extract interaction info from the closure environment
  env <- environment(construct_spec)
  iv <- get("iv", envir = env)
  moderator <- get("moderator", envir = env)

  paste0("  interaction_term(iv = \"", iv, "\", moderator = \"", moderator, "\")")
}

# Smart item code generation: detect multi_items patterns
codegen_items_smart <- function(items) {
  if (length(items) == 1) {
    return(paste0("single_item(\"", items, "\")"))
  }

  # Try to detect multi_items pattern (common prefix + sequential numbers)
  pattern <- detect_multi_items_pattern(items)
  if (!is.null(pattern)) {
    return(pattern)
  }

  # Fall back to explicit item list
  paste0("c(", paste0("\"", items, "\"", collapse = ", "), ")")
}

# Detect if items follow a multi_items("PREFIX", start:end) pattern
detect_multi_items_pattern <- function(items) {
  if (length(items) < 2) return(NULL)

  # Find common prefix (non-numeric part at start)
  prefixes <- gsub("[0-9]+$", "", items)
  if (length(unique(prefixes)) != 1) return(NULL)

  prefix <- unique(prefixes)
  if (nchar(prefix) == 0) return(NULL)

  # Extract numeric suffixes
  suffixes <- gsub(paste0("^", gsub("([.\\\\^$|?*+(){}\\[\\]])", "\\\\\\1", prefix)), "", items)
  nums <- suppressWarnings(as.integer(suffixes))
  if (any(is.na(nums))) return(NULL)

  # Check if sequential
  if (all(diff(nums) == 1)) {
    return(paste0("multi_items(\"", prefix, "\", ", min(nums), ":", max(nums), ")"))
  }

  # Non-sequential but all numeric
  return(paste0("multi_items(\"", prefix, "\", c(", paste(nums, collapse = ", "), "))"))
}

# Generate dimension references (for HOC constructs)
codegen_items_or_dimensions <- function(items, is_dimensions = FALSE) {
  if (is_dimensions) {
    paste0("c(", paste0("\"", items, "\"", collapse = ", "), ")")
  } else {
    codegen_items_smart(items)
  }
}

# Generate weights argument for composite/HOC
codegen_weights_arg <- function(construct_spec) {
  type_code <- construct_spec[3]
  if (type_code == "B" || type_code == "HOCB") {
    return(", weights = mode_B")
  }
  ""
}

# Generate measurement model code
codegen_measurement_model <- function(measurement_model) {
  construct_lines <- character()

  for (spec in measurement_model) {
    if (inherits(spec, "interaction")) {
      line <- codegen_interaction(spec)
    } else {
      line <- codegen_construct(spec)
    }
    if (!is.null(line)) {
      construct_lines <- c(construct_lines, line)
    }
  }

  paste0(construct_lines, collapse = ",\n")
}

# Generate structural model code
codegen_structural_model <- function(structural_model) {
  sm <- structural_model
  sources <- unique(sm[, "source"])

  path_lines <- character()
  for (src in sources) {
    targets <- sm[sm[, "source"] == src, "target"]
    if (length(targets) == 1) {
      targets_code <- paste0("\"", targets, "\"")
    } else {
      targets_code <- paste0("c(", paste0("\"", targets, "\"", collapse = ", "), ")")
    }
    path_lines <- c(path_lines,
                    paste0("  paths(from = \"", src, "\", to = ", targets_code, ")"))
  }

  paste0(path_lines, collapse = ",\n")
}
