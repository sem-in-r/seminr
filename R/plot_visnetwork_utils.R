# Purpose: Helper functions for converting SEMinR models to visNetwork
#          nodes/edges data.frames
#
# Naming conventions used in this file:
#   Category    | Pattern              | Example
#   Converter   | vis_*                | vis_nodes_sm(), vis_edges_sm()
#   Mapper      | map_theme_*          | map_theme_shape(), map_theme_color()
#   Formatter   | format_vis_*         | format_vis_tooltip()
#
# See also: plot_visnetwork.R, helpers-mmMatrix.R, helpers-smMatrix.R

# -- Theme → visNetwork mapping helpers --

# Map SEMinR DOT shape names to visNetwork shape names
map_vis_shape <- function(dot_shape) {

  shape_map <- c(
    "ellipse"   = "ellipse",
    "hexagon"   = "hexagon",
    "box"       = "box",
    "rectangle" = "box",
    "diamond"   = "diamond",
    "circle"    = "circle",
    "square"    = "square",
    "triangle"  = "triangle"
  )
  unname(shape_map[dot_shape] %||% "ellipse")
}

# Get visNetwork node color list from theme for SM constructs
vis_sm_node_color <- function(theme) {
  list(
    background = theme$sm.node.fill,
    border     = theme$sm.node.color,
    highlight  = list(background = theme$sm.node.fill, border = theme$sm.node.color)
  )
}

# Get visNetwork node color list from theme for MM items
vis_mm_node_color <- function(theme) {
  list(
    background = theme$mm.node.fill,
    border     = theme$mm.node.color,
    highlight  = list(background = theme$mm.node.fill, border = theme$mm.node.color)
  )
}

# Get construct shape based on type and theme
vis_construct_shape <- function(model, construct, theme) {
  c_type <- construct_type(model, construct)
  dot_shape <- switch(c_type,
    "interaction" = "ellipse",
    "C"    = theme$construct.reflective.shape,
    "A"    = theme$construct.compositeA.shape,
    "B"    = theme$construct.compositeB.shape,
    "HOCA" = theme$construct.compositeA.shape,
    "HOCB" = theme$construct.compositeB.shape,
    "UNIT" = theme$construct.compositeB.shape,
    "ellipse"
  )
  map_vis_shape(dot_shape)
}

# Get item shape based on construct type and theme
vis_item_shape <- function(model, construct, theme) {
  c_type <- construct_type(model, construct)
  dot_shape <- switch(c_type,
    "interaction" = "box",
    "C"    = theme$manifest.reflective.shape,
    "A"    = theme$manifest.compositeA.shape,
    "B"    = theme$manifest.compositeB.shape,
    "HOCA" = theme$manifest.compositeA.shape,
    "HOCB" = theme$manifest.compositeB.shape,
    "UNIT" = theme$manifest.compositeB.shape,
    "box"
  )
  map_vis_shape(dot_shape)
}

# Get edge color/dashes based on coefficient sign
vis_edge_style_sm <- function(value, theme) {
  if (value < 0) {
    list(
      color  = theme$sm.edge.negative.color,
      dashes = (theme$sm.edge.negative.style == "dashed")
    )
  } else {
    list(
      color  = theme$sm.edge.positive.color,
      dashes = (theme$sm.edge.positive.style == "dashed")
    )
  }
}

vis_edge_style_mm <- function(value, theme) {
  if (value < 0) {
    list(
      color  = theme$mm.edge.negative.color,
      dashes = (theme$mm.edge.negative.style == "dashed")
    )
  } else {
    list(
      color  = theme$mm.edge.positive.color,
      dashes = (theme$mm.edge.positive.style == "dashed")
    )
  }
}

# -- Tooltip formatters --

# Format tooltip HTML for a construct node
format_vis_construct_tooltip <- function(construct, model, theme) {
  c_type <- construct_type(model, construct)

  # Friendly type name
  type_label <- switch(c_type,
    "C"    = "Reflective",
    "A"    = "Composite (Mode A)",
    "B"    = "Composite (Mode B)",
    "HOCA" = "Higher-Order (Mode A)",
    "HOCB" = "Higher-Order (Mode B)",
    "UNIT" = "Single Item",
    "interaction" = "Interaction",
    c_type
  )

  tooltip <- paste0("<b>", construct, "</b><br>Type: ", type_label)

  # Add R-squared if endogenous
  if (!is.null(model$rSquared) && construct %in% colnames(model$rSquared)) {
    r2 <- round(model$rSquared[1, construct], theme$plot.rounding)
    tooltip <- paste0(tooltip, "<br>R&sup2; = ", r2)
    if (theme$plot.adj && nrow(model$rSquared) >= 2) {
      adj_r2 <- round(model$rSquared[2, construct], theme$plot.rounding)
      tooltip <- paste0(tooltip, "<br>Adj. R&sup2; = ", adj_r2)
    }
  }

  # Add items list (only when mmMatrix has proper dimensions)
  has_mm <- !is.null(model$mmMatrix) && !is.null(dim(model$mmMatrix)) &&
            length(dim(model$mmMatrix)) == 2 && nrow(model$mmMatrix) > 0 &&
            !is.null(colnames(model$mmMatrix))
  if (has_mm) {
    items <- construct_items(model$mmMatrix, construct)
    if (length(items) > 0 && !all(is_interaction(items))) {
      real_items <- items[!is_interaction(items)]
      tooltip <- paste0(tooltip, "<br>Items: ", paste(real_items, collapse = ", "))
    }
  }

  tooltip
}

# Format tooltip HTML for an item node
format_vis_item_tooltip <- function(item, construct, model, theme) {
  tooltip <- paste0("<b>", item, "</b><br>Construct: ", construct)

  # Add loading
  if (!is.null(model$outer_loadings) && item %in% rownames(model$outer_loadings) &&
      construct %in% colnames(model$outer_loadings)) {
    loading <- round(model$outer_loadings[item, construct], theme$plot.rounding)
    tooltip <- paste0(tooltip, "<br>Loading: ", loading)
  }

  # Add weight
  if (!is.null(model$outer_weights) && item %in% rownames(model$outer_weights) &&
      construct %in% colnames(model$outer_weights)) {
    weight <- round(model$outer_weights[item, construct], theme$plot.rounding)
    tooltip <- paste0(tooltip, "<br>Weight: ", weight)
  }

  tooltip
}

# Format tooltip HTML for a structural path edge
format_vis_sm_edge_tooltip <- function(source, target, model, theme) {
  coef <- round(model$path_coef[source, target], theme$plot.rounding)
  tooltip <- paste0(source, " &rarr; ", target, "<br>&beta; = ", coef)

  if (inherits(model, "boot_seminr_model")) {
    smry <- summary(model)
    row_index <- paste0(source, "  ->  ", target)
    ltbl <- smry$bootstrapped_paths
    if (row_index %in% rownames(ltbl)) {
      boot_values <- extract_bootstrapped_values(ltbl, row_index, model, theme)
      tooltip <- paste0(tooltip,
                        "<br>t = ", round(boot_values[["tvalue"]], theme$plot.rounding),
                        "<br>p ", pvalr(boot_values[["p"]]),
                        "<br>95% CI [", boot_values[["lower"]], ", ", boot_values[["upper"]], "]")
    }
  }

  tooltip
}

# Format tooltip HTML for a measurement model edge
format_vis_mm_edge_tooltip <- function(item, construct, model, theme) {
  use_wt <- use_construct_weights(theme, construct_type(model, construct))

  if (use_wt) {
    value <- round(model$outer_weights[item, construct], theme$plot.rounding)
    label <- "Weight"
  } else {
    value <- round(model$outer_loadings[item, construct], theme$plot.rounding)
    label <- "Loading"
  }

  tooltip <- paste0(item, " &harr; ", construct, "<br>", label, " = ", value)

  if (inherits(model, "boot_seminr_model")) {
    smry <- summary(model)
    row_index <- paste0(item, "  ->  ", construct)
    ltbl <- if (use_wt) smry$bootstrapped_weights else smry$bootstrapped_loadings
    if (row_index %in% rownames(ltbl)) {
      boot_values <- extract_bootstrapped_values(ltbl, row_index, model, theme)
      tooltip <- paste0(tooltip,
                        "<br>t = ", round(boot_values[["tvalue"]], theme$plot.rounding),
                        "<br>p ", pvalr(boot_values[["p"]]),
                        "<br>95% CI [", boot_values[["lower"]], ", ", boot_values[["upper"]], "]")
    }
  }

  tooltip
}

# -- Main conversion: model → visNetwork data.frames --

#' Convert a SEMinR model to visNetwork nodes and edges data.frames
#'
#' @param model An estimated SEMinR model (pls_model or boot_seminr_model)
#' @param theme A seminr_theme object
#' @param measurement_only Show only measurement model
#' @param structure_only Show only structural model
#'
#' @return A list with elements \code{nodes} and \code{edges} (both data.frames)
#' @keywords internal
model_to_visdata <- function(model, theme,
                             measurement_only = FALSE,
                             structure_only = FALSE) {

  nodes_list <- list()
  edges_list <- list()

  # --- Structural Model Nodes ---
  # For structure_only with empty mmMatrix, use constructs directly
  has_mm <- !is.null(model$mmMatrix) && length(model$mmMatrix) > 1 &&
            !is.null(dim(model$mmMatrix)) && nrow(model$mmMatrix) > 0

  if (has_mm) {
    mm_coding <- extract_mm_coding(model)
    sm_constructs <- mm_coding[, "name"]

    # Also add HOC items as SM-level nodes
    for (construct in model$constructs) {
      if (!structure_only && is_HOC(model$mmMatrix, construct)) {
        hoc_items <- construct_items(model$mmMatrix, construct)
        sm_constructs <- c(sm_constructs, hoc_items)
      }
    }
  } else {
    sm_constructs <- model$constructs
    mm_coding <- NULL
  }
  sm_constructs <- unique(sm_constructs)

  # Assign hierarchical levels for layout
  exo <- only_exogenous(model$smMatrix)
  endo <- only_endogenous(model$smMatrix)
  mid <- setdiff(sm_constructs, c(exo, endo))

  for (construct in sm_constructs) {
    # Determine level for hierarchical layout
    if (construct %in% exo) {
      level <- 1
    } else if (construct %in% endo) {
      level <- 3
    } else {
      level <- 2
    }

    shape <- vis_construct_shape(model, construct, theme)
    color <- vis_sm_node_color(theme)
    tooltip <- format_vis_construct_tooltip(construct, model, theme)

    # Build label: name + R² for endogenous
    label <- construct
    if (!is.null(model$rSquared) && construct %in% colnames(model$rSquared)) {
      r2 <- round(model$rSquared[1, construct], theme$plot.rounding)
      label <- paste0(construct, "\nR\u00B2 = ", r2)
    }

    nodes_list[[length(nodes_list) + 1]] <- data.frame(
      id               = construct,
      label            = label,
      group            = "construct",
      shape            = shape,
      color.background = color$background,
      color.border     = color$border,
      font.size        = theme$sm.node.label.fontsize,
      font.color       = theme$sm.node.label.fontcolor,
      borderWidth      = 2,
      title            = tooltip,
      level            = level,
      stringsAsFactors = FALSE
    )
  }

  # --- Structural Model Edges ---
  if (!measurement_only) {
    sm <- model$smMatrix
    for (i in seq_len(nrow(sm))) {
      source <- sm[i, "source"]
      target <- sm[i, "target"]

      # Get coefficient
      if (inherits(model, "boot_seminr_model")) {
        smry <- summary(model)
        row_index <- paste0(source, "  ->  ", target)
        ltbl <- smry$bootstrapped_paths
        boot_values <- extract_bootstrapped_values(ltbl, row_index, model, theme)
        coef <- boot_values[["mean"]]
      } else {
        coef <- round(model$path_coef[source, target], theme$plot.rounding)
      }

      edge_style <- vis_edge_style_sm(coef, theme)
      width <- abs(coef * theme$sm.edge.width_multiplier) + theme$sm.edge.width_offset

      # Build label
      edge_label <- ""
      if (theme$sm.edge.label.show) {
        edge_label <- paste0("\u03B2 = ", coef)
        if (inherits(model, "boot_seminr_model") && theme$sm.edge.boot.show_p_stars) {
          stars <- psignr(boot_values[["p"]])
          edge_label <- paste0(edge_label, stars)
        }
      }

      tooltip <- format_vis_sm_edge_tooltip(source, target, model, theme)

      edges_list[[length(edges_list) + 1]] <- data.frame(
        from         = source,
        to           = target,
        label        = edge_label,
        arrows       = "to",
        dashes       = edge_style$dashes,
        width        = width,
        color.color  = edge_style$color,
        title        = tooltip,
        font.size    = theme$sm.edge.label.fontsize,
        font.color   = theme$sm.edge.label.fontcolor,
        smooth       = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  # --- Measurement Model Nodes & Edges ---
  if (!structure_only && !is.null(mm_coding) && has_mm) {
    for (idx in seq_len(nrow(mm_coding))) {
      construct <- mm_coding[idx, "name"]
      c_type <- mm_coding[idx, "type"]

      # Skip interaction terms (no manifest items to show)
      if (c_type == "interaction") next

      items <- construct_items(model$mmMatrix, construct)
      # Skip interaction items
      items <- items[!is_interaction(items)]

      for (item in items) {
        # Item node
        item_shape <- vis_item_shape(model, construct, theme)
        item_color <- vis_mm_node_color(theme)
        item_tooltip <- format_vis_item_tooltip(item, construct, model, theme)

        # Use construct name as prefix for level to group items near their construct
        item_id <- item  # items should be unique across constructs

        nodes_list[[length(nodes_list) + 1]] <- data.frame(
          id               = item_id,
          label            = item,
          group            = "item",
          shape            = item_shape,
          color.background = item_color$background,
          color.border     = item_color$border,
          font.size        = theme$mm.node.label.fontsize,
          font.color       = theme$mm.node.label.fontcolor,
          borderWidth      = 1,
          title            = item_tooltip,
          level            = NA_integer_,
          stringsAsFactors = FALSE
        )

        # MM edge
        loading <- extract_mm_edge_value(model, theme,
                                         indicator = item,
                                         construct = construct)
        mm_edge_style <- vis_edge_style_mm(loading, theme)
        mm_width <- abs(loading * theme$mm.edge.width_multiplier) + theme$mm.edge.width_offset

        # Arrow direction based on construct type
        c_type_for_arrow <- construct_type(model, construct)
        if (c_type_for_arrow == "C") {
          arrow_dir <- theme$construct.reflective.arrow
        } else if (c_type_for_arrow %in% c("A", "HOCA")) {
          arrow_dir <- theme$construct.compositeA.arrow
        } else {
          arrow_dir <- theme$construct.compositeB.arrow
        }

        arrows <- switch(arrow_dir,
          "forward"  = "to",
          "backward" = "from",
          "none"     = ""
        )

        mm_label <- ""
        if (theme$mm.edge.label.show) {
          use_wt <- use_construct_weights(theme, construct_type(model, construct))
          letter <- if (use_wt) "w" else "\u03BB"
          mm_label <- paste0(letter, " = ", loading)
        }

        mm_tooltip <- format_vis_mm_edge_tooltip(item, construct, model, theme)

        edges_list[[length(edges_list) + 1]] <- data.frame(
          from         = item_id,
          to           = construct,
          label        = mm_label,
          arrows       = arrows,
          dashes       = mm_edge_style$dashes,
          width        = mm_width,
          color.color  = mm_edge_style$color,
          title        = mm_tooltip,
          font.size    = theme$mm.edge.label.fontsize,
          font.color   = theme$mm.edge.label.fontcolor,
          smooth       = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Combine into data.frames
  nodes_df <- if (length(nodes_list) > 0) do.call(rbind, nodes_list) else
    data.frame(id = character(), label = character(), group = character(),
               shape = character(), color.background = character(),
               color.border = character(), font.size = numeric(),
               font.color = character(), borderWidth = numeric(),
               title = character(),
               level = integer(), stringsAsFactors = FALSE)

  edges_df <- if (length(edges_list) > 0) do.call(rbind, edges_list) else
    data.frame(from = character(), to = character(), label = character(),
               arrows = character(), dashes = logical(), width = numeric(),
               color.color = character(), title = character(),
               font.size = numeric(), font.color = character(),
               smooth = logical(), stringsAsFactors = FALSE)

  rownames(nodes_df) <- NULL
  rownames(edges_df) <- NULL

  list(nodes = nodes_df, edges = edges_df)
}
