context("visNetwork backend tests")

# Skip all tests if visNetwork is not installed
skip_if_not_installed("visNetwork")

# -- Test fixtures --
# Set up a basic PLS model for testing
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Complaints",   single_item("CUSCO")),
  reflective("Loyalty",      multi_items("CUSL", 1:3))
)

mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

# -- Tests: model_to_visdata() --

test_that("model_to_visdata returns correct structure", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm)

  expect_type(visdata, "list")
  expect_named(visdata, c("nodes", "edges"))
  expect_s3_class(visdata$nodes, "data.frame")
  expect_s3_class(visdata$edges, "data.frame")
})

test_that("nodes data.frame has required columns", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm)

  required_cols <- c("id", "label", "group", "shape",
                     "color.background", "color.border",
                     "font.size", "font.color", "title")
  for (col in required_cols) {
    expect_true(col %in% names(visdata$nodes),
                info = paste("Missing column:", col))
  }
})

test_that("edges data.frame has required columns", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm)

  required_cols <- c("from", "to", "label", "arrows", "dashes",
                     "width", "color.color", "title")
  for (col in required_cols) {
    expect_true(col %in% names(visdata$edges),
                info = paste("Missing column:", col))
  }
})

test_that("all constructs appear as nodes", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm)

  construct_nodes <- visdata$nodes[visdata$nodes$group == "construct", ]
  for (cname in mobi_pls$constructs) {
    expect_true(cname %in% construct_nodes$id,
                info = paste("Missing construct node:", cname))
  }
})

test_that("all items appear as nodes", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm)

  item_nodes <- visdata$nodes[visdata$nodes$group == "item", ]
  for (item in mobi_pls$mmVariables) {
    expect_true(item %in% item_nodes$id,
                info = paste("Missing item node:", item))
  }
})

test_that("structural paths appear as edges", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm)

  sm <- mobi_pls$smMatrix
  for (i in seq_len(nrow(sm))) {
    src <- sm[i, "source"]
    tgt <- sm[i, "target"]
    matching <- visdata$edges$from == src & visdata$edges$to == tgt
    expect_true(any(matching),
                info = paste("Missing path:", src, "->", tgt))
  }
})

test_that("structure_only omits item nodes", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm, structure_only = TRUE)

  item_nodes <- visdata$nodes[visdata$nodes$group == "item", ]
  expect_equal(nrow(item_nodes), 0)
})

test_that("measurement_only omits structural edges", {
  thm <- seminr_theme_get()
  visdata <- model_to_visdata(mobi_pls, thm, measurement_only = TRUE)

  # Should have no construct-to-construct edges
  sm <- mobi_pls$smMatrix
  construct_names <- mobi_pls$constructs
  sm_edges <- visdata$edges[visdata$edges$from %in% construct_names &
                            visdata$edges$to %in% construct_names, ]
  expect_equal(nrow(sm_edges), 0)
})

# -- Tests: vis_graph() S3 methods --

test_that("vis_graph.pls_model returns visNetwork widget", {
  result <- vis_graph(mobi_pls)
  expect_s3_class(result, "visNetwork")
  expect_s3_class(result, "htmlwidget")
})

test_that("vis_graph respects custom theme", {
  thm <- seminr_theme_create(sm.node.fill = "lightblue",
                              mm.node.fill = "lightyellow")
  result <- vis_graph(mobi_pls, theme = thm)
  expect_s3_class(result, "visNetwork")
})

test_that("vis_graph.specified_model works", {
  spec <- specify_model(mobi_mm, mobi_sm)
  result <- vis_graph(spec)
  expect_s3_class(result, "visNetwork")
})

test_that("vis_graph.measurement_model works", {
  result <- vis_graph(mobi_mm)
  expect_s3_class(result, "visNetwork")
})

test_that("vis_graph.structural_model works", {
  result <- vis_graph(mobi_sm)
  expect_s3_class(result, "visNetwork")
})

test_that("vis_graph.default errors on unsupported objects", {
  expect_error(vis_graph(42), "does not support")
})

# -- Tests: plot() with backend parameter --

test_that("plot with backend=visnetwork calls vis_graph", {
  result <- plot(mobi_pls, backend = "visnetwork")
  expect_s3_class(result, "visNetwork")
})

test_that("plot with default backend uses DiagrammeR", {
  skip_if_not_installed("DiagrammeR")
  result <- plot(mobi_pls)
  # Default is DiagrammeR, which returns htmlwidget but not visNetwork class
  expect_true(inherits(result, "htmlwidget"))
  expect_false(inherits(result, "visNetwork"))
})

# -- Tests: theme mapping helpers --

test_that("map_vis_shape maps DOT shapes correctly", {
  expect_equal(map_vis_shape("ellipse"), "ellipse")
  expect_equal(map_vis_shape("hexagon"), "hexagon")
  expect_equal(map_vis_shape("box"), "box")
  expect_equal(map_vis_shape("rectangle"), "box")
})

test_that("vis_edge_style_sm distinguishes positive/negative", {
  thm <- seminr_theme_get()

  pos_style <- vis_edge_style_sm(0.5, thm)
  neg_style <- vis_edge_style_sm(-0.3, thm)

  expect_equal(pos_style$color, thm$sm.edge.positive.color)
  expect_equal(neg_style$color, thm$sm.edge.negative.color)
  expect_false(pos_style$dashes)  # default solid for positive
  expect_true(neg_style$dashes)   # default dashed for negative
})

# -- Tests: bootstrap model support --
# Bootstrap with parallel requires installed package; skip if unavailable.

test_that("vis_graph works with bootstrapped model", {
  boot <- tryCatch(
    bootstrap_model(mobi_pls, nboot = 20, cores = 1),
    error = function(e) NULL
  )
  skip_if(is.null(boot) || !inherits(boot, "boot_seminr_model"),
          "Bootstrap failed (package may not be installed for parallel workers)")

  result <- vis_graph(boot)
  expect_s3_class(result, "visNetwork")
})

test_that("bootstrap tooltips contain CI info", {
  boot <- tryCatch(
    bootstrap_model(mobi_pls, nboot = 20, cores = 1),
    error = function(e) NULL
  )
  skip_if(is.null(boot) || !inherits(boot, "boot_seminr_model"),
          "Bootstrap failed (package may not be installed for parallel workers)")

  thm <- seminr_theme_get()
  visdata <- model_to_visdata(boot, thm)

  # SM edges should have tooltip with CI
  sm_edges <- visdata$edges[visdata$edges$from %in% boot$constructs &
                            visdata$edges$to %in% boot$constructs, ]
  # At least one tooltip should contain "CI"
  expect_true(any(grepl("CI", sm_edges$title)))
})
