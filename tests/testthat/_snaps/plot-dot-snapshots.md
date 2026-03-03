# DOT output: basic PLS all reflective

    Code
      cat(dot_graph(model))
    Output
      digraph G {
      
      // ----------------------
      // General graph settings
      // ----------------------
      graph [
      charset = "UTF-8",
      layout = dot,
      label = "",
      fontsize = 24,
      fontcolor = black,
      fontname = helvetica,
      rankdir = LR,
      labelloc = t,
      splines = TRUE
      bgcolor = transparent
      ]
      
      // --------------------
      // The structural model
      // --------------------
      subgraph sm {
      rankdir = LR;
      node [
      shape = ellipse,
      color = black,
      fillcolor = white,
      style = filled,
      fontsize = 12,
      fontcolor = black,
      height = 1.03933333333333,
      width = 1.27083333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "Image" [label=<<B>Image </B>>, shape = ellipse]
      "Expectation" [label=<<B>Expectation </B><BR /><FONT POINT-SIZE='10'>R² = 0.745</FONT>>, shape = ellipse]
      "Quality" [label=<<B>Quality </B><BR /><FONT POINT-SIZE='10'>R² = 0.759</FONT>>, shape = ellipse]
      "Value" [label=<<B>Value </B><BR /><FONT POINT-SIZE='10'>R² = 0.455</FONT>>, shape = ellipse]
      "Satisfaction" [label=<<B>Satisfaction </B><BR /><FONT POINT-SIZE='10'>R² = 0.927</FONT>>, shape = ellipse]
      "Complaints" [label=<<B>Complaints </B><BR /><FONT POINT-SIZE='10'>R² = 0.353</FONT>>, shape = ellipse]
      "Loyalty" [label=<<B>Loyalty </B><BR /><FONT POINT-SIZE='10'>R² = 0.735</FONT>>, shape = ellipse]
      edge [
      color = black,
      fontsize = 9,
      fontcolor = black,
      fontname = helvetica,
      dir = both,
      arrowhead = normal,
      arrowtail = none
      ]
      "Image" -> {"Expectation"}[weight = 1, label = < β = 0.863<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 4.815, style = solid, color = black]
      "Image" -> {"Satisfaction"}[weight = 1, label = < β = 0.15<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.25, style = solid, color = black]
      "Image" -> {"Loyalty"}[weight = 1, label = < β = -0.091<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.955, style = dashed, color = black]
      "Expectation" -> {"Quality"}[weight = 1, label = < β = 0.871<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 4.855, style = solid, color = black]
      "Expectation" -> {"Value"}[weight = 1, label = < β = -0.054<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.77, style = dashed, color = black]
      "Expectation" -> {"Satisfaction"}[weight = 1, label = < β = 0.027<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.635, style = solid, color = black]
      "Quality" -> {"Value"}[weight = 1, label = < β = 0.721<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 4.105, style = solid, color = black]
      "Quality" -> {"Satisfaction"}[weight = 1, label = < β = 0.669<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 3.845, style = solid, color = black]
      "Value" -> {"Satisfaction"}[weight = 1, label = < β = 0.178<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.39, style = solid, color = black]
      "Satisfaction" -> {"Complaints"}[weight = 1, label = < β = 0.594<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 3.47, style = solid, color = black]
      "Satisfaction" -> {"Loyalty"}[weight = 1, label = < β = 0.962<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 5.31, style = solid, color = black]
      "Complaints" -> {"Loyalty"}[weight = 1, label = < β = -0.039<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.695, style = dashed, color = black]
      }
      // ---------------------
      // The measurement model
      // ---------------------
      
      subgraph construct_1 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "IMAG1" [label = "IMAG1", shape = box]
      "IMAG2" [label = "IMAG2", shape = box]
      "IMAG3" [label = "IMAG3", shape = box]
      "IMAG4" [label = "IMAG4", shape = box]
      "IMAG5" [label = "IMAG5", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "IMAG1" -> {"Image"}[weight = 1000, label = < λ = 0.619 >, penwidth = 2.357, style = solid, color = dimgrey]
      "IMAG2" -> {"Image"}[weight = 1000, label = < λ = 0.533 >, penwidth = 2.099, style = solid, color = dimgrey]
      "IMAG3" -> {"Image"}[weight = 1000, label = < λ = 0.447 >, penwidth = 1.841, style = solid, color = dimgrey]
      "IMAG4" -> {"Image"}[weight = 1000, label = < λ = 0.675 >, penwidth = 2.525, style = solid, color = dimgrey]
      "IMAG5" -> {"Image"}[weight = 1000, label = < λ = 0.667 >, penwidth = 2.501, style = solid, color = dimgrey]
      
      }
      subgraph construct_2 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUEX1" [label = "CUEX1", shape = box]
      "CUEX2" [label = "CUEX2", shape = box]
      "CUEX3" [label = "CUEX3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUEX1" -> {"Expectation"}[weight = 1000, label = < λ = 0.51 >, penwidth = 2.03, style = solid, color = dimgrey]
      "CUEX2" -> {"Expectation"}[weight = 1000, label = < λ = 0.464 >, penwidth = 1.892, style = solid, color = dimgrey]
      "CUEX3" -> {"Expectation"}[weight = 1000, label = < λ = 0.436 >, penwidth = 1.808, style = solid, color = dimgrey]
      
      }
      subgraph construct_3 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "PERQ1" [label = "PERQ1", shape = box]
      "PERQ2" [label = "PERQ2", shape = box]
      "PERQ3" [label = "PERQ3", shape = box]
      "PERQ4" [label = "PERQ4", shape = box]
      "PERQ5" [label = "PERQ5", shape = box]
      "PERQ6" [label = "PERQ6", shape = box]
      "PERQ7" [label = "PERQ7", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "PERQ1" -> {"Quality"}[weight = 1000, label = < λ = 0.805 >, penwidth = 2.915, style = solid, color = dimgrey]
      "PERQ2" -> {"Quality"}[weight = 1000, label = < λ = 0.547 >, penwidth = 2.141, style = solid, color = dimgrey]
      "PERQ3" -> {"Quality"}[weight = 1000, label = < λ = 0.755 >, penwidth = 2.765, style = solid, color = dimgrey]
      "PERQ4" -> {"Quality"}[weight = 1000, label = < λ = 0.677 >, penwidth = 2.531, style = solid, color = dimgrey]
      "PERQ5" -> {"Quality"}[weight = 1000, label = < λ = 0.675 >, penwidth = 2.525, style = solid, color = dimgrey]
      "PERQ6" -> {"Quality"}[weight = 1000, label = < λ = 0.676 >, penwidth = 2.528, style = solid, color = dimgrey]
      "PERQ7" -> {"Quality"}[weight = 1000, label = < λ = 0.814 >, penwidth = 2.942, style = solid, color = dimgrey]
      
      }
      subgraph construct_4 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "PERV1" [label = "PERV1", shape = box]
      "PERV2" [label = "PERV2", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "PERV1" -> {"Value"}[weight = 1000, label = < λ = 0.745 >, penwidth = 2.735, style = solid, color = dimgrey]
      "PERV2" -> {"Value"}[weight = 1000, label = < λ = 0.939 >, penwidth = 3.317, style = solid, color = dimgrey]
      
      }
      subgraph construct_5 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSA1" [label = "CUSA1", shape = box]
      "CUSA2" [label = "CUSA2", shape = box]
      "CUSA3" [label = "CUSA3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUSA1" -> {"Satisfaction"}[weight = 1000, label = < λ = 0.671 >, penwidth = 2.513, style = solid, color = dimgrey]
      "CUSA2" -> {"Satisfaction"}[weight = 1000, label = < λ = 0.704 >, penwidth = 2.612, style = solid, color = dimgrey]
      "CUSA3" -> {"Satisfaction"}[weight = 1000, label = < λ = 0.829 >, penwidth = 2.987, style = solid, color = dimgrey]
      
      }
      subgraph construct_6 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSCO" [label = "CUSCO", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUSCO" -> {"Complaints"}[weight = 1000, label = < λ = 1 >, penwidth = 3.5, style = solid, color = dimgrey]
      
      }
      subgraph construct_7 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSL1" [label = "CUSL1", shape = box]
      "CUSL2" [label = "CUSL2", shape = box]
      "CUSL3" [label = "CUSL3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = normal
      arrowtail = none
      ]
      "Loyalty" -> {"CUSL1"}[weight = 1000, label = < λ = 0.609 >, penwidth = 2.327, style = solid, color = dimgrey]
      "Loyalty" -> {"CUSL2"}[weight = 1000, label = < λ = 0.151 >, penwidth = 0.953, style = solid, color = dimgrey]
      "Loyalty" -> {"CUSL3"}[weight = 1000, label = < λ = 0.865 >, penwidth = 3.095, style = solid, color = dimgrey]
      
      }
      }

# DOT output: mixed reflective and composite

    Code
      cat(dot_graph(model))
    Output
      digraph G {
      
      // ----------------------
      // General graph settings
      // ----------------------
      graph [
      charset = "UTF-8",
      layout = dot,
      label = "",
      fontsize = 24,
      fontcolor = black,
      fontname = helvetica,
      rankdir = LR,
      labelloc = t,
      splines = TRUE
      bgcolor = transparent
      ]
      
      // --------------------
      // The structural model
      // --------------------
      subgraph sm {
      rankdir = LR;
      node [
      shape = ellipse,
      color = black,
      fillcolor = white,
      style = filled,
      fontsize = 12,
      fontcolor = black,
      height = 1.03933333333333,
      width = 1.27083333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "Image" [label=<<B>Image </B>>, shape = ellipse]
      "Expectation" [label=<<B>Expectation </B><BR /><FONT POINT-SIZE='10'>R² = 0.764</FONT>>, shape = hexagon]
      "Quality" [label=<<B>Quality </B><BR /><FONT POINT-SIZE='10'>R² = 0.776</FONT>>, shape = hexagon]
      "Value" [label=<<B>Value </B><BR /><FONT POINT-SIZE='10'>R² = 0.41</FONT>>, shape = hexagon]
      "Satisfaction" [label=<<B>Satisfaction </B><BR /><FONT POINT-SIZE='10'>R² = 0.925</FONT>>, shape = ellipse]
      "Complaints" [label=<<B>Complaints </B><BR /><FONT POINT-SIZE='10'>R² = 0.353</FONT>>, shape = ellipse]
      "Loyalty" [label=<<B>Loyalty </B><BR /><FONT POINT-SIZE='10'>R² = 0.735</FONT>>, shape = ellipse]
      edge [
      color = black,
      fontsize = 9,
      fontcolor = black,
      fontname = helvetica,
      dir = both,
      arrowhead = normal,
      arrowtail = none
      ]
      "Image" -> {"Expectation"}[weight = 1, label = < β = 0.874<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 4.87, style = solid, color = black]
      "Image" -> {"Satisfaction"}[weight = 1, label = < β = 0.134<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.17, style = solid, color = black]
      "Image" -> {"Loyalty"}[weight = 1, label = < β = -0.091<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.955, style = dashed, color = black]
      "Expectation" -> {"Quality"}[weight = 1, label = < β = 0.881<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 4.905, style = solid, color = black]
      "Expectation" -> {"Value"}[weight = 1, label = < β = -0.117<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.085, style = dashed, color = black]
      "Expectation" -> {"Satisfaction"}[weight = 1, label = < β = 0.045<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.725, style = solid, color = black]
      "Quality" -> {"Value"}[weight = 1, label = < β = 0.741<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 4.205, style = solid, color = black]
      "Quality" -> {"Satisfaction"}[weight = 1, label = < β = 0.687<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 3.935, style = solid, color = black]
      "Value" -> {"Satisfaction"}[weight = 1, label = < β = 0.159<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.295, style = solid, color = black]
      "Satisfaction" -> {"Complaints"}[weight = 1, label = < β = 0.594<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 3.47, style = solid, color = black]
      "Satisfaction" -> {"Loyalty"}[weight = 1, label = < β = 0.962<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 5.31, style = solid, color = black]
      "Complaints" -> {"Loyalty"}[weight = 1, label = < β = -0.039<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.695, style = dashed, color = black]
      }
      // ---------------------
      // The measurement model
      // ---------------------
      
      subgraph construct_1 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "IMAG1" [label = "IMAG1", shape = box]
      "IMAG2" [label = "IMAG2", shape = box]
      "IMAG3" [label = "IMAG3", shape = box]
      "IMAG4" [label = "IMAG4", shape = box]
      "IMAG5" [label = "IMAG5", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "IMAG1" -> {"Image"}[weight = 1000, label = < λ = 0.619 >, penwidth = 2.357, style = solid, color = dimgrey]
      "IMAG2" -> {"Image"}[weight = 1000, label = < λ = 0.534 >, penwidth = 2.102, style = solid, color = dimgrey]
      "IMAG3" -> {"Image"}[weight = 1000, label = < λ = 0.447 >, penwidth = 1.841, style = solid, color = dimgrey]
      "IMAG4" -> {"Image"}[weight = 1000, label = < λ = 0.676 >, penwidth = 2.528, style = solid, color = dimgrey]
      "IMAG5" -> {"Image"}[weight = 1000, label = < λ = 0.665 >, penwidth = 2.495, style = solid, color = dimgrey]
      
      }
      subgraph construct_2 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUEX1" [label = "CUEX1", shape = hexagon]
      "CUEX2" [label = "CUEX2", shape = hexagon]
      "CUEX3" [label = "CUEX3", shape = hexagon]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = normal
      arrowtail = none
      ]
      "CUEX1" -> {"Expectation"}[weight = 1000, label = < w = 0.483 >, penwidth = 1.949, style = solid, color = dimgrey]
      "CUEX2" -> {"Expectation"}[weight = 1000, label = < w = 0.483 >, penwidth = 1.949, style = solid, color = dimgrey]
      "CUEX3" -> {"Expectation"}[weight = 1000, label = < w = 0.483 >, penwidth = 1.949, style = solid, color = dimgrey]
      
      }
      subgraph construct_3 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "PERQ1" [label = "PERQ1", shape = box]
      "PERQ2" [label = "PERQ2", shape = box]
      "PERQ3" [label = "PERQ3", shape = box]
      "PERQ4" [label = "PERQ4", shape = box]
      "PERQ5" [label = "PERQ5", shape = box]
      "PERQ6" [label = "PERQ6", shape = box]
      "PERQ7" [label = "PERQ7", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "PERQ1" -> {"Quality"}[weight = 1000, label = < λ = 0.804 >, penwidth = 2.912, style = solid, color = dimgrey]
      "PERQ2" -> {"Quality"}[weight = 1000, label = < λ = 0.64 >, penwidth = 2.42, style = solid, color = dimgrey]
      "PERQ3" -> {"Quality"}[weight = 1000, label = < λ = 0.783 >, penwidth = 2.849, style = solid, color = dimgrey]
      "PERQ4" -> {"Quality"}[weight = 1000, label = < λ = 0.769 >, penwidth = 2.807, style = solid, color = dimgrey]
      "PERQ5" -> {"Quality"}[weight = 1000, label = < λ = 0.755 >, penwidth = 2.765, style = solid, color = dimgrey]
      "PERQ6" -> {"Quality"}[weight = 1000, label = < λ = 0.774 >, penwidth = 2.822, style = solid, color = dimgrey]
      "PERQ7" -> {"Quality"}[weight = 1000, label = < λ = 0.779 >, penwidth = 2.837, style = solid, color = dimgrey]
      
      }
      subgraph construct_4 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "PERV1" [label = "PERV1", shape = box]
      "PERV2" [label = "PERV2", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = normal
      arrowtail = none
      ]
      "PERV1" -> {"Value"}[weight = 1000, label = < w = 0.18 >, penwidth = 1.04, style = solid, color = dimgrey]
      "PERV2" -> {"Value"}[weight = 1000, label = < w = 0.866 >, penwidth = 3.098, style = solid, color = dimgrey]
      
      }
      subgraph construct_5 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSA1" [label = "CUSA1", shape = box]
      "CUSA2" [label = "CUSA2", shape = box]
      "CUSA3" [label = "CUSA3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUSA1" -> {"Satisfaction"}[weight = 1000, label = < λ = 0.67 >, penwidth = 2.51, style = solid, color = dimgrey]
      "CUSA2" -> {"Satisfaction"}[weight = 1000, label = < λ = 0.705 >, penwidth = 2.615, style = solid, color = dimgrey]
      "CUSA3" -> {"Satisfaction"}[weight = 1000, label = < λ = 0.829 >, penwidth = 2.987, style = solid, color = dimgrey]
      
      }
      subgraph construct_6 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSCO" [label = "CUSCO", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUSCO" -> {"Complaints"}[weight = 1000, label = < λ = 1 >, penwidth = 3.5, style = solid, color = dimgrey]
      
      }
      subgraph construct_7 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.601833333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSL1" [label = "CUSL1", shape = box]
      "CUSL2" [label = "CUSL2", shape = box]
      "CUSL3" [label = "CUSL3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = normal
      arrowtail = none
      ]
      "Loyalty" -> {"CUSL1"}[weight = 1000, label = < λ = 0.609 >, penwidth = 2.327, style = solid, color = dimgrey]
      "Loyalty" -> {"CUSL2"}[weight = 1000, label = < λ = 0.151 >, penwidth = 0.953, style = solid, color = dimgrey]
      "Loyalty" -> {"CUSL3"}[weight = 1000, label = < λ = 0.865 >, penwidth = 3.095, style = solid, color = dimgrey]
      
      }
      }

# DOT output: interaction term

    Code
      cat(dot_graph(model))
    Output
      digraph G {
      
      // ----------------------
      // General graph settings
      // ----------------------
      graph [
      charset = "UTF-8",
      layout = dot,
      label = "",
      fontsize = 24,
      fontcolor = black,
      fontname = helvetica,
      rankdir = LR,
      labelloc = t,
      splines = TRUE
      bgcolor = transparent
      ]
      
      // --------------------
      // The structural model
      // --------------------
      subgraph sm {
      rankdir = LR;
      node [
      shape = ellipse,
      color = black,
      fillcolor = white,
      style = filled,
      fontsize = 12,
      fontcolor = black,
      height = 1.03933333333333,
      width = 1.85433333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "Image" [label=<<B>Image </B>>, shape = ellipse]
      "Quality" [label=<<B>Quality </B>>, shape = ellipse]
      "Expectation" [label=<<B>Expectation </B>>, shape = ellipse]
      "Quality*Expectation" [label=<<B>Quality*Expectation </B>>, shape = ellipse]
      "Loyalty" [label=<<B>Loyalty </B><BR /><FONT POINT-SIZE='10'>R² = 0.663</FONT>>, shape = ellipse]
      edge [
      color = black,
      fontsize = 9,
      fontcolor = black,
      fontname = helvetica,
      dir = both,
      arrowhead = normal,
      arrowtail = none
      ]
      "Image" -> {"Loyalty"}[weight = 1, label = < β = 0.861<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 4.805, style = solid, color = black]
      "Quality" -> {"Loyalty"}[weight = 1, label = < β = -0.177<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.385, style = dashed, color = black]
      "Expectation" -> {"Loyalty"}[weight = 1, label = < β = 0.085<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.925, style = solid, color = black]
      "Quality*Expectation" -> {"Loyalty"}[weight = 1, label = < β = -0.234<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.67, style = dashed, color = black]
      }
      // ---------------------
      // The measurement model
      // ---------------------
      
      subgraph construct_1 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 1.1855,
      fontname = helvetica,
      fixedsize = true
      ]
      "IMAG1" [label = "IMAG1", shape = box]
      "IMAG2" [label = "IMAG2", shape = box]
      "IMAG3" [label = "IMAG3", shape = box]
      "IMAG4" [label = "IMAG4", shape = box]
      "IMAG5" [label = "IMAG5", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "IMAG1" -> {"Image"}[weight = 1000, label = < λ = 0.535 >, penwidth = 2.105, style = solid, color = dimgrey]
      "IMAG2" -> {"Image"}[weight = 1000, label = < λ = 0.461 >, penwidth = 1.883, style = solid, color = dimgrey]
      "IMAG3" -> {"Image"}[weight = 1000, label = < λ = 0.47 >, penwidth = 1.91, style = solid, color = dimgrey]
      "IMAG4" -> {"Image"}[weight = 1000, label = < λ = 0.699 >, penwidth = 2.597, style = solid, color = dimgrey]
      "IMAG5" -> {"Image"}[weight = 1000, label = < λ = 0.753 >, penwidth = 2.759, style = solid, color = dimgrey]
      
      }
      subgraph construct_2 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 1.1855,
      fontname = helvetica,
      fixedsize = true
      ]
      "PERQ1" [label = "PERQ1", shape = box]
      "PERQ2" [label = "PERQ2", shape = box]
      "PERQ3" [label = "PERQ3", shape = box]
      "PERQ4" [label = "PERQ4", shape = box]
      "PERQ5" [label = "PERQ5", shape = box]
      "PERQ6" [label = "PERQ6", shape = box]
      "PERQ7" [label = "PERQ7", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "PERQ1" -> {"Quality"}[weight = 1000, label = < λ = 0.831 >, penwidth = 2.993, style = solid, color = dimgrey]
      "PERQ2" -> {"Quality"}[weight = 1000, label = < λ = 0.602 >, penwidth = 2.306, style = solid, color = dimgrey]
      "PERQ3" -> {"Quality"}[weight = 1000, label = < λ = 0.815 >, penwidth = 2.945, style = solid, color = dimgrey]
      "PERQ4" -> {"Quality"}[weight = 1000, label = < λ = 0.639 >, penwidth = 2.417, style = solid, color = dimgrey]
      "PERQ5" -> {"Quality"}[weight = 1000, label = < λ = 0.66 >, penwidth = 2.48, style = solid, color = dimgrey]
      "PERQ6" -> {"Quality"}[weight = 1000, label = < λ = 0.597 >, penwidth = 2.291, style = solid, color = dimgrey]
      "PERQ7" -> {"Quality"}[weight = 1000, label = < λ = 0.785 >, penwidth = 2.855, style = solid, color = dimgrey]
      
      }
      subgraph construct_3 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 1.1855,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUEX1" [label = "CUEX1", shape = box]
      "CUEX2" [label = "CUEX2", shape = box]
      "CUEX3" [label = "CUEX3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUEX1" -> {"Expectation"}[weight = 1000, label = < λ = 0.493 >, penwidth = 1.979, style = solid, color = dimgrey]
      "CUEX2" -> {"Expectation"}[weight = 1000, label = < λ = 0.577 >, penwidth = 2.231, style = solid, color = dimgrey]
      "CUEX3" -> {"Expectation"}[weight = 1000, label = < λ = 0.348 >, penwidth = 1.544, style = solid, color = dimgrey]
      
      }
      
      subgraph construct_5 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 1.1855,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSL1" [label = "CUSL1", shape = box]
      "CUSL2" [label = "CUSL2", shape = box]
      "CUSL3" [label = "CUSL3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = normal
      arrowtail = none
      ]
      "Loyalty" -> {"CUSL1"}[weight = 1000, label = < λ = 0.668 >, penwidth = 2.504, style = solid, color = dimgrey]
      "Loyalty" -> {"CUSL2"}[weight = 1000, label = < λ = 0.132 >, penwidth = 0.896, style = solid, color = dimgrey]
      "Loyalty" -> {"CUSL3"}[weight = 1000, label = < λ = 0.799 >, penwidth = 2.897, style = solid, color = dimgrey]
      
      }
      }

# DOT output: higher-order composite

    Code
      cat(dot_graph(model))
    Output
      digraph G {
      
      // ----------------------
      // General graph settings
      // ----------------------
      graph [
      charset = "UTF-8",
      layout = dot,
      label = "",
      fontsize = 24,
      fontcolor = black,
      fontname = helvetica,
      rankdir = LR,
      labelloc = t,
      splines = TRUE
      bgcolor = transparent
      ]
      
      // --------------------
      // The structural model
      // --------------------
      subgraph sm {
      rankdir = LR;
      node [
      shape = ellipse,
      color = black,
      fillcolor = white,
      style = filled,
      fontsize = 12,
      fontcolor = black,
      height = 1.03933333333333,
      width = 1.27083333333333,
      fontname = helvetica,
      fixedsize = true
      ]
      "Image" [label=<<B>Image </B>>, shape = hexagon]
      "Expectation" [label=<<B>Expectation </B>>, shape = hexagon]
      "Value" [label=<<B>Value </B>>, shape = hexagon]
      "Nick" [label=<<B>Nick </B>>, shape = hexagon]
      "Satisfaction" [label=<<B>Satisfaction </B><BR /><FONT POINT-SIZE='10'>R² = 0.706</FONT>>, shape = hexagon]
      "Quality" [label=<<B>Quality </B>>, shape = hexagon]
      "Loyalty" [label=<<B>Loyalty </B>>, shape = hexagon]
      edge [
      color = black,
      fontsize = 9,
      fontcolor = black,
      fontname = helvetica,
      dir = both,
      arrowhead = normal,
      arrowtail = none
      ]
      "Image" -> {"Satisfaction"}[weight = 1, label = < β = 0.14<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.2, style = solid, color = black]
      "Expectation" -> {"Satisfaction"}[weight = 1, label = < β = 0.065<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 0.825, style = solid, color = black]
      "Value" -> {"Satisfaction"}[weight = 1, label = < β = 0.147<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 1.235, style = solid, color = black]
      "Nick" -> {"Satisfaction"}[weight = 1, label = < β = 0.592<BR /><FONT POINT-SIZE='7'>   </FONT> >, penwidth = 3.46, style = solid, color = black]
      }
      // ---------------------
      // The measurement model
      // ---------------------
      
      subgraph construct_1 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.555666666666667,
      fontname = helvetica,
      fixedsize = true
      ]
      "IMAG1" [label = "IMAG1", shape = box]
      "IMAG2" [label = "IMAG2", shape = box]
      "IMAG3" [label = "IMAG3", shape = box]
      "IMAG4" [label = "IMAG4", shape = box]
      "IMAG5" [label = "IMAG5", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "IMAG1" -> {"Image"}[weight = 1000, label = < λ = 0.763 >, penwidth = 2.789, style = solid, color = dimgrey]
      "IMAG2" -> {"Image"}[weight = 1000, label = < λ = 0.599 >, penwidth = 2.297, style = solid, color = dimgrey]
      "IMAG3" -> {"Image"}[weight = 1000, label = < λ = 0.562 >, penwidth = 2.186, style = solid, color = dimgrey]
      "IMAG4" -> {"Image"}[weight = 1000, label = < λ = 0.769 >, penwidth = 2.807, style = solid, color = dimgrey]
      "IMAG5" -> {"Image"}[weight = 1000, label = < λ = 0.736 >, penwidth = 2.708, style = solid, color = dimgrey]
      
      }
      subgraph construct_2 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.555666666666667,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUEX1" [label = "CUEX1", shape = box]
      "CUEX2" [label = "CUEX2", shape = box]
      "CUEX3" [label = "CUEX3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUEX1" -> {"Expectation"}[weight = 1000, label = < λ = 0.761 >, penwidth = 2.783, style = solid, color = dimgrey]
      "CUEX2" -> {"Expectation"}[weight = 1000, label = < λ = 0.71 >, penwidth = 2.63, style = solid, color = dimgrey]
      "CUEX3" -> {"Expectation"}[weight = 1000, label = < λ = 0.598 >, penwidth = 2.294, style = solid, color = dimgrey]
      
      }
      subgraph construct_3 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.555666666666667,
      fontname = helvetica,
      fixedsize = true
      ]
      "PERV1" [label = "PERV1", shape = box]
      "PERV2" [label = "PERV2", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "PERV1" -> {"Value"}[weight = 1000, label = < λ = 0.901 >, penwidth = 3.203, style = solid, color = dimgrey]
      "PERV2" -> {"Value"}[weight = 1000, label = < λ = 0.94 >, penwidth = 3.32, style = solid, color = dimgrey]
      
      }
      subgraph construct_4 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.555666666666667,
      fontname = helvetica,
      fixedsize = true
      ]
      "Quality" [label = "Quality", shape = box]
      "Loyalty" [label = "Loyalty", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = normal
      arrowtail = none
      ]
      "Quality" -> {"Nick"}[weight = 1000, label = < w = 0.716 >, penwidth = 2.648, style = solid, color = dimgrey]
      "Loyalty" -> {"Nick"}[weight = 1000, label = < w = 0.413 >, penwidth = 1.739, style = solid, color = dimgrey]
      
      }
      subgraph construct_5 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.555666666666667,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSA1" [label = "CUSA1", shape = box]
      "CUSA2" [label = "CUSA2", shape = box]
      "CUSA3" [label = "CUSA3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = normal
      arrowtail = none
      ]
      "Satisfaction" -> {"CUSA1"}[weight = 1000, label = < λ = 0.805 >, penwidth = 2.915, style = solid, color = dimgrey]
      "Satisfaction" -> {"CUSA2"}[weight = 1000, label = < λ = 0.847 >, penwidth = 3.041, style = solid, color = dimgrey]
      "Satisfaction" -> {"CUSA3"}[weight = 1000, label = < λ = 0.846 >, penwidth = 3.038, style = solid, color = dimgrey]
      
      }
      subgraph construct_6 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.555666666666667,
      fontname = helvetica,
      fixedsize = true
      ]
      "PERQ1" [label = "PERQ1", shape = box]
      "PERQ2" [label = "PERQ2", shape = box]
      "PERQ3" [label = "PERQ3", shape = box]
      "PERQ4" [label = "PERQ4", shape = box]
      "PERQ5" [label = "PERQ5", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "PERQ1" -> {"Quality"}[weight = 1000, label = < λ = 0.817 >, penwidth = 2.951, style = solid, color = dimgrey]
      "PERQ2" -> {"Quality"}[weight = 1000, label = < λ = 0.675 >, penwidth = 2.525, style = solid, color = dimgrey]
      "PERQ3" -> {"Quality"}[weight = 1000, label = < λ = 0.783 >, penwidth = 2.849, style = solid, color = dimgrey]
      "PERQ4" -> {"Quality"}[weight = 1000, label = < λ = 0.797 >, penwidth = 2.891, style = solid, color = dimgrey]
      "PERQ5" -> {"Quality"}[weight = 1000, label = < λ = 0.773 >, penwidth = 2.819, style = solid, color = dimgrey]
      
      }
      subgraph construct_7 {
      node [
      shape = box,
      color = dimgrey,
      fillcolor = white,
      style = filled,
      fontsize = 8,
      fontcolor = black,
      height = 0.169666666666667,
      width = 0.555666666666667,
      fontname = helvetica,
      fixedsize = true
      ]
      "CUSL1" [label = "CUSL1", shape = box]
      "CUSL2" [label = "CUSL2", shape = box]
      "CUSL3" [label = "CUSL3", shape = box]
      edge [
      color = dimgrey,
      fontsize = 7,
      fontcolor = black,
      fontname = helvetica,
      minlen = 1,
      dir = both
      arrowhead = none
      arrowtail = normal
      ]
      "CUSL1" -> {"Loyalty"}[weight = 1000, label = < λ = 0.818 >, penwidth = 2.954, style = solid, color = dimgrey]
      "CUSL2" -> {"Loyalty"}[weight = 1000, label = < λ = 0.194 >, penwidth = 1.082, style = solid, color = dimgrey]
      "CUSL3" -> {"Loyalty"}[weight = 1000, label = < λ = 0.919 >, penwidth = 3.257, style = solid, color = dimgrey]
      
      }
      }

