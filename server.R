# server.R


server <- function(input, output, session) {
  
  
  output$visual_options_ui <- renderUI({
    req(input$file)  # Only runs if a file has been uploaded
    
    column(8,
           div(class = "card-panel",
               h4("Visualization and labeling options"),
               
               fluidRow(
                 column(6,
                        uiOutput("group_labels_colors_table")
                 ),
                 
                 column(3,
                        checkboxGroupInput("visual_options", "Visualization:",
                                           choices = list("Show values on links" = "show_values")),
                        uiOutput("slider_tamany_valors_links"),
                        uiOutput("titol_sankey")
                 ),
                 column(3,
                        uiOutput("labels_inputs"),
                        h5(strong("Label size")),
                        sliderInput("n_titol","",value = 15, min = 0,max = 50)
                 )
               )
           )
    )
  })
  
  
  #Link values size
  output$slider_tamany_valors_links <- renderUI({
    if ("show_values" %in% input$visual_options) {
      sliderInput("font_size_values", "Number size:",
                  min = 1, max = 40, value = 20, step = 1)
    } else {
      NULL
    }
  })
  

  
  
  
  output$sankey_plot_ui <- renderUI({
    req(input$file)  # Only if there is a file
    
    tagList(
      hr(),
      br(),
      div(class = "sankey-wrapper",
          sankeyNetworkOutput("sankeyPlot", width = "100%", height = "600px")
      )
    )
  })
  
  
  
  # ReactiveValues to store the loaded and processed data
  rv <- reactiveValues(
    dades = NULL,
    links = NULL,
    nodes = NULL,
    data = NULL,
    time_label_offsets = NULL
  )
  
  
  observeEvent(input$info_dades, {
    showModal(modalDialog(
      title = "Required data structure",
      tagList(
        p("Please make sure your Excel file meets the following requirements:"),
        
        tags$ul(
          tags$li("The data must be located in the first worksheet of the Excel file."),
          tags$li("The first row must contain the column names, corresponding to the different time points (e.g. Baseline, Month 3, Month 6)."),
          tags$li("The first column must contain the patient IDs (one unique ID per row)."),
          tags$li("All remaining columns must correspond to time points."),
          tags$li("Column names should not contain special characters such as _, $, or @."),
          tags$li("Values within each column must be consistent (e.g. 'Barcelona' and 'BCN' are treated as different categories).")
        ),
        
        tags$hr(),
        
        h4("Example"),
        
        tags$table(
          class = "table table-bordered table-sm",
          tags$thead(
            tags$tr(
              tags$th("Patient ID"),
              tags$th("Baseline"),
              tags$th("Month 3"),
              tags$th("Month 6")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("P001"),
              tags$td("Low"),
              tags$td("Medium"),
              tags$td("High")
            ),
            tags$tr(
              tags$td("P002"),
              tags$td("Medium"),
              tags$td("Medium"),
              tags$td("Low")
            ),
            tags$tr(
              tags$td("P003"),
              tags$td("High"),
              tags$td("High"),
              tags$td("High")
            )
          )
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  
  # When a file is uploaded, read and process it
  observeEvent(input$file, {
    
    req(input$file)
    rv$dades <- readxl::read_excel(input$file$datapath)
    
    # Calls the data_preparation function that returns a list with links and nodes
    resultat <- data_preparation(rv$dades)
    
    rv$links <- resultat$links
    rv$nodes <- resultat$nodes
    rv$n_temps<-resultat$num_temps
    rv$col_names <-resultat$col_names
    
    # Reset the manual drag offsets for the time-point column labels
    rv$time_label_offsets <- rep(0, rv$n_temps)
    
    
    # Assign group if it's not already present, within nodes
    if (!"group" %in% colnames(rv$nodes)) {
      rv$nodes$group <- sub("^[^_]*_(.*)$", "\\1", rv$nodes$name)
    }
    
    
    
    # Prepare colors for the groups
    grups <- unique(rv$nodes$group)
    
    max_colors <- 12
    base_colors <- brewer.pal(12, "Set3")
    
    if(length(grups) > max_colors){
      base_colors <- colorRampPalette(base_colors)(length(grups))
    }
    
    
    
    
    colors_df <- data.frame(
      group = grups,
      color = base_colors,
      stringsAsFactors = FALSE
    )
    
    rv$nodes$color <- colors_df$color[match(rv$nodes$group, colors_df$group)]
    
    # Store color and label info per group (labels initially NA)
    rv$data <- data.frame(
      group = grups,
      color = colors_df$color,
      labels = grups,
      stringsAsFactors = FALSE
    )
  })
  
  
  
  # Reactive that builds nodes with assigned colors and labels
  nodes <- reactive({
    req(rv$nodes, rv$data)
    df_nodes <- rv$nodes
    
    df_nodes$colors <- rv$data$color[match(df_nodes$group, rv$data$group)]
    df_nodes$label <- rv$data$labels[match(df_nodes$group, rv$data$group)]
    
    # Try to change the node names
    suffix <- sub(".*(_[0-9]+)$", "\\1", df_nodes$name)
    df_nodes$name <- paste(df_nodes$label, suffix, sep = " ")
    df_nodes$name <- sub("_[^_]*$", "", df_nodes$name)
    
    
    df_nodes
  })
  
  
  
  
  observe({
    req(rv$data)
    
    df_uniques <- rv$data[!duplicated(rv$data$group), c("group", "color")]
    
    lapply(seq_len(nrow(df_uniques)), function(i) {
      observeEvent(input[[paste0("color_group_", i)]], {
        nou_color <- input[[paste0("color_group_", i)]]
        grup      <- df_uniques$group[i]
        
        rv$data$color[rv$data$group == grup] <- nou_color
      }, ignoreInit = TRUE)
    })
  })
  
  
  
  
  # Reactive links (colors can be added if desired)
  links <- reactive({
    req(rv$links, nodes())
    df_links <- rv$links
    
    df_nodes <- nodes()
    if (!"group" %in% colnames(df_links)) {
      df_links$group <- df_nodes$group[df_links$source + 1]
    }
    
    df_links$colors <- rv$data$color[match(df_links$group, rv$data$group)]
    df_links
  })
  
  
  
  
  observe({
    req(rv$data)
    
    df_uniques <- rv$data[!duplicated(rv$data$group), c("group", "labels")]
    
    lapply(seq_len(nrow(df_uniques)), function(i) {
      observeEvent(input[[paste0("label_group_", i)]], {
        grup <- df_uniques$group[i]
        nova_etiqueta <- input[[paste0("label_group_", i)]]
        rv$data$labels[rv$data$group == grup] <- nova_etiqueta
      }, ignoreInit = TRUE)
    })
  })
  
  
  
  
  output$group_labels_colors_table <- renderUI({
    req(rv$data)
    
    df_uniques <- rv$data[!duplicated(rv$data$group), c("group", "labels", "color")]
    
    ui_elements <- list(
      h5("Assignment of labels and colors per group:"),
      fluidRow(
        column(4, strong("Group")),
        column(4, strong("Label")),
        column(4, strong("Color"))
      )
    )
    
    rows <- lapply(seq_len(nrow(df_uniques)), function(i) {
      fluidRow(
        column(4, df_uniques$group[i]),
        column(4,
               textInput(
                 inputId = paste0("label_group_", i),
                 label = NULL,
                 value = df_uniques$labels[i],
                 placeholder = paste("Label for", df_uniques$group[i])
               )
        ),
        column(4,
               colourInput(
                 inputId = paste0("color_group_", i),
                 label = NULL,
                 value = df_uniques$color[i],
                 showColour = "both"
               )
        )
      )
    })
    
    do.call(tagList, c(ui_elements, rows))
  })
  
  
  
  
  #Titles ---------------------------------------------------------------------------------
  
  output$labels_inputs <- renderUI({
    n <- rv$n_temps
    if (is.null(n) || n < 1) return(NULL)
    
    tagList(
      lapply(seq_len(n), function(i) {
        textInput(paste0("label_temps_", i), paste("Time label", i), value = rv$col_names[i])
      })
    )
  })
  
  labels_temps <- reactive({
    n <- rv$n_temps
    if (is.null(n) || n < 1) return(NULL)
    
    sapply(seq_len(n), function(i) {
      input[[paste0("label_temps_", i)]]
    })
  })
  
  
  # Receives the updated horizontal offsets (in px) after the user drags a
  # time-point column label in the browser, and stores them so they survive
  # future re-renders of the sankey widget
  observeEvent(input$time_label_offsets, {
    req(input$time_label_offsets)
    offs <- tryCatch(jsonlite::fromJSON(input$time_label_offsets), error = function(e) NULL)
    if (!is.null(offs)) {
      rv$time_label_offsets <- as.numeric(offs)
    }
  }, ignoreInit = TRUE)
  
  
  
  
  margin_top <- reactive({
    base_margin <- 20
    extra <- ifelse(is.null(input$n_titol), 15, input$n_titol)
    base_margin + extra
  })
  
  
  # Sankey for the screen (responsive)
  sankey <- reactive({
    build_sankey(width = 850, height = 500)
  })
  
  # Reusable function to build the sankey with specific dimensions
  build_sankey <- function(width, height) {
    sankeyNetwork(
      Links = links(),
      Nodes = nodes(),
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      NodeGroup = "group",
      LinkGroup = "group",
      nodeWidth = 70,
      nodePadding = 7,
      fontSize = 14,
      fontFamily = "sans-serif",
      height = height,
      width = width,
      margin = list(left = 140, top = margin_top()),
      sinksRight = TRUE,
      colourScale = JS(
        paste0(
          'd3.scaleOrdinal().domain([',
          paste0(shQuote(rv$data$group), collapse = ","),
          ']).range([',
          paste0(shQuote(rv$data$color), collapse = ","),
          '])'
        )
      )
    )
  }
  
  
  
  
  # Applies the extra rendering (labels, values, titles) to a given sankey widget
  decorate_sankey <- function(p) {
    
    labels <- nodes()$label
    if (all(is.na(labels))) labels <- NULL
    
    if (!is.null(labels)) {
      labels_json <- jsonlite::toJSON(labels)
      p <- onRender(p, sprintf('
      function(el,x){
        var labels = %s;
        d3.select(el).selectAll(".node text")
          .text(function(d,i) { return labels[i] || d.name; })
          .attr("x", x.options.nodeWidth - 35)
          .attr("alignment-baseline", "middle")
          .attr("text-anchor", "middle");
      }
    ', labels_json))
    }
    
    if ("show_values" %in% input$visual_options) {
      font_size <- input$font_size_values
      p <- onRender(p, sprintf('
      function(el) {
        var svg = d3.select(el).select("svg");
        var g = svg.select("g");
        var sankey = this.sankey;
        var nodeWidth = sankey.nodeWidth();
        var linkData = sankey.links();

        var labels = g.selectAll(".link-label")
          .data(linkData)
          .enter()
          .append("text")
          .attr("class", "link-label")
          .attr("text-anchor", "middle")
          .attr("alignment-baseline", "middle")
          .style("font-size", "%dpx")
          .style("pointer-events", "none")
          .text(d => d.value);

        function updateLabels() {
          labels
            .attr("x", d => d.source.x + nodeWidth + (d.target.x - (d.source.x + nodeWidth)) / 2)
            .attr("y", d => {
              var startY = d.source.y + d.sy + d.dy / 2;
              var endY = d.target.y + d.ty + d.dy / 2;
              return (startY + endY) / 2;
            });
        }

        setInterval(updateLabels, 100);
      }
    ', font_size))
    }
    
    labels_t <- labels_temps()
    
    if (!is.null(labels_t) && length(labels_t) > 0) {
      labels_json <- jsonlite::toJSON(labels_t)
      
      font_size_titol <- input$n_titol %||% 15
      
      # Per-column horizontal offset (px) accumulated from manual dragging.
      # Falls back to zeros if it hasn't been set yet or the number of
      # columns changed (e.g. a new file was uploaded).
      # Read with isolate() so that dragging (which updates this value)
      # does NOT itself trigger a full re-render of the sankey widget -
      # the drag already moves the label live via D3. The stored value is
      # still picked up next time the plot rebuilds for another reason.
      offsets <- isolate(rv$time_label_offsets)
      if (is.null(offsets) || length(offsets) != length(labels_t)) {
        offsets <- rep(0, length(labels_t))
      }
      offsets_json <- jsonlite::toJSON(offsets)
      
      p <- onRender(p, sprintf('
    function(el) {
      var cols_x = this.sankey.nodes().map(d => d.x)
        .filter((v, i, a) => a.indexOf(v) === i)
        .sort(function(a, b){return a - b});

      var labels = %s;
      var fontSize = %d;
      var offsets = %s;
      var nodeWidth = this.sankey.nodeWidth();
      var baseShift = 150; // nudge to the left so it sits nicely centered

      var svg = d3.select(el).select("svg");

      var textSel = svg.selectAll(".time-label")
        .data(cols_x)
        .enter()
        .append("text")
        .attr("class", "time-label")
        .attr("x", (d, i) => d + nodeWidth / 2 + baseShift + (offsets[i] || 0))
        .attr("y", fontSize + 10)
        .attr("font-family", "sans-serif")
        .attr("font-size", fontSize)
        .attr("text-anchor", "middle")
        .style("cursor", "ew-resize")
        .text((d, i) => labels[i] || "");

      // Allow the user to fine-tune the horizontal position by dragging
      var drag = d3.drag()
        .on("drag", function(d) {
          var i = cols_x.indexOf(d);
          offsets[i] = (offsets[i] || 0) + d3.event.dx;
          d3.select(this).attr("x", d + nodeWidth / 2 + baseShift + offsets[i]);
        })
        .on("end", function() {
          if (window.Shiny) {
            Shiny.setInputValue("time_label_offsets", JSON.stringify(offsets), {priority: "event"});
          }
        });

      textSel.call(drag);
    }
  ', labels_json, font_size_titol, offsets_json))
    }
    
    p
  }
  
  
  sankey_plot <- reactive({
    decorate_sankey(sankey())
  })
  
  
  output$sankeyPlot <- renderSankeyNetwork({
    sankey_plot()
  })
  
  
  # ---------------------------------------------------------------------------
  # Interactive HTML download
  # ---------------------------------------------------------------------------
  output$downloadPlotHTML <- downloadHandler(
    filename = function() {
      paste0("sankey_plot_", Sys.Date(), ".html")
    },
    content = function(file) {
      
      # Same dimensions used for the full-page PDF
      page_width  <- 1600
      page_height <- 900
      
      # Build the sankey at full size, without UI margins
      p_full <- build_sankey(width = page_width, height = page_height)
      p_full <- decorate_sankey(p_full)
      
      # Save as self-contained HTML (everything embedded: JS, CSS, data)
      saveWidget(p_full, file, selfcontained = TRUE, background = "white")
    }
  )
  
  # ---------------------------------------------------------------------------
  # Full-page PDF download
  # ---------------------------------------------------------------------------
  output$downloadPlotPDF <- downloadHandler(
    filename = function() {
      paste0("sankey_plot_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      
      # Dimensions designed to fill the whole page in landscape format
      page_width  <- 1600
      page_height <- 900
      
      # Build a full-size sankey, without UI margins
      p_full <- build_sankey(width = page_width, height = page_height)
      p_full <- decorate_sankey(p_full)
      
      # Minimalist HTML, no margins, that fills the whole window
      temp_html <- tempfile(fileext = ".html")
      saveWidget(p_full, temp_html, selfcontained = TRUE,
                 background = "white")
      
      # Add CSS so the body has no margins and the widget fills the whole page
      html_content <- readLines(temp_html, warn = FALSE)
      css_inject <- "<style>html,body{margin:0;padding:0;overflow:hidden;}</style>"
      html_content <- sub("</head>", paste0(css_inject, "</head>"), html_content, fixed = TRUE)
      writeLines(html_content, temp_html)
      
      # Step 1: capture a PNG image at exact size (without page margins)
      temp_png <- tempfile(fileext = ".png")
      webshot2::webshot(
        temp_html,
        file = temp_png,
        vwidth = page_width,
        vheight = page_height,
        cliprect = "viewport",
        zoom = 2
      )
      
      # Step 2: convert the image to PDF making the page exactly
      # the size of the image -> the chart fills 100% of the page
      img <- magick::image_read(temp_png)
      magick::image_write(img, path = file, format = "pdf", density = "150x150")
    }
  )
  
}
