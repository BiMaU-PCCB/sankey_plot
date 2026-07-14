# ui.R

# Visual styling lives in www/styles.css, and `my_theme` comes from global.R.


ui <- fluidPage(
  theme = my_theme,
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(class = "app-title",
      h2("Interactive Sankey plot from Excel file")
  ),
  
  fluidRow(
    column(4,
           div(class = "card-panel",
               h4("Select the data"),
               fileInput("file", "Select an Excel file",
                         accept = c(".xlsx", ".xls")),
               actionButton("info_dades", "Info: Data structure",
                            class = "info-btn"),
               br(), br(),
               hr(),
               downloadButton("downloadPlotPDF", "Download Sankey as PDF",
                              class = "btn-success", icon = icon("file-pdf")),
               br(), br(),
               downloadButton("downloadPlotHTML", "Download Sankey as HTML",
                              class = "btn-success", icon = icon("file-code")),
               br(), br(),
               div(class = "alert alert-info", style = "font-size: 0.85em;",
                   tags$strong("How this works:"),
                   tags$ul(
                     tags$li("Timepoint labels (the titles above each column) can be dragged left or right to fine-tune their position."),
                     tags$li("Nodes can also be dragged to adjust their position. However, manual positions will be reset whenever you change a color, label, or other visualization option."),
                     tags$li("Timepoints are shown in the same order as the columns in your Excel file."),
                     tags$li("You can use the ", tags$b("\u25B2 / \u25BC"), " buttons to reorder the groups and change their position."),
                     tags$li("PDF exports preserve colors and labels, but not manual node and timepoint positions."),
                     tags$li("HTML exports preserve selected colors and labels and allow interactive editing of nodes and timepoints in your browser.")
                   )
               )
           )
    ),
    uiOutput("visual_options_ui")
  ),
  
  fluidRow(
    column(12,
           uiOutput("sankey_plot_ui")
    )
  )
  
)
