
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
                              class = "btn-success", icon = icon("file-code"))
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
