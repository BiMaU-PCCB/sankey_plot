# global.R (Executed before ui.R and server.R. Anything defined here is available in both.)

# Loading packages

required_packages <- c(
  "shiny",
  "bslib",
  "networkD3",
  "DT",
  "RColorBrewer",
  "htmlwidgets",
  "colourpicker",
  "magick",
  "readxl",
  "jsonlite",
  "webshot2"
)

installed_packages <- rownames(installed.packages())

missing_packages <- required_packages[!(required_packages %in% installed_packages)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}


library(shiny)
library(bslib)
library(networkD3)
library(DT)
library(RColorBrewer)
library(htmlwidgets)
library(colourpicker)
library(magick)
library(readxl)
library(jsonlite)
library(webshot2)


# webshot::install_phantomjs() # run once, only if the PDF download fails




# Visual theme (used in ui.R)

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C7A7B",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)



# Loading the data processing function (used in server.R)

source("R/data_processing.R")
