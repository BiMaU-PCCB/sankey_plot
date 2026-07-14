# Sankey Plot App

Interactive Shiny app that transforms a wide Excel timepoint table into an explorable Sankey diagram, with PDF/HTML export.

## Structure

```
sankey_plot/
│
├── sankey_app.Rproj          # RStudio project file
├── .gitignore                # Files ignored by Git
├── global.R                  # Package loading, theme configuration, shared functions
├── ui.R                      # User interface definition
├── server.R                  # Server logic, Sankey generation, download handlers
├── R/
│   └── data_processing.R     # Data processing functions (Excel → Sankey nodes and links)
├── www/
│   └── styles.css            # Custom CSS styles loaded automatically by Shiny
├── data/
│   └── example_1.xlsx
└── README.md

```

## Running the app

### Option 1: Run directly from GitHub

The application can be launched automatically from R without manually downloading the repository.

Install Shiny if needed:

```r
install.packages("shiny")
```

Then run:

```r
shiny::runGitHub(
  repo = "sankey_plot",
  username = "BiMaU-PCCB"
)
```

This command downloads the latest version of the repository from GitHub and opens the Shiny application automatically.

---

### Option 2: Run locally

From R, set your working directory to the `sankey_app/` folder:

```r
shiny::runApp()
```

Or, from anywhere:

```r
shiny::runApp("path/to/sankey_app")
```

In RStudio, open any of `global.R`, `ui.R`, or `server.R` and click **Run App**. RStudio will automatically detect the Shiny application structure.

## Required Excel structure

The input Excel file must follow this structure:

* Data must be stored in the first worksheet.
* First column: patient ID (one row per patient).
* Remaining columns: one column per timepoint (e.g. `Baseline`, `Month 3`, `Month 6`).
* Each cell must contain a consistent categorical value representing the state/category at that timepoint.
* Column names must not contain `_`, `$`, or `@`.
* Missing values must be represented as empty cells (not `NA` or `NaN`).

An example Excel file (`Example.xlsx`) is included in the `data/` folder and can be used to test the application. This file follows the required input structure.

This structure is also described inside the application in the **"Info: Data structure"** button.


## Dependencies

The application requires the following R packages:

* bslib
* colourpicker
* htmlwidgets
* jsonlite
* magick
* networkD3
* RColorBrewer
* readxl
* shiny
* webshot2

Install any missing packages from R with:

```r
install.packages(c(
  "bslib",
  "colourpicker",
  "htmlwidgets",
  "jsonlite",
  "magick",
  "networkD3",
  "RColorBrewer",
  "readxl",
  "shiny",
  "webshot2"
))
```
