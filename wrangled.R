# STAT 440 Final Project — 'wrangled'
# Jackson Fleege


# --libraries that will be used to build--


library(tidyverse) # probably necessary
library(shiny) # for the app structure
library(bslib) # modern UI stuff for shiny
library(DT) # for the data table output


# -----------data sets to test------------


# CSV: https://uofi.box.com/shared/static/2nk4zg0jbanfr6vz5jmjli9hsdp61agj.csv 

#

#



# ---------------helpers------------------


# guesses the delimiter of a file by looking at the first line and 
# counting the occurrences of common delimiters. returns the delimiter
# with the highest count, which is likely the correct one for the file. 
# can be used when the user selects "Auto-detect" for the delimiter in the UI.
guess_delim <- function(file) {
  first_line <- readLines(file, n = 1, warn = FALSE)
  
  possible_delims <- c(
    "," = ",",
    ";" = ";",
    "|" = "\\|",
    "\t" = "\t" # only consider structured data types
  )
  
  counts <- sapply(possible_delims, function(d) {
    stringr::str_count(first_line, d)
  })
  
  names(which.max(counts))
}


# ------------------ui--------------------


ui <- page_sidebar(
  
  
  # code to adjust theme
  theme = bs_theme(bootswatch = "cosmo"),
  
  
  # sidebar code
  sidebar = sidebar(
    position = "left",
    width = 300,
    
    
    # little help blurb to guide users to avoid as little errors as possible
    helpText("Paste your direct dataset link below. Make sure there are no extra spaces before or after the URL."),
    
    
    # select STRUCTURED data set option that we learned in class
    # this means either the delim can either be one of:
    # ',', ';', '|', ' ', or '\t' (typically)
    textInput(
      "dataset",
      tags$h5(style = "font-weight:700;", "Insert Data Set Link Here:"),
      value = "",
      placeholder = "https://example.com/data.csv" # for aesthetic and user purposes
    ),
    
    
    # if they know the type of delimiter, let them pick
    # if not, use the built in helper function
    selectInput(
      "delimiter",
      tags$h5(style = "font-weight:700;", "Delimiter:"),
      choices = c(
        "Auto-detect" = "auto",
        "Comma (,)" = ",",
        "Semicolon (;)" = ";",
        "Pipe (|)" = "|",
        "Tab" = "\t",
        "Space" = " "
      ),
      selected = "auto"
    )
  ),
  
  
  # main display
  card(
    card_header("Data Preview"),
    DTOutput("data_table")
  )
)


# ----------------server------------------


server <- function(input, output) {
  
  
  # reads data from the provided URL, using the specified delimiter 
  # (or auto-detecting it if "Auto-detect" is selected).
  data <- reactive({
    req(input$dataset)
    
    delim_used <- if (input$delimiter == "auto") {
      guess_delim(input$dataset)
    } else {
      input$delimiter
    }
    
    read_delim(
      file = input$dataset,
      delim = delim_used,
      show_col_types = FALSE
    )
  })
  
  
  # outputs the data table using the DT package,
  output$data_table <- renderDT({
    datatable(
      data(),
      filter = "top",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100), # to adjust shown entries
        scrollX = TRUE
      )
    )
  })
}


# ----------------run app-----------------


shinyApp(ui, server) # runs everything