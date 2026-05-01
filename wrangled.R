# STAT 440 Final Project — 'wrangled'
# Jackson Fleege


# --libraries that will be used to build--


library(tidyverse) # probably necessary
library(shiny) # for the app structure
library(bslib) # modern UI stuff for shiny
library(DT) # for the data table output


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
  
  
  # sidebar code
  sidebar = sidebar(
    position = "left",
    
    # select STRUCTURED data set option that we learned in class
    # this means either the delim can either be one of:
    # ',', ';', '|', ' ', or '\t' (typically)
    textInput(
      "dataset",
      tags$h5(style = "font-weight:700;", "Insert Data Set Link Here:"),
      value = "",
      placeholder = "https://example.com/data.csv" # for aesthetic and user purposes
    ),
    
    
    # if they know the type of delimeter, let them pick
    # if not, use the built in helper function
    selectInput(
      "delimiter",
      "Delimiter:",
      choices = c(
        "Auto-detect" = "auto",
        "Comma (,)" = ",",
        "Semicolon (;)" = ";",
        "Pipe (|)" = "|",
        "Tab" = "\t",
        "Space" = " "
      ),
      selected = "auto"
    ),
    
    
    # main display
    card(
      card_header("Data Preview"),
      DTOutput("data_table")
    )
    
    
  )
)


# ----------------server------------------


server <- function(input, output) {
  
  
  # reads data from the provided URL, using the specified delimiter 
  # (or auto-detecting it if "Auto-detect" is selected).
  data <- reactive({
    req(input$dataset_url)
    
    delim_guess <- guess_delim(input$dataset_url)
    
    read_delim(
      file = input$dataset_url,
      delim = delim_guess,
      show_col_types = FALSE
    )
  })
  
  
  # outputs the data table using the DT package,
  output$data_table <- renderDT({
    datatable(data())
  })
}


# ----------------run app-----------------


shinyApp(ui, server) # runs everything