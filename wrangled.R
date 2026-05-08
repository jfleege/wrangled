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


ui <- page_fluid(
  
  title = "wrangled",
  
  theme = bs_theme(bootswatch = "united"),
  
  tags$style(HTML("
    .form-group,
    .shiny-input-container {
      width: 100% !important;
    }

    .input-group {
      width: 100% !important;
    }

    .inner-card {
      border: 1px solid #555;
      padding: 16px;
      border-radius: 8px;
      margin-bottom: 16px;
    }
    
    .sub-card {
      border: 1px solid #444;
      padding: 16px;
      border-radius: 8px;
      height: 100%;
    }
    
    .equal-height-row > .bslib-grid-item {
      display: flex;
    }

    .equal-height-row .sub-card {
      width: 100%;
      height: 100%;
    } 
  ")),
  
  card(
    style = "padding: 16px;",
    
    # title / bio section
    div(
      class = "inner-card",
      
      tags$h1(
        "'wrangled'",
        style = "font-weight: 700; margin-bottom: 10px;"
      ),
      
      tags$p(
        "A Shiny app for uploading, previewing, and visually exploring structured datasets. Users can either upload a local file or paste a direct dataset link, then view the data in an interactive table.",
        style = "font-size: 1.2rem; margin-bottom: 0;"
      )
    ),
    
    # upload / options section
    div(
      class = "inner-card",
      
      tags$h3(
        "Upload Link/Dataset",
        style = "font-weight: 700; margin-bottom: 12px;"
      ),
      
      
      layout_columns(
        col_widths = c(8, 4),
        
        div(
          class = "sub-card",
          
          tags$p(
            "Upload a structured data file from your computer or paste a direct dataset link below. Only CSV, TSV, and TXT files are supported.",
            style = "font-size: 1.2rem; color: #666; margin-bottom: 16px;"
          ),
          
          fileInput(
            "dataset_file",
            tags$h4(style = "font-weight:700;", "Upload Data Set File:"),
            accept = c(".csv", ".tsv", ".txt"),
            width = "100%"
          ),
          
          tags$p(
            "Or if you prefer, paste your structured dataset link below. Make sure there are no extra spaces before or after the URL.",
            style = "font-size: 1.2rem; color: #666; margin-top: 18px; margin-bottom: 8px;"
          ),
          
          textInput(
            "dataset",
            tags$h4(style = "font-weight:700;", "Insert Data Set Link Here:"),
            value = "",
            placeholder = "https://example.com/data.csv",
            width = "100%"
          )
        ),
        
        div(
          class = "sub-card",
          
          selectInput(
            "delimiter",
            tags$h4(style = "font-weight:700;", "Delimiter:"),
            choices = c(
              "Auto-detect" = "auto",
              "Comma (,)" = ",",
              "Semicolon (;)" = ";",
              "Pipe (|)" = "|",
              "Tab" = "\t",
              "Space" = " "
            ),
            width = "100%"
          ),
          
          tags$p(
            "If you know the delimeter type used in your dataset, select it here above If not, 'Auto-detect' is selected by default, which will attempt to guess the delimeter type for you.",
            style = "font-size: 1.2rem; color: #666; margin-top: 18px; margin-bottom: 8px;"
          )
        )
      )
    ),
    
    # data preview section
    div(
      class = "inner-card",
      
      tags$h2(
        "Data Preview",
        style = "font-weight: 700; margin-bottom: 12px;"
      ),
      
      tags$p(
        "Preview the uploaded dataset below. You can also remove null rows, rename columns, reorder columns, and download the edited version.",
        style = "font-size: 0.95rem; color: #666; margin-bottom: 16px;"
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        
        div(
          class = "sub-card",
          
          tags$h4("Missing Values"),
          
          verbatimTextOutput("missing_summary"),
          
          actionButton(
            "remove_nulls",
            "Remove Rows with Null Values",
            class = "btn-warning"
          )
        ),
        
        div(
          class = "sub-card",
          
          tags$h4("Rename Column"),
          
          selectInput(
            "rename_old",
            "Column to Rename:",
            choices = NULL,
            width = "100%"
          ),
          
          textInput(
            "rename_new",
            "New Column Name:",
            value = "",
            width = "100%"
          ),
          
          actionButton(
            "rename_col",
            "Rename Column",
            class = "btn-primary"
          )
        ),
        
        div(
          class = "sub-card",
          
          tags$h4("Reorder / Remove Columns"),
          
          tags$p(
            "Select the columns to keep. The order selected will become the new column order.",
            style = "font-size: 0.9rem; color: #666; margin-bottom: 8px;"
          ),
          
          selectizeInput(
            "column_keep_order",
            "Columns to Keep:",
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          ),
          
          actionButton(
            "apply_column_changes",
            "Apply Column Changes",
            class = "btn-primary"
          )
        )
      ),
      
      br(),
      
      downloadButton(
        "download_edited",
        "Download Edited Dataset"
      ),
      
      br(),
      br(),
      
      DTOutput("data_table")
    )
  )
)


# ----------------server------------------


server <- function(input, output, session) {
  
  # stores the currently edited version of the data
  edited_data <- reactiveVal(NULL)
  
  
  # reads data from either uploaded file or pasted URL
  raw_data <- reactive({
    
    file_path <- NULL
    
    if (!is.null(input$dataset_file)) {
      file_path <- input$dataset_file$datapath
    } else {
      req(input$dataset)
      file_path <- stringr::str_trim(input$dataset)
    }
    
    delim_used <- if (input$delimiter == "auto") {
      guess_delim(file_path)
    } else {
      input$delimiter
    }
    
    read_delim(
      file = file_path,
      delim = delim_used,
      show_col_types = FALSE
    )
  })
  
  
  # when a new dataset is loaded, set edited_data equal to raw_data
  observeEvent(raw_data(), {
    
    df <- raw_data()
    edited_data(df)
    
    updateSelectInput(
      session,
      "rename_old",
      choices = names(df)
    )
    
    updateSelectizeInput(
      session,
      "column_keep_order",
      choices = names(df),
      selected = names(df),
      server = TRUE
    )
  })
  
  
  # shows missing value summary
  output$missing_summary <- renderPrint({
    req(edited_data())
    
    df <- edited_data()
    
    cat("Total null values:", sum(is.na(df)), "\n")
    cat("Rows with at least one null:", sum(!complete.cases(df)), "\n")
    cat("Columns with at least one null:", sum(colSums(is.na(df)) > 0), "\n")
  })
  
  
  # removes rows with null values
  observeEvent(input$remove_nulls, {
    req(edited_data())
    
    df <- edited_data()
    df <- df |> drop_na()
    
    edited_data(df)
    
    updateSelectInput(
      session,
      "rename_old",
      choices = names(df)
    )
    
    updateSelectizeInput(
      session,
      "column_keep_order",
      choices = names(df),
      selected = names(df),
      server = TRUE
    )
  })
  
  
  # renames selected column
  observeEvent(input$rename_col, {
    req(edited_data())
    req(input$rename_old)
    req(input$rename_new)
    
    df <- edited_data()
    
    old_name <- input$rename_old
    new_name <- stringr::str_trim(input$rename_new)
    
    if (new_name == "") {
      showNotification("Please enter a new column name.", type = "error")
      return()
    }
    
    if (new_name %in% names(df)) {
      showNotification("That column name already exists.", type = "error")
      return()
    }
    
    names(df)[names(df) == old_name] <- new_name
    
    edited_data(df)
    
    updateSelectInput(
      session,
      "rename_old",
      choices = names(df)
    )
    
    updateSelectizeInput(
      session,
      "column_keep_order",
      choices = names(df),
      selected = names(df),
      server = TRUE
    )
    
    updateTextInput(
      session,
      "rename_new",
      value = ""
    )
  })
  
  
  # reorders columns
  observeEvent(input$apply_column_changes, {
    req(edited_data())
    req(input$column_keep_order)
    
    df <- edited_data()
    
    selected_cols <- input$column_keep_order
    
    if (length(selected_cols) == 0) {
      showNotification(
        "Please keep at least one column.",
        type = "error"
      )
      return()
    }
    
    df <- df |> 
      select(all_of(selected_cols))
    
    edited_data(df)
    
    updateSelectInput(
      session,
      "rename_old",
      choices = names(df)
    )
    
    updateSelectizeInput(
      session,
      "column_keep_order",
      choices = names(df),
      selected = names(df),
      server = TRUE
    )
  })
  
  
  # outputs the current edited table
  output$data_table <- renderDT({
    req(edited_data())
    
    datatable(
      edited_data(),
      filter = "top",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        scrollX = TRUE
      )
    )
  })
  
  
  # downloads edited dataset
  output$download_edited <- downloadHandler(
    filename = function() {
      "wrangled_edited_dataset.csv"
    },
    content = function(file) {
      req(edited_data())
      write_csv(edited_data(), file)
    }
  )
}


# ----------------run app-----------------


shinyApp(ui, server) # runs everything