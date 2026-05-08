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
  
  title = "wrangled", # create title
  
  theme = bs_theme(bootswatch = "united"), # adjust theme
  

  # this is where all the HTML and fancy nonsense 
  # i created just for vibes goes
  tags$style(HTML(" 
  
    :root,
    [data-bs-theme='light'] {
      --wrangled-page-bg: #F2F0EF;
      --wrangled-inner-bg: #E6E6E6;
      --wrangled-muted: #666;
      --wrangled-inner-border: #555;
      --wrangled-sub-border: #444;
      --wrangled-card-bg: var(--bs-body-bg);
    }

    [data-bs-theme='dark'] {
      --wrangled-page-bg: #181a1b;
      --wrangled-inner-bg: #2b3035;
      --wrangled-muted: var(--bs-secondary-color);
      --wrangled-inner-border: var(--bs-border-color);
      --wrangled-sub-border: var(--bs-border-color);
      --wrangled-card-bg: #212529;
    }
  
    body {
      background-color: var(--wrangled-page-bg);
    }
    
    .card {
      background-color: var(--wrangled-card-bg);
    }
    
    .form-group,
    .shiny-input-container {
      width: 100% !important;
    }

    .input-group {
      width: 100% !important;
    }

    .inner-card {
      background-color: var(--wrangled-inner-bg);
      border: 1px solid var(--wrangled-inner-border);
      padding: 16px;
      border-radius: 8px;
      margin-bottom: 16px;
    }
    
    .sub-card {
      border: 1px solid var(--wrangled-sub-border);
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
  
  
  # first card display
  card(
    style = "padding: 16px;",
    
    
    # title / bio section
    div(
      class = "inner-card",
      
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-start; gap: 16px;",
        
        # title portion
        div(
          tags$h1(
            "'wrangled'",
            style = "font-weight: 700; margin-bottom: 18px; font-style: italic"
          ),
          
          # subtitle portion
          tags$h3(
            "a Statistical Data Management (STAT 440) final project by Jackson Fleege",
            style = "font-weight: 700; margin-bottom: 20px;"
          )
        ),
        
        # this lets the night/day button work
        input_dark_mode(
          id = "dark_mode",
          mode = "light"
        )
      ),
      
      
      # bio/description for users
      tags$p(
        "A Shiny app for uploading, previewing, and visually exploring 
        structured datasets. Users can either upload a local file or paste a 
        direct dataset link, then view the data in an interactive table. Upon 
        providing the given dataset, users then can view any missing values 
        in their data, remove rows with null values, rename columns, reorder 
        columns, and download the edited version of their dataset. This app is 
        designed to be a simple, user-friendly tool for anyone looking to 
        quickly explore and make basic edits to their structured datasets 
        without needing to write any code themselves.",
        style = "font-size: 1.2rem; margin-bottom: 18; color: var(--wrangled-muted);"
      ),
      
      
      # important note about the app
      tags$h5(
        "*NOTE — This app was made with the intention of being used to only 
        view data, not analyze it. While you can make basic edits to the data, 
        there are no built-in statistical analysis features.",
        style = "font-weight: 700; color: #E95420; margin-bottom: 12px; font-style: italic;"
      ),
      
      # link to my repo for my information
      tags$p(
        "For more information about this project and how to use it, 
        visit my GitHub repository ",
        tags$a(
          "here.",
          href = "https://github.com/jfleege/wrangled",
          target = "_blank"
        ),
        style = "font-size: .9rem; margin-bottom: 0;"
      )
    ),
    
    
    # upload / options section
    div(
      class = "inner-card",
      
      # title for data input section
      tags$h3(
        "Upload Dataset File / Link",
        style = "font-weight: 700; margin-bottom: 12px;"
      ),
      
      
      # column layout, first column for file/link input, second column 
      # for delimiter options. can adjust sizes later if needed
      layout_columns(
        col_widths = c(8, 4),
        
        div(
          class = "sub-card",
          
          tags$h4(
            "Upload Dataset File:",
            style = "font-weight:700; margin-bottom: 6px;"
          ),
          
          tags$p(
            "Upload a structured data file from your computer. Only CSV, TSV, and TXT files are supported.",
            style = "font-size: .9rem; color: var(--wrangled-muted); margin-bottom: 12px;"
          ),
          
          # this input allows users to upload a file from their computer
          fileInput(
            "dataset_file",
            label = NULL,
            accept = c(".csv", ".tsv", ".txt"),
            width = "100%"
          ),
          
          
          tags$h4(
            "Insert Dataset Link:",
            style = "font-weight:700;"
          ),
          
          tags$p(
            "Or if you prefer, paste your structured dataset link below. 
            Make sure there are no extra spaces before or after the URL.
            Only CSV, TSV, and TXT files are supported.",
            style = "font-size: .9rem; color: var(--wrangled-muted); margin-top: 0px; margin-bottom: 12px;"
          ),
          
          # allows for text input of dataset URL, which can be used
          # instead of file upload
          textInput(
            "dataset",
            label = NULL,
            value = "",
            placeholder = "https://example.com/data.csv",
            width = "100%"
          )
        ),
        
        # this section allows users to select the delimiter type for their dataset,
        # with an auto-detect option that uses the guess_delim function to try to
        # determine the correct delimiter based on the first line of the file
        div(
          class = "sub-card",
          
          tags$h4(
            "Delimiter:",
            style = "font-weight:700; margin-bottom: 6px;"
          ),
          
          tags$p(
            "If you know the delimiter type used in your dataset, select it 
            below. If not, Auto-detect will attempt to guess the delimiter 
            type for you.",
            style = "font-size: 0.9rem; color: var(--wrangled-muted); margin-bottom: 12px;"
          ),
          
          selectInput(
            "delimiter",
            label = NULL,
            choices = c(
              "Auto-detect" = "auto",
              "Comma (,)" = ",",
              "Semicolon (;)" = ";",
              "Pipe (|)" = "|",
              "Tab" = "\t",
              "Space" = " "
            ),
            selected = "auto",
            width = "100%"
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
      
      
      layout_columns(
        col_widths = c(4, 4, 4),
        
        # this section shows a summary of missing values in the current dataset 
        # and allows users to remove rows with null values
        div(
          class = "sub-card",
          
          tags$h4(
            "Missing Values",
            style = "font-weight: 700; margin-bottom: 6px;"
          ),
          
          tags$p(
            "View a quick summary of missing values in the current dataset. 
            Use the button below to remove rows and columns that contain at 
            least one missing value.",
            style = "font-size: .9rem; color: var(--wrangled-muted); margin-bottom: 12px;"
          ),
          
          verbatimTextOutput("missing_summary"),
          
          actionButton(
            "remove_nulls",
            "Remove Rows with Null Values",
            class = "btn-warning"
          )
        ),
        
        # this section allows users to rename columns in the displayed data, 
        # with some error handling for blank names and duplicate names, 
        # also updates the column choices for the reorder/remove columns section 
        # to match the new columns after changes are applied
        div(
          class = "sub-card",
          
          tags$h4(
            "Rename Column",
            style = "font-weight: 700; margin-bottom: 6px;"
          ),
          
          tags$p(
            "Choose a column from the dataset, enter a new name, and apply the 
            change to the displayed version of the data.",
            style = "font-size: .9rem; color: var(--wrangled-muted); margin-bottom: 12px;"
          ),
          
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
        
        # this section allows users to select which columns to keep and in 
        # what order, with unselected columns being removed from the 
        # displayed data, also updates the column choices for the 
        # rename column section to match the new columns after changes 
        # are applied
        div(
          class = "sub-card",
          
          tags$h4(
            "Reorder / Remove Columns",
            style = "font-weight: 700; margin-bottom: 6px;"
          ),
          
          tags$p(
            "Select the columns to keep. The order selected will become the new 
            column order, and unselected columns will be removed.",
            style = "font-size: .9rem; color: var(--wrangled-muted); margin-bottom: 12px;"
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
  
  
  # reads data from either uploaded file or pasted URL and stores it as 
  # raw_data, also uses the guess_delim function to automatically detect 
  # the delimiter if the user selects "Auto-detect" in the UI, and reads the 
  # data using the appropriate delimiter, with some error handling for missing 
  # file or URL input and for unsupported file types
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
  
  
  # when a new dataset is loaded, set edited_data equal to raw_data and
  # update the rename column choices and column keep order choices to match the
  # columns of the new dataset
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
  
  
  # shows missing value summary for the current edited dataset, including 
  # total null values, rows with at least one null, and columns with 
  # at least one null
  output$missing_summary <- renderPrint({
    req(edited_data())
    
    df <- edited_data()
    
    cat("Total null values:", sum(is.na(df)), "\n")
    cat("Rows with at least one null:", sum(!complete.cases(df)), "\n")
    cat("Columns with at least one null:", sum(colSums(is.na(df)) > 0), "\n")
  })
  
  
  # removes rows with null values and updates the edited data, as well as 
  # the rename column choices and column keep order choices to match the 
  # new columns after changes are applied
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
  
  
  # renames selected column to new name, with some error handling for 
  # blank names and duplicate names
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
  
  
  # reorders columns based on user selection and removes unselected columns
  # also updates the rename column choices and column keep order choices to 
  # match the new columns after changes are applied
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
  # also lets users display the data in a nice interactive table with 
  # filtering, pagination, and horizontal scrolling
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
  
  
  # downloads edited dataset, with users adjustments if applicable
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
