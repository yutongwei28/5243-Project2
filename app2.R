library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)


library(shiny)

ui <- fluidPage(
  titlePanel("Data Analysis Tool for Advanced Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose File to upload",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx",
                  ".json",
                  ".rds"
                )),
      checkboxInput("header", "CSV File has Header", value = TRUE),
      radioButtons("sep", "Separator in CSV File",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote in Data File",
                   choices = c(None = "", "Double Quote" = "\"", "Single Quote" = "'"),
                   selected = "\""),
      actionButton("upload", "Upload Data"),
      
      # Move built-in dataset selection here
      selectInput("builtin_dataset", "Select Built-in Dataset", 
                         choices = c("None", "mtcars", "iris"), 
                         selected = "mtcars"),
      actionButton("load_builtin", "Load Built-in Dataset"),
      
      # Remove duplicates
      actionButton("remove_duplicates", "Remove Duplicates"),
      
      # Select variable for preprocessing
      selectInput("selected_var", "Select Variables for Preprocessing",
                  choices = NULL, selected = NULL, multiple = TRUE),
      # Select preprocessing actions
      selectInput("preprocess_options", "Select Preprocessing Actions",
                         choices = list("Handle Inconsistencies" = "handle_inconsistencies",
                                        "Delete Missing Values" = "delete_na",
                                        "Fill Missing Values (Mean Imputation)" = "impute_mean",
                                        "Fill Missing Values (Median Imputation)" = "impute_median",
                                        "Remove Outliers" = "outliers",
                                        "Apply Log Transformation" = "log_transform",
                                        "Standardize Numeric Features" = "standardize",
                                        "Normalize Data" = "normalize",
                                        "Encode Categorical Features" = "encode"),
                         selected = NULL),
      selectInput("fix_options", "Select Fixes",
                         choices = list("Standardize Date Format" = "fix_date_format",
                                        "Fix Data Types" = "fix_types",
                                        "Trim Text Spaces" = "fix_spaces"),
                         selected = NULL),
      sliderInput("outlier_threshold", "Outlier Removal Threshold (Z-Score)",
                  min = 1, max = 5, value = 3, step = 0.1), # Customize outlier removal
      actionButton("preprocess", "Preprocess Variables"),
      
      # Plot customization
      selectInput("x_var", "Select X-axis Variable", choices = NULL),
      selectInput("y_var", "Select Y-axis Variable", choices = NULL),
      
      actionButton("feature_engineer", "Feature Engineer"),
      actionButton("eda", "Perform EDA")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", tableOutput("table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Plot", plotlyOutput("plot")),
        tabPanel("Feature Engineering", textInput("new_feature", "New Feature Expression")),
        tabPanel("EDA Results", DT::dataTableOutput("eda_table"))
      )
    )
  )
)


# Define server logic 

library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)

server <- function(input, output, session) {
  # Initialize data with mtcars dataset
  data <- reactiveVal(mtcars)
  
  # Upload custom data
  observeEvent(input$upload, {
    req(input$file1)
    file <- input$file1$datapath
    ext <- tools::file_ext(file)
    
    if (ext %in% c("csv", "txt")) {
      df <- read_csv(file, col_names = input$header, col_types = NULL, skip = 0, n_max = -1, progress = FALSE, locale = default_locale(), na = c("", "NA"), comment = "")
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(file, sheet = 1, range = NULL, col_names = input$header, col_types = NULL, skip = 0, n_max = -1, progress = FALSE, locale = default_locale(), na = c("", "NA"), comment = "")
    } else if (ext == "json") {
      df <- fromJSON(file)
    } else if (ext == "rds") {
      df <- readRDS(file)
    }
    
    data(df)
  })
  
  # Load built-in dataset
  observeEvent(input$load_builtin, {
    dataset <- input$builtin_dataset
    if (dataset == "mtcars") {
      data(mtcars)
    } else if (dataset == "iris") {
      data(iris)
    }
  })
  
  # Update selected columns dynamically when data changes
  observe({
    req(data())
    updateSelectInput(session, "selected_var", choices = names(data()))
    updateSelectInput(session, "x_var", choices = names(data()))
    updateSelectInput(session, "y_var", choices = names(data()))
  })
  
  # Remove duplicates
  observeEvent(input$remove_duplicates, {
    req(data())
    df <- data()
    df <- df %>% distinct()
    data(df) 
  })
  
  # Preprocess data
  observeEvent(input$preprocess, {
    req(data())
    df <- data()
    
    selected_var <- input$selected_var
    
    if (!is.null(selected_var)) {
      
      if ("outliers" %in% input$cleaning_options) {
        threshold <- input$outlier_threshold
        df <- df %>% filter(across(where(is.numeric), ~ abs(scale(.)) < threshold))
      } else if ("log_transform" %in% input$preprocess_options) {
        df[[selected_var]] <- log1p(df[[selected_var]])
      } else if ("delete_na" %in% input$preprocess_options) {
        df <- df %>% drop_na(selected_var)
      } else if ("impute_mean" %in% input$preprocess_options) {
        df[[selected_var]][is.na(df[[selected_var]])] <- mean(df[[selected_var]], na.rm = TRUE)
      } else if ("impute_median" %in% input$preprocess_options) {
        df[[selected_var]][is.na(df[[selected_var]])] <- median(df[[selected_var]], na.rm = TRUE)
      } else if ("standardize" %in% input$preprocess_options) {
        df[[selected_var]] <- scale(df[[selected_var]])
      } else if ("normalize" %in% input$preprocess_options) {
        df[[selected_var]] <- (df[[selected_var]] - min(df[[selected_var]], na.rm = TRUE)) /
          (max(df[[selected_var]], na.rm = TRUE) - min(df[[selected_var]], na.rm = TRUE))
      } else if ("encode" %in% input$preprocess_options) {
        df[[selected_var]] <- as.factor(df[[selected_var]])
      } else if ("handle_inconsistencies" %in% input$preprocess_options) {
        # Fix data types (convert strings in numeric columns)
        if ("fix_types" %in% input$fix_options) {
          df[[selected_var]] <- df[[selected_var]] %>%
            mutate(across(where(~ any(suppressWarnings(!is.na(as.numeric(.))))), as.numeric))
        } # Trim Extra Spaces in Text Columns
        else if ("fix_spaces" %in% input$fix_options) {
          df[[selected_var]] <- trimws(as.character(df[[selected_var]]))
        } # Standardize Date Format
        else if ("fix_date_format" %in% input$fix_options) {
          df[[selected_var]] <- as.Date(df[[selected_var]],
            tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d-%m-%Y", "%B %d, %Y"))
        }
      }
    }
    
    data(df)
  })
  
  # Feature engineering
  observeEvent(input$feature_engineer, {
    req(data())
    df <- data()
    new_feature_expr <- input$new_feature
    df <- df %>% dplyr::mutate(new_feature = eval(parse(text = new_feature_llexpr)))
    data(df)
  })
  
  # Perform EDA
  observeEvent(input$eda, {
    req(data())
    df <- data()
    eda_results <- summary(df)
    output$eda_table <- DT::renderDataTable({
      datatable(eda_results)
    })
  })
  
  # Display data preview
  output$table <- renderTable({
    req(data())
    head(data())
  })
  
  # Display summary statistics
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Display plot
  output$plot <- renderPlotly({
    req(data(), input$x_var, input$y_var)
    df <- data()
    p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) + geom_point()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)