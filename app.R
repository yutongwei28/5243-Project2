library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(caret)
library(corrplot)


data.cleaning.tab <- tabPanel("Data Cleaning", sidebarLayout(
  sidebarPanel(
    # Remove duplicates
    tags$p(tags$b("Handling Duplicates")),
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
    actionButton("preprocess", "Preprocess Variables")
  ),
  
  mainPanel(tabsetPanel(
    tabPanel("Cleaned Data Preview", tableOutput("cleaned_table")),
    tabPanel("Summary Statistics", verbatimTextOutput("cleaned_summary"))
  ))
))

feature.tab <- tabPanel("Feature Engineering", sidebarLayout(
  sidebarPanel(
    # feature Engineering options
    h4("Feature Engineering"),
    
    # New feature generation
    textInput(
      "new_feature_expr",
      "New Feature Expression (e.g., column1 + column2)",
      ""
    ),
    actionButton("apply_new_feature", "Create New Feature"),
    
    # transformations on existing features
    selectInput(
      "transform_column",
      "Select Column for Transformation",
      choices = NULL
    ),
    radioButtons(
      "transformation_type",
      "Select Transformation",
      choices = c(
        "Logarithm" = "log",
        "Square Root" = "sqrt",
        "Square" = "square"
      )
    ),
    actionButton("apply_transformation", "Apply Transformation"),
    
    # plotting option
    checkboxInput("show_plot", "Show Plot of Feature Impact", value = TRUE)
  ),
  
  mainPanel(tabsetPanel(
    tabPanel("Feature Engineering Results", tableOutput("feature_table")),
    tabPanel("Feature Impact Plot", plotOutput("feature_impact_plot"))
  ))
))

eda.tab <- tabPanel("Exploratory Data Analysis", sidebarLayout(
  sidebarPanel(
    h4("Exploratory Data Analysis Settings"),
    
    # Select X-axis variable (for scatter plot)
    uiOutput("x_var_select"),
    
    # Select Y-axis variable (for scatter plot)
    uiOutput("y_var_select"),
    
    # Filtering options for numeric columns
    uiOutput("numeric_filter"),
    
    # Select plot type
    selectInput(
      "plot_type",
      "Select Plot Type",
      choices = c("Scatter Plot", "Histogram", "Box Plot")
    ),
    
    # Display correlation matrix button
    actionButton("correlation_btn", "Show Correlation Matrix")
  ),
  
  mainPanel(tabsetPanel(
    tabPanel("Data Summary", verbatimTextOutput("data_summary")),
    tabPanel("Visualization", plotlyOutput("eda_plot")),
    tabPanel("Correlation Matrix", plotOutput("correlation_matrix")),
    tabPanel("Filtered Data", DT::dataTableOutput("filtered_data"))
  ))
))


ui <- fluidPage(
  navbarPage(
    title = "Advanced Data Analysis Tool",
    
    # File Upload Page
    tabPanel("Upload Data", sidebarLayout(
      sidebarPanel(
        fileInput(
          "file1",
          "Choose File to upload",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xls",
            ".xlsx",
            ".json",
            ".rds"
          )
        ),
        checkboxInput("header", "CSV File has Header", value = TRUE),
        radioButtons(
          "sep",
          "Separator in CSV File",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ","
        ),
        radioButtons(
          "quote",
          "Quote in Data File",
          choices = c(
            None = "",
            "Double Quote" = "\"",
            "Single Quote" = "'"
          ),
          selected = "\""
        ),
        actionButton("upload", "Upload Data"),
        
        selectInput(
          "builtin_dataset",
          "Select Built-in Dataset",
          choices = c("None", "mtcars", "iris"),
          selected = "mtcars"
        ),
        actionButton("load_builtin", "Load Built-in Dataset")
      ),
      mainPanel(tabsetPanel(tabPanel(
        "Data Preview", tableOutput("table")
      )))
    )),
    
    data.cleaning.tab,
    
    feature.tab,
    
    eda.tab
  )
)

server <- function(input, output, session) {
  # initialize data with mtcars dataset
  data <- reactiveVal(mtcars)
  
  # upload data
  observeEvent(input$upload, {
    req(input$file1)
    file <- input$file1$datapath
    ext <- tools::file_ext(file)
    
    if (ext %in% c("csv", "txt")) {
      df <- read_csv(
        file,
        col_names = input$header,
        col_types = NULL,
        skip = 0,
        n_max = -1,
        progress = FALSE,
        locale = default_locale(),
        na = c("", "NA"),
        comment = ""
      )
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(file, sheet = 1)
    } else if (ext == "json") {
      df <- fromJSON(file)
    } else if (ext == "rds") {
      df <- readRDS(file)
    }
    
    data(df)
  })
  
  # load built-in dataset
  observeEvent(input$load_builtin, {
    dataset <- input$builtin_dataset
    if (dataset == "mtcars") {
      data(mtcars)
    } else if (dataset == "iris") {
      data(iris)
    }
  })
  
  # data Preview
  output$table <- renderTable({
    req(data())
    head(data())
  })
  
  
  # Remove duplicates
  observeEvent(input$remove_duplicates, {
    req(data())
    df <- data()
    df <- df %>% distinct()
    data(df) 
  })
  
  # Update selected columns to preprocess
  observe({
    req(data())
    updateSelectInput(session, "selected_var", choices = names(data()))
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
  
  # display cleaned data preview
  output$cleaned_table <- renderTable({
    req(data())
    head(data())
  })
  
  # show summary statistics
  output$cleaned_summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # update available columns for transformation dynamically
  observe({
    df <- data()
    updateSelectInput(session, "transform_column", choices = colnames(df))
  })
  
  # create a new feature based on user input
  observeEvent(input$apply_new_feature, {
    req(input$new_feature_expr)
    df <- data()
    
    # Evaluate the expression entered by the user to create a new feature
    new_feature_expr <- input$new_feature_expr
    df <- df %>% mutate(new_feature = eval(parse(text = new_feature_expr)))
    
    data(df)
  })
  
  # apply mathematical transformations on selected column
  # Apply mathematical transformations on the selected column
  observeEvent(input$apply_transformation, {
    req(input$transform_column)
    df <- data()
    
    # Get the selected column and its name
    column_name <- input$transform_column
    
    # Apply the selected transformation and create a new column name
    if (input$transformation_type == "log") {
      # Apply log transformation and create a new column name (e.g., age_log)
      new_column_name <- paste0(column_name, "_log")
      df <- df %>% mutate(!!new_column_name := log(get(column_name) + 1))  # Add 1 to avoid log(0)
    } else if (input$transformation_type == "sqrt") {
      # Apply square root transformation and create a new column name (e.g., age_sqrt)
      new_column_name <- paste0(column_name, "_sqrt")
      df <- df %>% mutate(!!new_column_name := sqrt(get(column_name)))
    } else if (input$transformation_type == "square") {
      # Apply square transformation and create a new column name (e.g., age_square)
      new_column_name <- paste0(column_name, "_square")
      df <- df %>% mutate(!!new_column_name := get(column_name) ^ 2)
    }
    
    # Update the data with the transformed column
    data(df)
  })
  
  
  # display feature table 
  output$feature_table <- renderTable({
    req(data())
    head(data())
  })
  
  # generate plot of feature impact (before and after transformation)
  output$feature_impact_plot <- renderPlot({
    req(data())
    df <- data()
    
    if (input$show_plot) {
      feature_col <- input$transform_column
      
      # before transformation: original feature
      p_before <- ggplot(df, aes_string(x = feature_col)) + 
        geom_histogram(bins = 30, fill = "cornflowerblue") +
        labs(title = paste("Before", input$transformation_type, "on", feature_col),
             x = feature_col, y = "Frequency") +
        theme_minimal()
      
      # after transformation: transformed feature
      transformed_col <- paste0(feature_col, "_", input$transformation_type)  # Dynamic column name based on transformation
      
      # check if the transformed column exists
      if (transformed_col %in% colnames(df)) {
        p_after <- ggplot(df, aes_string(x = transformed_col)) + 
          geom_histogram(bins = 30, fill = "lightgreen") +
          labs(title = paste("After", input$transformation_type, "on", feature_col),
               x = transformed_col, y = "Frequency") +
          theme_minimal()
        
        gridExtra::grid.arrange(p_before, p_after, ncol = 2)
      } else {
        # if the transformed column does not exist yet, show only the "before" plot
        print(p_before)
      }
    }
  })
  
  
  
  output$x_var_select <- renderUI({
    df <- data()
    selectInput(
      "x_var",
      "Select X-Axis Variable",
      choices = colnames(df),
      selected = colnames(df)[1]
    )
  })
  
  # Dynamically generate Y-axis variable selection for analysis
  output$y_var_select <- renderUI({
    df <- data()
    selectInput(
      "y_var",
      "Select Y-Axis Variable",
      choices = colnames(df),
      selected = colnames(df)[2] # Default to second column for variety
    )
  })
  
  # Dynamically generate filtering options for numeric columns
  output$numeric_filter <- renderUI({
    df <- data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    sliderInput(
      "num_filter",
      "Filter Numeric Column Values",
      min = min(df[[numeric_cols[1]]], na.rm = TRUE),
      max = max(df[[numeric_cols[1]]], na.rm = TRUE),
      value = c(min(df[[numeric_cols[1]]], na.rm = TRUE), max(df[[numeric_cols[1]]], na.rm = TRUE))
    )
  })
  
  # Display summary statistics
  output$data_summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Filter data based on user selection
  filtered_data <- reactive({
    df <- data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    req(input$num_filter)
    df <- df %>% filter(df[[numeric_cols[1]]] >= input$num_filter[1] &
                          df[[numeric_cols[1]]] <= input$num_filter[2])
    df
  })
  
  # Render the filtered data in a table
  output$filtered_data <- DT::renderDataTable({
    req(filtered_data())
    datatable(filtered_data())
  })
  
  # Render plot based on the selected plot type
  output$eda_plot <- renderPlotly({
    req(data())
    df <- data()
    
    if (input$plot_type == "Scatter Plot") {
      req(input$x_var, input$y_var) # Ensure both X and Y variables are selected
      p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "cornflowerblue") +
        labs(x = input$x_var, y = input$y_var, title = paste("Scatter Plot of", input$x_var, "vs", input$y_var)) +
        theme_minimal()
    } else if (input$plot_type == "Histogram") {
      req(input$x_var) # Only X variable needed
      p <- ggplot(df, aes_string(x = input$x_var)) +
        geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
        labs(x = input$x_var, y = "Count", title = paste("Histogram of", input$x_var)) +
        theme_minimal()
    } else if (input$plot_type == "Box Plot") {
      req(input$x_var) # Only X variable needed
      dat <- data.frame(value = df[[input$x_var]])
      p <- ggplot(dat, aes(x = "", y = value)) +
        geom_boxplot(fill = "cornflowerblue", color = "black") +
        labs(x = input$x_var, y = "Value", title = paste("Box Plot of", input$x_var)) +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  # Render correlation matrix
  observeEvent(input$correlation_btn, {
    req(data())
    df <- data()
    
    # Compute correlation matrix for numeric columns
    cor_matrix <- cor(df[sapply(df, is.numeric)], use = "complete.obs")
    
    output$correlation_matrix <- renderPlot({
      corrplot(
        cor_matrix,
        method = "circle",
        type = "upper",
        order = "hclust",
        tl.col = "black",
        tl.srt = 45
      )
    })
  })
  
  
  
}

shinyApp(ui = ui, server = server)
