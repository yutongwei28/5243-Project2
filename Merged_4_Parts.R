# app.R
library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(rlang)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Data Analysis Tool for Advanced Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      # --- File Upload Section ---
      fileInput("file1", "Choose File to Upload",
                accept = c(".csv", ".xls", ".xlsx", ".json", ".rds")),
      checkboxInput("header", "CSV File has Header", value = TRUE),
      radioButtons("sep", "Separator in CSV File",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote in Data File",
                   choices = c(None = "", "Double Quote" = "\"", "Single Quote" = "'"),
                   selected = "\""),
      actionButton("upload", "Upload Data"),
      
      # --- Built-in Dataset Selection ---
      selectInput("builtin_dataset", "Select Built-in Dataset", 
                  choices = c("None", "mtcars", "iris"), 
                  selected = "mtcars"),
      actionButton("load_builtin", "Load Built-in Dataset"),
      
      # --- Remove Duplicates ---
      actionButton("remove_duplicates", "Remove Duplicates"),
      
      # --- Preprocessing Options ---
      selectInput("selected_var", "Select Variables for Preprocessing",
                  choices = NULL, multiple = TRUE),
      selectInput("preprocess_options", "Select Preprocessing Actions",
                  choices = list("Delete Missing Values" = "delete_na",
                                 "Fill Missing (Mean)" = "impute_mean",
                                 "Fill Missing (Median)" = "impute_median",
                                 "Remove Outliers (Z-Score)" = "outliers",
                                 "Apply Log Transform" = "log_transform",
                                 "Standardize (Z-Score)" = "standardize",
                                 "Normalize [0,1]" = "normalize",
                                 "Encode as Factor" = "encode"),
                  multiple = TRUE),
      sliderInput("outlier_threshold", "Outlier Z-Score Threshold",
                  min = 1, max = 5, value = 3, step = 0.5),
      
      # --- Data Fix Options ---
      selectInput("fix_options", "Select Data Fixes",
                  choices = list("Standardize Date Format" = "fix_date_format",
                                 "Fix Data Types" = "fix_types",
                                 "Trim Spaces in Text" = "fix_spaces"),
                  multiple = TRUE),
      actionButton("preprocess", "Apply Preprocessing"),
      
      # --- Feature Engineering ---
      #textInput("new_feature", "New Feature Expression",
                #placeholder = "e.g., new_col = mpg / wt"),
      
      # New feature creation
      textInput("new_feature_expr", "New Feature Expression (e.g., column1 + column2)", ""),
      textInput("new_feature_name", "New Feature Name", ""),
      actionButton("apply_new_feature", "Create New Feature"),
      
      # Transformations on existing features
      selectInput("transform_column", "Select Column for Transformation", choices = NULL),
      radioButtons("transformation_type", "Select Transformation",
                   choices = c("Logarithm" = "log", "Square Root" = "sqrt", "Square" = "square")
      ),
      actionButton("apply_transformation", "Apply Transformation"),
      
      # Show plot of feature impact
      checkboxInput("show_plot", "Show Plot of Feature Impact", value = TRUE),
      
      
      #helpText("Type an R expression. Example: new_col = mpg / wt"),
      #actionButton("feature_engineer", "Add New Feature"),
      
      # --- EDA & Plot Settings ---
      selectInput("x_var", "X-axis", choices = NULL),
      selectInput("y_var", "Y-axis", choices = NULL),
      actionButton("eda", "Perform EDA"),
      
      # --- Download Processed Data ---
      downloadButton("download_data", "Download Processed Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DT::dataTableOutput("table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Feature Engineering Results", tableOutput("feature_table")),
        tabPanel("Feature Impact Plot", plotOutput("feature_impact_plot")),
        tabPanel("Plot", plotlyOutput("plot")),
        tabPanel("Correlation Heatmap", plotOutput("corr_plot")),
        tabPanel("EDA Results", DT::dataTableOutput("eda_table")),
        
      )
    )
  )
)
    
        # --- Detailed EDA Tab ---
        tabPanel("Advanced EDA",
                 sidebarLayout(
                   sidebarPanel(
                     # --- Univariate Analysis ---
                     h4("Univariate Analysis"),
                     selectInput("uni_var", "Select Numeric Variable", choices = NULL),
                     radioButtons("uni_plot_type", "Plot Type",
                                  choices = c("Histogram" = "hist", "Boxplot" = "boxplot"),
                                  selected = "hist"),
                     br(),
                     
                     # --- Bivariate Analysis ---
                     h4("Bivariate Analysis"),
                     selectInput("bi_xvar", "X Variable", choices = NULL),
                     selectInput("bi_yvar", "Y Variable", choices = NULL),
                     checkboxInput("use_group", "Group by a Categorical Variable?", value = FALSE),
                     uiOutput("group_var_ui"),  # We'll dynamically show the grouping var if needed
                     br(),
                     
                     # --- Correlation Analysis ---
                     h4("Correlation Analysis"),
                     helpText("Select multiple numeric variables to view correlation heatmap."),
                     selectInput("corr_vars", "Variables for Correlation",
                                 choices = NULL, multiple = TRUE),
                     
                     actionButton("run_eda", "Run Advanced EDA")
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Univariate Results",
                                plotOutput("univariate_plot"),
                                verbatimTextOutput("univariate_summary")
                       ),
                       tabPanel("Bivariate Results",
                                plotOutput("bivariate_plot"),
                                verbatimTextOutput("bivariate_summary")
                       ),
                       tabPanel("Correlation Heatmap",
                                plotOutput("corr_plot")
                       )
                     )
                   )
                 )
        )

  
# Define server logic
server <- function(input, output, session) {

  # Reactive value to store the dataset
  data <- reactiveVal(mtcars)

# --- A) Data Upload ---
  observeEvent(input$upload, {
    req(input$file1)
    file <- input$file1$datapath
    ext  <- tools::file_ext(file)
    
    df <- switch(ext,
                 "csv" = read_csv(file, col_names = input$header),
                 "xls" = read_excel(file, col_names = input$header),
                 "xlsx" = read_excel(file, col_names = input$header),
                 "json" = fromJSON(file),
                 "rds" = readRDS(file),
                 {
                   showNotification("Unsupported file format.", type = "error")
                   NULL
                 }
    )
    req(df)
    data(df)
  })
  
  observeEvent(input$load_builtin, {
    dataset <- input$builtin_dataset
    if (dataset == "mtcars") {
      data(mtcars)
    } else if (dataset == "iris") {
      data(iris)
    }
  })
  
  # Remove Duplicates
  observeEvent(input$remove_duplicates, {
    req(data())
    df <- data() %>% distinct()
    data(df)
  })
  
  # B) Preprocessing Steps
  # Update selectInput choices when data changes
  observe({
    req(data())
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    # For the "selected_var" input
    updateSelectInput(session, "selected_var", choices = names(df))
    
    # For basic EDA x_var, y_var
    updateSelectInput(session, "x_var", choices = names(df))
    updateSelectInput(session, "y_var", choices = names(df))
    
    # For advanced EDA
    updateSelectInput(session, "uni_var", choices = numeric_vars)
    updateSelectInput(session, "bi_xvar", choices = names(df))
    updateSelectInput(session, "bi_yvar", choices = numeric_vars)
    updateSelectInput(session, "corr_vars", choices = numeric_vars)
  })
  
  # Preprocessing
  observeEvent(input$preprocess, {
    req(data())
    df <- data()
    vars <- input$selected_var
    
    if (length(vars) == 0) {
      showNotification("No variables selected for preprocessing.", type = "warning")
      return()
    }
    
    # Apply each selected option
    for (option in input$preprocess_options) {
      if (option == "delete_na") {
        df <- df %>% drop_na(all_of(vars))
      } else if (option == "impute_mean") {
        for (col in vars) {
          if (is.numeric(df[[col]])) {
            df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
          }
        }
      } else if (option == "impute_median") {
        for (col in vars) {
          if (is.numeric(df[[col]])) {
            df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
          }
        }
      } else if (option == "outliers") {
        threshold <- input$outlier_threshold
        for (col in vars) {
          if (is.numeric(df[[col]])) {
            zscores <- scale(df[[col]])
            df <- df[abs(zscores) < threshold | is.na(zscores), ]
          }
        }
      } else if (option == "log_transform") {
        for (col in vars) {
          if (is.numeric(df[[col]])) {
            df[[col]] <- log1p(df[[col]])
          }
        }
      } else if (option == "standardize") {
        for (col in vars) {
          if (is.numeric(df[[col]])) {
            df[[col]] <- scale(df[[col]])
          }
        }
      } else if (option == "normalize") {
        for (col in vars) {
          if (is.numeric(df[[col]])) {
            rng <- range(df[[col]], na.rm = TRUE)
            df[[col]] <- (df[[col]] - rng[1]) / (rng[2] - rng[1])
          }
        }
      } else if (option == "encode") {
        for (col in vars) {
          df[[col]] <- as.factor(df[[col]])
        }
      }
    }
    
    # Data Fixes
    if (length(input$fix_options) > 0) {
      for (fix_opt in input$fix_options) {
        if (fix_opt == "fix_spaces") {
          df <- df %>% mutate(across(where(is.character), trimws))
        } else if (fix_opt == "fix_date_format") {
          df <- df %>% mutate(across(where(is.character), ~ as.Date(.,
                                                                    tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d-%m-%Y", "%B %d, %Y")),
                                     .names = "fixed_{col}"))
        } else if (fix_opt == "fix_types") {
          df <- df %>% mutate(across(where(~ all(!is.na(suppressWarnings(as.numeric(.))))),
                                     ~ as.numeric(.)))
        }
      }
    }
    
    data(df)
    
    updateSelectInput(session, "transform_column", choices = colnames(df))
  })
  
  # C) Feature Engineering
  observeEvent(input$apply_new_feature, {
    req(data(), input$new_feature_expr, input$new_feature_name)
    
    df <- data()
    new_feature_expr <- input$new_feature_expr
    new_feature_name <- gsub("\\s+", "_", input$new_feature_name)  # Replace spaces with underscores
    
    if (new_feature_name %in% colnames(df)) {
      showNotification("Feature name already exists! Choose a different name.", type = "error")
      return()
    }
    
    tryCatch({
      df[[new_feature_name]] <- eval(parse(text = new_feature_expr), df)
      data(df)
    }, error = function(e) {
      showNotification("Invalid expression! Ensure correct column names.", type = "error")
    })
  })
  
  # Apply transformations
  observeEvent(input$apply_transformation, {
    req(data(), input$transform_column, input$transformation_type)
    
    df <- data()
    col_name <- input$transform_column
    new_col_name <- paste0(col_name, "_", input$transformation_type)
    
    if (new_col_name %in% colnames(df)) {
      showNotification("Transformation already exists!", type = "error")
      return()
    }
    
    tryCatch({
      if (input$transformation_type == "log") {
        df[[new_col_name]] <- log(df[[col_name]] + 1)
      } else if (input$transformation_type == "sqrt") {
        df[[new_col_name]] <- sqrt(df[[col_name]])
      } else if (input$transformation_type == "square") {
        df[[new_col_name]] <- df[[col_name]]^2
      }
      
      data(df)
    }, error = function(e) {
      showNotification("Transformation error! Ensure numerical values.", type = "error")
    })
  })
  
  # Display the updated dataset
  output$feature_table <- renderTable({
    req(data())
    head(data(), 10)
  })
  
  # Display feature impact plot
  output$feature_impact_plot <- renderPlot({
    req(data(), input$show_plot, input$transform_column)
    
    df <- data()
    col_name <- input$transform_column
    transformed_col <- paste0(col_name, "_", input$transformation_type)
    
    if (transformed_col %in% colnames(df)) {
      ggplot(df, aes_string(x = col_name, y = transformed_col)) +
        geom_point(alpha = 0.6, color = "blue") +
        theme_minimal() +
        labs(title = paste("Impact of", input$transformation_type, "on", col_name),
             x = col_name, y = transformed_col)
    }
  })
  
  # D) Basic EDA: Summary and Plot
  observeEvent(input$basic_eda, {
    req(data())
    df <- data()
    eda_results <- capture.output(summary(df))
    output$eda_table <- DT::renderDataTable({
      datatable(data.frame(EDA_Summary = eda_results), options = list(pageLength = 10))
    })
  })
  
  # Data Preview
  output$table <- DT::renderDataTable({
    req(data())
    datatable(data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Summary Statistics
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Interactive Plot using Plotly
  output$plot <- renderPlotly({
    req(data(), input$x_var, input$y_var)
    df <- data()
    validate(
      need(is.numeric(df[[input$x_var]]) || is.factor(df[[input$x_var]]),
           "X variable must be numeric or factor."),
      need(is.numeric(df[[input$y_var]]) || is.factor(df[[input$y_var]]),
           "Y variable must be numeric or factor.")
    )
    p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() +
      theme_minimal()
    ggplotly(p)
  })
  
  # Correlation Heatmap (Basic)
  output$corr_plot <- renderPlot({
    req(data())
    df <- data()
    numeric_df <- dplyr::select_if(df, is.numeric)
    if (ncol(numeric_df) < 2) {
      showNotification("Not enough numeric columns for correlation plot.", type = "warning")
      return(NULL)
    }
    cormat <- cor(numeric_df, use = "complete.obs")
    ggplot2::ggplot(data = as.data.frame(as.table(cormat)),
                    aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      coord_fixed() +
      labs(x = "", y = "", fill = "Corr")
  })
  
  # E) Advanced EDA (Univariate, Bivariate,
  #    Correlation) - "Advanced EDA" Tab
  # 1. Dynamically show grouping variable
  output$group_var_ui <- renderUI({
    req(data())
    df <- data()
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    if (input$use_group) {
      selectInput("group_var", "Grouping Variable", choices = cat_vars)
    } else {
      return(NULL)
    }
  })
  
  # 2. Observe "Run Advanced EDA" button
  observeEvent(input$run_eda, {
    req(data())
    df <- data()
    
    # --- Univariate ---
    output$univariate_plot <- renderPlot({
      req(input$uni_var, input$uni_plot_type)
      
      if (input$uni_plot_type == "hist") {
        ggplot(df, aes_string(x = input$uni_var)) +
          geom_histogram(fill = "steelblue", color = "black", bins = 30) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$uni_var), x = input$uni_var, y = "Count")
      } else {
        ggplot(df, aes_string(y = input$uni_var)) +
          geom_boxplot(fill = "steelblue") +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$uni_var), y = input$uni_var)
      }
    })
    
    output$univariate_summary <- renderPrint({
      req(input$uni_var)
      summary(df[[input$uni_var]])
    })
    
    # --- Bivariate ---
    output$bivariate_plot <- renderPlot({
      req(input$bi_xvar, input$bi_yvar)
      
      if (input$use_group && !is.null(input$group_var)) {
        # Grouping is enabled
        gvar <- input$group_var
        
        if (is.numeric(df[[input$bi_xvar]]) && is.numeric(df[[input$bi_yvar]])) {
          # Scatter with color grouping
          ggplot(df, aes_string(x = input$bi_xvar, y = input$bi_yvar, color = gvar)) +
            geom_point() +
            theme_minimal() +
            labs(title = paste("Scatter Plot of", input$bi_yvar, "vs", input$bi_xvar, "by", gvar),
                 x = input$bi_xvar, y = input$bi_yvar, color = gvar)
        } else {
          # Boxplot if y is numeric, x is factor
          ggplot(df, aes_string(x = input$bi_xvar, y = input$bi_yvar, fill = gvar)) +
            geom_boxplot() +
            theme_minimal() +
            labs(title = paste("Boxplot of", input$bi_yvar, "by", input$bi_xvar, "grouped by", gvar),
                 x = input$bi_xvar, y = input$bi_yvar, fill = gvar)
        }
      } else {
        # No grouping
        if (is.numeric(df[[input$bi_xvar]]) && is.numeric(df[[input$bi_yvar]])) {
          # Scatter plot
          ggplot(df, aes_string(x = input$bi_xvar, y = input$bi_yvar)) +
            geom_point(color = "steelblue") +
            theme_minimal() +
            labs(title = paste("Scatter Plot of", input$bi_yvar, "vs", input$bi_xvar),
                 x = input$bi_xvar, y = input$bi_yvar)
        } else {
          # Boxplot if x is factor and y is numeric
          ggplot(df, aes_string(x = input$bi_xvar, y = input$bi_yvar)) +
            geom_boxplot(fill = "steelblue") +
            theme_minimal() +
            labs(title = paste("Boxplot of", input$bi_yvar, "by", input$bi_xvar),
                 x = input$bi_xvar, y = input$bi_yvar)
        }
      }
    })
    
    output$bivariate_summary <- renderPrint({
      req(input$bi_xvar, input$bi_yvar)
      if (is.numeric(df[[input$bi_xvar]]) && is.numeric(df[[input$bi_yvar]])) {
        corr_val <- cor(df[[input$bi_xvar]], df[[input$bi_yvar]], use = "complete.obs")
        cat("Correlation between", input$bi_xvar, "and", input$bi_yvar, "=", corr_val, "\n\n")
        summary(lm(df[[input$bi_yvar]] ~ df[[input$bi_xvar]]))
      } else {
        cat("Bivariate summary not implemented for these variable types.\n")
      }
    })
    
    # --- Correlation Heatmap (Advanced Tab) ---
    output$adv_corr_plot <- renderPlot({
      req(input$corr_vars)
      vars <- input$corr_vars
      if (length(vars) < 2) {
        plot.new()
        text(0.5, 0.5, "Please select at least two numeric variables.")
        return(NULL)
      }
      corr_df <- df[, vars, drop = FALSE]
      cormat <- cor(corr_df, use = "complete.obs")
      cormat_melt <- melt(cormat)
      ggplot(cormat_melt, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                             midpoint = 0, limit = c(-1,1)) +
        theme_minimal() +
        coord_fixed() +
        labs(x = "", y = "", fill = "Corr",
             title = "Correlation Heatmap (Advanced)")
    })
  })
  
  # F) Download Processed Data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("processed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(data())
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# 4) Run the Shiny App
shinyApp(ui = ui, server = server)

