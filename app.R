#
# First, ensure you have the necessary packages installed:
# install.packages("shiny")
# install.packages("readr")
# install.packages("readxl")
# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("DT")
#
#

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
      selectInput("builtin_dataset", "Select Built-in Dataset", choices = c("None", "mtcars", "iris"), selected = "mtcars"),
      actionButton("load_builtin", "Load Built-in Dataset"),
      
      actionButton("clean", "Clean Data"),
      actionButton("preprocess", "Preprocess Data"),
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
  
  # Upload data
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
  
  # Clean data
  observeEvent(input$clean, {
    req(data())
    df <- data()
    df <- df %>% dplyr::drop_na() %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
    data(df)
  })
  
  # Preprocess data
  observeEvent(input$preprocess, {
    req(data())
    df <- data()
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ifelse(. < 0, NA, .)))
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
    req(data())
    df <- data()
    p <- ggplot(df, aes(x = wt, y = mpg)) + geom_point()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)