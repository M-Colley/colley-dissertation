# Load necessary libraries
library(shiny)
library(shinythemes)
library(shinyalert)
library(readr)
library(report)
library(dplyr)
library(ggplot2)
library(rlang)

# Define the Shiny UI
ui <- fluidPage(
  useShinyalert(),
  theme = shinytheme("cerulean"),
  #tags$div("You should use this app with the data in dissertation_colley_combined_interest_ease_reality.csv"),
  titlePanel("Data Analysis App - Interest, Ease, Availability"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Input"),
      wellPanel(
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        tags$br(),
        tags$h4("Dataset Summary"),
        textOutput("datasetSummary"),
        tags$br(),
        tags$h4("Linear Regression Specification"),
        uiOutput("regression_input"),
        actionButton("btn", "Run Analysis")
      )
    ),
    mainPanel(
      tags$h3("Analysis Results"),
      tags$h4("Years Summary:"),
      verbatimTextOutput("yearsSummary"),
      tags$h4("Histogram of Dependent Variable:"),
      plotOutput("histPlot"),
      tags$h4("Scatter Plot of Variables:"),
      plotOutput("scatterPlot"),
      tags$h4("Regression Results:"),
      verbatimTextOutput("regressionResults"),
      tags$h4("Used Tools:"),
      verbatimTextOutput("sessionInfoReport")
    )
  )
)

# Define the Shiny server
server <- function(input, output, session) {
  
  # Show a pop-up window with help text when the app starts
  observe({
    shinyalert("You should use this app with the data in <br><br><i><b>dissertation_colley_combined_interest_ease_reality.csv</b></i>", type = "info", html = TRUE)
  })
  
  observeEvent(input$file1, {
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    
    combined <- read_delim(inFile$datapath, delim=";")
    
    # Dataset summary report
    dataset_report <- report::report(combined)
    
    output$datasetSummary <- renderText({
      as.character(dataset_report)
    })
    
    # Create checkboxes for variable selection
    output$regression_input <- renderUI({
      list(
        checkboxGroupInput("dependent_var", "Dependent Variable:", setdiff(names(combined), c("year", "citation"))),
        checkboxGroupInput("independent_var", "Independent Variables:", setdiff(names(combined), c("ease", "interest", "reality", "citation")))
      )
    })
  })
  
  observeEvent(input$btn, {
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    
    combined <- read_delim(inFile$datapath, delim=";")
    
    # Year summary
    if ('year' %in% colnames(combined)) {
      output$yearsSummary <- renderPrint({
        combined %>%
          dplyr::group_by(year) %>%
          dplyr::summarise(
            Mean = mean(!!sym(input$dependent_var), na.rm = TRUE),
            SD = sd(!!sym(input$dependent_var), na.rm = TRUE)
          )
      })
    }
    
    if (length(input$dependent_var) == 1) {
      output$histPlot <- renderPlot({
        ggplot(combined, aes(x = !!sym(input$dependent_var), y = after_stat(density))) +
          geom_histogram(fill = "skyblue", color = "black", binwidth = 1) +
          geom_density(alpha=.2, fill="#FF6666") +
          theme_minimal() +
          labs(title = "Histogram of Dependent Variable", x = input$dependent_var, y = "Density") +
          facet_wrap(~year)
      })
    }
    
    # Regression
    if (length(input$dependent_var) > 1 || length(input$independent_var) < 1) {
      output$regressionResults <- renderPrint({
        "Please select exactly one dependent variable and at least one independent variable."
      })
    } else {
      dependent_var <- input$dependent_var
      independent_var <- input$independent_var
      
      #dependent_var <- as.factor(dependent_var)
      
      # Standardize the dependent variable
      #combined[dependent_var] <- standardize(combined[dependent_var])
      
      model_formula <- as.formula(paste(dependent_var, "~", paste(independent_var, collapse = " + ")))
      print(model_formula)
      model <- lm(model_formula, data = combined)
      
      
      output$regressionResults <- renderPrint({
        summary(model)
        #report::report(model)
      })
      
      # Scatter Plot of Variables
      if (length(input$independent_var) == 1) {
        output$scatterPlot <- renderPlot({
          ggplot(combined, aes(x = !!sym(input$independent_var), y = !!sym(input$dependent_var))) +
            geom_point(color = "blue") +
            geom_smooth(method = "lm", se = FALSE, color = "red") +
            theme_minimal() +
            labs(title = "Scatter Plot of Variables", x = input$independent_var, y = input$dependent_var)
        })
      }
    }
    
    # Report for session info
    output$sessionInfoReport <- renderPrint({
      report(sessionInfo())
    })
  })
}

shinyApp(ui = ui, server = server)
