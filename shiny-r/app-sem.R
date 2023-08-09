# Load necessary libraries
library(shiny)
library(shinythemes)
library(shinyalert)
library(readr)
library(lavaan)
library(lavaanPlot)
library(semTable)
library(effectsize)
library(report)

# Define the Shiny UI
ui <- fluidPage(
  useShinyalert(),
  theme = shinytheme("cerulean"),
  titlePanel("SEM Analysis App"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Input"),
      wellPanel(
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        tags$br(),
        tags$h4("Dataset Summary"),
        textOutput("datasetSummary"),
        tags$br(),
        tags$h4("SEM Model Specification"),
        textAreaInput("modelInput", 
                      label = "", 
                      value = '
                      # measurement model
                      trust_combined =~ overallTiAUnderstanding + overallTiATrust
                      # structural model
                      trust_combined ~ SA + tlx_mental + perceivedSafetyCombined
                      ', 
                      rows = 7,
                      width = '100%'),
        tags$br(),
        actionButton("btn", "Run Analysis")
      )
    ),
    mainPanel(
      tags$h3("Analysis Results"),
      tags$h4("SEM Summary:"),
      verbatimTextOutput("summary"),
      tags$h4("SEM Table Results:"),
      uiOutput("semTableOutput"),
      tags$h4("SEM Plot:"),
      imageOutput("plotOutput"),
      tags$h4("Lavaan Model Report:"),
      verbatimTextOutput("lavaanReport"),
      tags$h4("Effect Size Interpretation:"),
      verbatimTextOutput("effectSizeOutput"),
      tags$h4("Used Tools:"),
      verbatimTextOutput("sessionInfoReport")
    )
  )
)

# Define the Shiny server
server <- function(input, output, session) {
  
  # Show a pop-up window with help text when the app starts
  observe({
    shinyalert("You should use this app with the data in <br><br><i><b>dissertation_colley_combined_trust_all_core_pub.csv</b></i>", type = "info", html = TRUE)
  })
  
  observeEvent(input$btn, {
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    combined <- read_csv(inFile$datapath)
    
    # Dataset summary report
    dataset_report <- report::report(combined)
    
    output$datasetSummary <- renderText({
      as.character(dataset_report)
    })
    
    # Use the input model specification
    model2 <- input$modelInput
    
    result2 <- sem(model=model2, data=combined, estimator="mlr")
    
    output$summary <- renderPrint({
      summary(result2, fit.measures=TRUE, standardized=TRUE)
    })
    
    output$semTableOutput <- renderUI({
      HTML(semTable(result2, type = "html", caption ="Results for the SEM."))
    })
    
    tempFilePNG <- tempfile(fileext = ".png")
    pl <- lavaanPlot2(result2, include = "covs", 
                      node_options = list(shape = "box", fontname = "Helvetica"), 
                      edge_options = list(color = "grey"), stars = c("regress"), 
                      coef_labels = TRUE)
    save_png(pl, tempFilePNG)
    
    output$plotOutput <- renderImage({
      list(src = tempFilePNG, alt = "SEM Plot")
    }, deleteFile = TRUE)
    
    # Report for the lavaan model
    output$lavaanReport <- renderPrint({
      report(result2)
    })
    
    # Report for session info
    output$sessionInfoReport <- renderPrint({
      report(sessionInfo())
    })
    
    output$effectSizeOutput <- renderPrint({
      effectsize::interpret(result2)
    })
  })
}

shinyApp(ui = ui, server = server)
