library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Simple Shiny Statistics App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      selectInput("variable", "Choose a variable:", choices = NULL),
      actionButton("go", "Go!")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Statistics",
                           h2("Statistics"),
                           verbatimTextOutput("table")),
                  tabPanel("Plot",
                           plotOutput("pie"),
                           downloadButton("downloadPie", "Download Plot")),
                  tabPanel("Summary",
                           h2("Summary"),
                           verbatimTextOutput("summary"),
                           plotOutput("summaryPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactive({
    req(input$file)
    data <- read.csv(input$file$datapath)
    non_numeric_cols <- sapply(data, function(col) !is.numeric(col))
    updateSelectInput(session, "variable", choices = colnames(data)[non_numeric_cols])
    data
  })
  
  output$table <- renderPrint({
    table(values()[, input$variable])
  })
  
  output$pie <- renderPlot({
    pie(table(values()[, input$variable]), main = "Pie Chart", xlab = input$variable)
  })
  
  output$downloadPie <- downloadHandler(
    filename = function() {
      paste(input$variable, ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  output$summary <- renderPrint({
    cat("\nSummary of all information:\n")
    cat("\nTable:\n")
    print(table(values()[, input$variable]))
  })
  
  output$summaryPlot <- renderPlot({
    pie(table(values()[, input$variable]), main = "Pie Chart", xlab = input$variable)
  })
}

shinyApp(ui = ui, server = server)
