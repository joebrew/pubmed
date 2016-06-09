source('search_pubmed.R')
library(ggplot2)
# UI #####################################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput('term', 'Search term', value = 'Centro de investigação em Saude de Manhiça'),
      # checkboxInput('abstract', label = 'Only in abstract',
                    # value = TRUE),
      sliderInput('years', 'Years',
                  min = 2000, max = 2016, value = c(2015, 2016),
                  sep = ''),
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("simple_plot"),
              downloadButton('downloadData', 'Download'),
              dataTableOutput('results_table'))
  )
)

# SERVER #################################################
server <- function(input, output) {
  
  # Create results dataframe
  results <- reactive({
    x <- pubmed(start_year = input$years[1],
           end_year = input$years[2],
           search_topic = input$term,
           counts_only = FALSE)
    # y <- cbind(x$results, x$abstracts)
    # y <- y$results
    # y
    x
  })
  
  # Produce table of restuls
  output$results_table <- renderDataTable({
    x <- results()$results
    x
  })
  
  
  # Download table
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$term, '.csv', sep='') },
    content = function(file) {
      x <- results()
      y <- cbind(x$results, x$abstracts)
      write.csv(y, file)
    }
  )

  
  output$simple_plot <- renderPlot({
    ggplot()
  })
}

# RUN #####################################################
shinyApp(ui = ui, server = server)