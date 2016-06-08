# UI #####################################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput('term', 'Search term', value = 'Florida mosquito control'),
      # checkboxInput('abstract', label = 'Only in abstract',
                    # value = TRUE),
      sliderInput('years', 'Years',
                  min = 2000, max = 2016, value = c(2015, 2016),
                  sep = ''),
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"),
              tableOutput('results_table'))
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
    y <- cbind(x$results, x$abstracts)
    y
  })
  
  # Produce table of restuls
  output$results_table <- renderTable({
    x <- results()
    x
  })

  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

# RUN #####################################################
shinyApp(ui = ui, server = server)