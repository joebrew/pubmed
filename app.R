source('search_pubmed.R')
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(plotly)

# UI #####################################################
ui <- navbarPage(
  title = 'Publicaçoes do CISM',
  theme = shinytheme("flatly"),
    tabPanel('Search',
             fluidRow(
               column(4,
                      textInput('term', 
                                'Search term', 
                                value = paste0('(CISM) OR ',
                                               '(Centro de Investigação em Saude de Manhiça) OR ',
                                               '(Manhiça Health Research Center) OR ',
                                               '(Fundação Manhiça)')),
                      # checkboxInput('abstract', label = 'Only in abstract',
                      # value = TRUE),
                      sliderInput('years', 'Years',
                                  min = 2000, max = 2016, value = c(2015, 2015),
                                  sep = ''),
                      actionButton("redraw", "Redraw")
                      # htmlOutput("author_ui")
                      
               ),
             column(8,
                    titlePanel(title = 'Results'),
                    p('Wait a few seconds while we fetch your data'), 
                    h2(textOutput("status_text")),
                    br(),
                    downloadButton('downloadData', 'Download'),
                    br(),
                    hr(),
                    h2('Visualization'),
                    # plotlyOutput('year_plot'),
                    plotOutput('year_plot'),
                    br(),
                    h2('Interactive table of results'),
                    dataTableOutput('results_table')))
  ),
  tabPanel('About',
           fluidRow(
             column(12, 
                    h1('Publicaçoes do CISM: uma ferramenta aberta'),
                    p('Bla bla bla'))
             # column(4,p('more text')),
             # column(8,p('more text'))
           )))

# SERVER #################################################
server <- function(input, output, session) {
  
  reac <- reactiveValues(redraw = TRUE, 
                         term = isolate(input$term), 
                         years  = isolate(input$years))
  
  # If any inputs are changed, set the redraw parameter to FALSE
  observe({
    input$term
    input$years
    reac$redraw <- FALSE
  })
  
  # This event will also fire for any inputs, but will also fire for
  # a timer and with the 'redraw now' button.
  # The net effect is that when an input is changed, a 5 second timer
  # is started. This will be reset any time that a further input is
  # changed. If it is allowed to lapse (or if the button is pressed)
  # then the inputs are copied into the reactiveValues which in turn
  # trigger the plot to be redrawn.
  observe({
    invalidateLater(5000, session)
    input$term
    input$years
    input$redraw
    isolate(cat(reac$redraw, input$term, input$years, "\n"))
    if (isolate(reac$redraw)) {
      reac$term <- input$term
      reac$years <- input$years
    } else {
      isolate(reac$redraw <- TRUE)
    }
  })
  
  # Create results dataframe
  results <- reactive({
    x <- pubmed(start_year = input$years[1],
                end_year = input$years[2],
                search_topic = input$term,
                counts_only = FALSE)
    # if(!is.null(input$author)){
    #   if(input$author != 'All'){
    #     keeps <- which(x$results$`first _author_last_name` == input$author)
    #     x$results <- x$results[keeps,]
    #     x$abstracts <- x$abstracts[keeps,]
    #   }
    # }
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
  
  # STATUS OF THE QUERY
  output$status_text <- renderText({
    x <- results()
    if(is.null(x)){
      'Preparing your data'
    } else {
      'Your data is ready for download'
    }
  })
  
  # # DYNAMIC UI FOR AUTHOR SELECTION
  # output$author_ui <- renderUI({ 
  #   x <- results()$results
  #   authors <- c('All', sort(unique(x$`first _author_last_name`)))
  #   selectInput("author", "Select only one first author", authors)
  # })

  # VISUALIZATION BY YEAR
  output$year_plot <- renderPlot({
  # output$year_plot <- renderPlotly({
    df <- results()$results
    # # Group by year
    # by_year <- df %>%
    #   group_by(year) %>%
    #   tally
    # ggplot(data = by_year,
    #        aes(x = year,
    #            y = n)) +
    #   geom_area() +
    #   theme_fivethirtyeight()
    
    # Group by year/author
    by_year_author <- df %>%
      group_by(year, `first _author_last_name`) %>%
      tally
    cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(df$`first _author_last_name`)))
    g <- 
      ggplot(data = by_year_author,
           aes(x = year,
               y = n,
               group = `first _author_last_name`,
               fill = `first _author_last_name`)) +
      geom_bar(stat = 'identity', position = 'stack') +
      scale_fill_manual(name = 'Author',
                          values = cols) +
      theme_fivethirtyeight() +
      guides(fill=guide_legend(ncol=10))
    print(g)
    # ggplotly(g)
  })
}

# RUN #####################################################
shinyApp(ui = ui, server = server)