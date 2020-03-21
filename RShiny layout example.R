library(tidyverse)
library(shiny)

rm(list = ls())
dummy <- as.data.frame(rownames(USArrests))
colnames(dummy) <- "State"
dummy2 <- USArrests
data <- cbind(dummy, dummy2) 
row.names(data) <- NULL
rm(dummy, dummy2)

# Now let's make a simple rshiny app, starting with the ui
ui <- shinyUI(
  
  navbarPage("RShiny Example",
             
             tabPanel("Intro page",
                      "Random page with random text. Useful if you want an intro page"),
             
             tabPanel("Chart page",
                      pageWithSidebar(
                        headerPanel("Chart"),
                        sidebarPanel(
                          selectInput(
                            inputId = 'selected_crime1', 
                            label = 'Selected crime:', 
                            choices = c('Murder',
                                        'Assault',
                                        'Rape'),
                            selected = c('Murder')),
                          
                          width = 2), # End of sidebar
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Crime bar chart", 
                                     plotOutput('chart')
                            )
                          ),
                          width = 10) # End of mainPanel
                        
                      ) # End of pagewithSidebar
             ), #End of Energy Use
             
             tabPanel("Data page",
                      pageWithSidebar(
                        headerPanel("Data"),
                        sidebarPanel(
                          conditionalPanel(condition = "input.tab==1",
                          selectInput(
                            inputId = 'selected_crime2', 
                            label = 'Selected crime:', 
                            choices = c('Murder',
                                        'Assault',
                                        'Rape'),
                            selected = c('Murder'))
                          
                          ),
                          
                          conditionalPanel(condition = "input.tab==2"
                                           
                                           
                          ),
                          
                          width = 2), # End of sidebar
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Original data", 
                                     value = 1,
                                     tableOutput('data')
                            ),
                            tabPanel("Blank tab",
                            value = 2,
                            "This tab is blank"),
                            id="tab"
                          ),
                          width = 10) # End of mainPanel
                        
                      ) # End of pagewithSidebar
             )
             
  ) # End of navbarPage
) # End of UI

server <- shinyServer(function(input,output, session)({
  
  chartData <- reactive({
    
    final_data <- data %>%
      mutate(`Number (in 100,000s)` = !! rlang::sym(input$selected_crime1)) 
    
    return(final_data)
    
  })
  
  output$chart <- renderPlot({
    ggplot(data = chartData(),
           aes(x = State, y = `Number (in 100,000s)`)) +
      geom_bar(stat = "identity") +
      coord_flip() 
  })
  
  tableData <- reactive({
    
    final_data <- data %>%
      mutate(`Number (in 100,000s)` = !! rlang::sym(input$selected_crime2)) %>%
      select(State, `Number (in 100,000s)`)
    
    return(final_data)
    
  })
  
  output$data <- renderTable({
    tableData() 
  })
  
}))

shinyApp(ui, server)
