library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Weather vs Sports"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("Sport", "sport:",
                  choices = c("Basketball", "Baseball")),
      
      selectInput("Year", "year:",
                  choices = c("2012", "2013","2014","2015","2016","2017")),
      
      # Include clarifying text ----
      helpText("Note: We collected the data from Basketball reference and Baseball reference."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("Show", "Go")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: summary of distribution
      h4("Attendance plot"),
      verbatimTextOutput("Attendance plot"),
      
      # Output: weather of that day
      h4("weather"),
      tableOutput("weather")
    )
    
  )
)








# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only updated when the user clicks the button
  
  datasetInput <- eventReactive(input$Show, 
    switch(input$dataset,
           Basketball = "Basketball",
              2012 = "2012BK",
              2013 = "2013BK",
              2014 = "2014BK",
              2015 = "2015BK",
              2016 = "2016BK",
              2017 = "2017BK",
           
           Baseball = "Baseball",
             2012 = "2012BS",
             2013 = "2013BS",
             2014 = "2014BS",
             2015 = "2015BS",
             2016 = "2016BS",
             2017 = "2017Bs",
    
    ))
           

  

  output$sport <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  

  output$year <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
