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
      selectInput("sport", "Choose a sport:",
                  choices = c("Basketball", "Baseball")),
      
      selectInput("Year", "Choose the year you want to see:",
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


server <- function(input, output) {
  

  
  datasetInput <- eventReactive(input$update, 
    switch(input$dataset,
           Basketball = "Basketball",
           Baseball = "Baseball"))
           

  

  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  

  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
