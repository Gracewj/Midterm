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
      
      # Output: Header + summary of distribution ----
      h4("Attendance plot"),
      verbatimTextOutput("Attendance plot"),
      
      # Output: Header + table of distribution ----
      h4("weather"),
      tableOutput("weather")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
