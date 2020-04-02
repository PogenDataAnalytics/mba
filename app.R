library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Market Basket Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("confidence",
                     "Confidence:",
                     min = 1,
                     max = 100,
                     value = 20)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
      )
   )
)

# SERVER
server <- function(input, output) {
   
   output$distPlot <- renderPlot({

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

