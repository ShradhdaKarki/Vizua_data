library(shiny)

# task4: add in getFreq function for pre-processing

# task6: add in shinythemes function

ui <- fluidPage(
  titlePanel("Shakespeare's Plays Word Frequencies") # Application title
  
 
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  
}

shinyApp(ui = ui, server = server)
