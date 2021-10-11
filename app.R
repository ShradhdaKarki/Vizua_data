library(shiny)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(tibble)
library(dplyr)
library(tidyverse)
library(shinythemes)


getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")



# task4: add in getFreq function for pre-processing

# task6: add in shinythemes function

ui <- fluidPage(theme=shinytheme("cyborg"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(selectInput("Selection","Choose Book",books),
                 checkboxInput("StopWord","Stop Word",value=TRUE),
                 actionButton("RunButton","ReRun"),
                 hr(),
                 h3("Word Cloud Settings"),
                 sliderInput("Max","Max # of WOrds",min = 10,  max = 200, value = 100, step = 10),
                 sliderInput("Large","Size of Largest Words",min = 1, max = 8, value = 4),
                 sliderInput("Small","Size of Smallest Word",min = 0.1, max = 4, value = 0.5),
                 hr(),
                 h3("Word Cloud Settings"),
                 sliderInput("MinimumWord","Minimum Word For Count Chart",min = 10,  max = 100, value = 25),
                 sliderInput("WordSize","Word Size For Counts Chart",min = 8,  max = 30, value = 14),
                 
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Word Cloud",plotOutput("cloud",height="1000px",width="800px")),
                  tabPanel("Word Counts",plotOutput("freq",height="1000px",width="800px")))
    ),
    position=c("left","right"),
    fluid=TRUE
  )
  )
  
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights


server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq<-eventReactive(input$RunButton,withProgress({
    setProgress(message = "Processing Corpus...")
    getFreq(input$Selection,input$StopWord) # ... = replace with the two inputs from Task 2
  }))
  

  
  output$cloud<-renderPlot({v <- freq()
  pal <- brewer.pal(8,"Dark2")
  
  v %>% 
    with(
      wordcloud(
        word, 
        n, 
        scale = c(input$Small, input$Large),
        random.order = FALSE, 
        max.words = input$Max, 
        colors=pal))
  })
  
 
  
#   output$freq<-renderPlot({v<-freq()
#           v%>%ggplot(data=freq)+geom_bar(mapping=aes(reorder(x=word),y=n))+ filter(x=input$MinimumWord)
#           +theme(legend.text = element_text(input$WordSize))
#                            
#   })
# }

output$freq<-renderPlot({
  v<-freq()
  v%>%filter(n>input$MinimumWord)%>%
    ggplot(aes(reorder(word,n),n))+
    geom_col()+
    coord_flip()+
    theme(text=element_text(size=input$WordSize),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
})
}

shinyApp(ui = ui, server = server)
