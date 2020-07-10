#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(encoding="UTF-8")
library(shiny)
library(shinyjs)
library(shinydashboard)
library(tm)
library(tibble)
library(tibble)
library(wordcloud2)
library(tidytext)
library(dplyr)
library(httr)


response <- GET(url="https://www.dropbox.com/s/ceeby99do97s4zy/tweets.df.RData?dl=1")
writeBin(response$content, "tweets.df.RData")
load("tweets.df.RData")

extra.stop.words <- c('com','www','ly','twitter','pic','bit','fb','d',
                      'twitter.com','juntosecuadorpic.twitter.com','tusaludtuderechopic.twitter.com',
                      'de')

ui <- dashboardPage(
    dashboardHeader(title = 'App interactiva'
    ),
    dashboardSidebar(
        collapsed = FALSE,"INSTRUCCIONES:",
        tags$footer(
            tags$p("escriba la palabra deseada en minúsculas, sin tildes y sin símbolos especiales como '#',
                   posteriormente presione el botón para generar una nube de palabras de todos los tweets que 
                   contengan la palabra escrita.")),
        textInput("caption", "Caption", "Ingrese palabra"),
        verbatimTextOutput("value"),
        actionButton(inputId = "button",label = "Buscar y graficar")
        
    ),
    dashboardBody(
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
        wordcloud2Output("wordc", width = "50%", height = "350px")
    )
)

server <- function(input, output) {
    addClass(selector = "body", class = "sidebar-collapse")
    
    presion <- observeEvent(input$button, {
        
        output$wordc <- renderWordcloud2({
            
            selection <- tweets.df[grep(paste(paste0('\\<',input$caption,'\\>'), collapse="|"), tweets.df$Text),]
            
            sw <- readLines("https://www.dropbox.com/s/228vir1q7do06td/stop_words_spanish.txt?dl=1",warn = F)
            
            # stopwords dataframe
            stopwords.df <- tibble(
                word = c(sw, 
                         # in case there are english tweets
                         stopwords(kind = 'en'),  
                         extra.stop.words)
            )
            
            
            
            # remove additional stopwords
            words.df <- selection %>% 
                unnest_tokens(input = Text, output = word) %>% 
                anti_join(y = stopwords.df, by = 'word')
            
            word.count <- words.df %>% count(word, sort = TRUE)
            word.count$n[1] <-  word.count$n[1] / 10
            
            
            wordcloud2(word.count, color = 'random-dark', size= 1.1,)
            
        })
        
    })
    
    
    #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
    #output$value <- renderText({ input$caption })
    
    
    
}

shinyApp(ui, server)