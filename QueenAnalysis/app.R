#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)

queen <- read_rds("queen.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage("Analysis of Queen",
                 tabPanel("Audio Features",
                          sidebarLayout(
                              sidebarPanel(
                                  helpText("Choose one of Queen's 15 studio albums to obtain a graph with each song's audio features."),
                                  varSelectInput("feature", h3("Feature"), 
                                                 queen %>% select(danceability, energy, liveness, speechiness, acousticness)),
                                  selectInput("album", h3("Album"), 
                                              choices = list("Queen", 
                                                             "Queen II", 
                                                             "Sheer Heart Attack", 
                                                             "A Night At The Opera", 
                                                             "A Day At The Races",
                                                             "News Of The World",
                                                             "Jazz",
                                                             "The Game",
                                                             "Flash Gordon",
                                                             "Hot Space",
                                                             "The Works",
                                                             "A Kind Of Magic",
                                                             "The Miracle", 
                                                             "Innuendo",
                                                             "Made In Heaven"), selected = 1),
                                  p(strong("Danceability:"), "describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                                  p(strong("Energy:"), "a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                                  p(strong("Liveness:"), "detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."),
                                  p(strong("Speechiness:"), "detects the presence of spoken words in a track. The more exclusively speech-like the recording, the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech. Values below 0.33 most likely represent music and other non-speech-like tracks."),
                                  p(strong("Accousticness:"), "a confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.")
                                  
                                  )
                              ,
                              mainPanel(
                                  plotOutput("plot")
                              ))    
                 ),
                 tabPanel("About",
                          h2("Project Summary"),
                          p("This project analyzes audio features of songs from Queen's 15 studio albums. The data was sourced from Spotify, specifically from their Web API Reference website."),
                          h2("About Me"), 
                          p("My name is Hannah Valencia and I am a sophomore in the Gov 1005 class at Harvard University. I am concentrating in Economics but I enjoy data science immensely! Contact me at hvalencia@college.harvard.edu with any comments or questions.")
                 )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    observe({
    qtable <- queen %>% filter(album_name == input$album)
    
    output$plot <- renderPlot(
        ggplot(qtable, aes(x = track_name, !!input$feature)) + 
            geom_col(fill = "mediumorchid") + 
            xlab(input$track_name) +
            ylab(input$feature) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
