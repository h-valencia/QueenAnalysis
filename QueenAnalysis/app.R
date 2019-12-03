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
library(plotly)
library(ggpubr)
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
                 tabPanel("Song Sentiment",
                    tabsetPanel(
                        tabPanel("Discover",
                          sidebarLayout(
                              sidebarPanel(
                                  p("Analyze the musical sentiment of songs from Queen's 15 studio albums."),
                                  p("Each quadrant corresponds to a different sentiment. According to Spotify, valence is a measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry). A song with high energy and high valence would be happy and joyful sounding, whereas a song with low valence and low energy would be described as sad or depressing."),
                                  p("Hover over a data point to find out more information.")
                              ),
                              mainPanel(
                                  plotlyOutput("plot2")
                              ))),
                        tabPanel("Statistical Analysis",
                                 sidebarLayout(
                                     sidebarPanel(
                                        helpText("Choose an album to insert its regression line on the graph."),
                                        checkboxGroupInput("albs", h3("Album"),
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
                                                                          "Made In Heaven"), selected = "Queen")
                                                 ),
                                     
                                     mainPanel(
                                         plotOutput("plot3"),
                                         h3("About This Graph"),
                                         p("This graph looks at the relationship between energy and valence.")
                                         )
                                     )
                                 )
                             )
                         ),
                 
                 
                 tabPanel("Lyric Association",
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput("words",
                                              "Number of words: ",
                                              min = 10,
                                              max = 100,
                                              value = 50),
                              p("Create a lyrical word cloud to see the most frequently used words in each of Queen's six chart-topping songs."),
                              p("Use the slider to select how many words appear in the graphic.")),
                              
                              mainPanel(
                                  plotOutput("cloudPlot")
                              )
                              
                          )
                     
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
            theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
                panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                panel.grid.major = element_blank(),
                axis.text.y = element_text(color = "white"),
                axis.title.x = element_text(color = "white"),
                axis.title.y = element_text(color = "white"))
        )
    })
    
    
    output$plot2 <- renderPlotly({
        
       plot_ly(data = queen, x = ~valence, y = ~energy, color = ~album_name, colors = "Set1",
               type = 'scatter', mode = 'markers',
               hoverinfo = 'text',
               text = ~paste("Song: ", track_name, "</br>",
                             "</br> Album: ", album_name,
                             "</br> Valence: ", valence,
                             "</br> Energy: ", energy)) %>%
            add_segments(x = 0.5, xend = 0.5, y = 0, yend = 1) %>%
            add_segments(x = 0, xend = 1, y = 0.5, yend = 0.5) %>%
            layout(title = 'Song Sentiment',
                   xaxis = list(title = 'Valence',
                                zeroline = TRUE,
                                range = c(0, 1)),
                   yaxis = list(title = 'Energy',
                                range = c(0,1)),
                   annotations = list(text = c("Turbulent/Angry", "Sad/Depressing", "Happy/Joyful", "Chill/Peaceful"),  
                                      x = c(0.15, 0.15, 0.85, 0.85), 
                                      y = c(1, 0.05, 1, 0.05),
                                      showarrow=FALSE))
    })
    
    
    observe({
        qplot3 <- queen %>% filter(album_name %in% input$albs)
        
    output$plot3 <- renderPlot(
        ggplot(qplot3, aes(x = valence, y = energy, group = album_name, color = album_name)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE) +
            geom_vline(xintercept = .5) +
            geom_hline(yintercept = .5) +
            labs(x = "Valence", y = "Energy", title = "Song Sentiment", fill = "Album Name")+
            stat_cor(label.x = .1)
        )
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
