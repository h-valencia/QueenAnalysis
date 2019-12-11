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
library(memoise)
library(wordcloud)
library(tm)
library(SnowballC)
library(ggplot2)
library(spotifyr)
library(ggrepel)
library(tidyverse)

queen <- read_rds("queen.rds")
lyrics <- read_csv("https://raw.githubusercontent.com/walkerkq/musiclyrics/master/billboard_lyrics_1964-2015.csv")
qtop <- lyrics %>%
    filter(Artist == "queen") %>%
    na.omit() %>%
    mutate(Song = c("Killer Queen", 
                    "Bohemian Rhapsody", 
                    "You're My Best Friend", 
                    "Somebody To Love",
                    "Crazy Little Thing Called Love",
                    "Another One Bites The Dust",
                    "Bohemian Rhapsody")) %>%
    filter(!Year == 1992) %>%
    select(Rank, Song, Lyrics)
queenkey <- queen %>%
    select(album_name, key_name) %>%
    group_by(album_name, key_name) %>%
    mutate(n=n()) %>%
    unique() %>%
    group_by(key_name) %>%
    mutate(total=sum(n)) %>%
    mutate(percent=round((n/total)*100))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage("Analysis of Queen",
                 tabPanel("Audio Features",
                    tabsetPanel(
                        tabPanel("Bar Plot",
                                 h3("Bar Plot of Audio Features", style = "color:mediumorchid"),
                                 br(),
                          sidebarLayout(
                              sidebarPanel(
                                  helpText("Choose one of Queen's 15 studio albums to obtain a graph with each song's audio features."),
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
                                  varSelectInput("feature", h3("Feature"), 
                                                 queen %>% select(danceability, energy, liveness, speechiness, acousticness)),
                                  
                                  p(strong("Danceability:"), "describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                                  p(strong("Energy:"), "a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                                  p(strong("Liveness:"), "detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."),
                                  p(strong("Speechiness:"), "detects the presence of spoken words in a track. The more exclusively speech-like the recording, the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech. Values below 0.33 most likely represent music and other non-speech-like tracks."),
                                  p(strong("Accousticness:"), "a confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.")
                                  
                                  ),
                              
                              mainPanel(
                                  plotOutput("plot")
                                  )
                              )
                          ),
                        
                        tabPanel("Density Plot",
                                 h3("Density Plot of Audio Features", style = "color:mediumorchid"),
                                 br(),
                                 sidebarLayout(
                                     sidebarPanel(
                                         helpText("Choose out of Queen's 15 studio albums to obtain a density graph with the album's audio features."),
                                         varSelectInput("featden", h3("Feature"), 
                                                        queen %>% select(danceability, energy, liveness, speechiness, acousticness)),
                                         checkboxGroupInput("albden", h3("Album"), 
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
                                         plotOutput("denplot")
                                     )
                                 )
                            )
                        )
                    ),
                    
                 tabPanel("Song Sentiment",
                    tabsetPanel(
                        tabPanel("Discover",
                                 h3("Song Sentiment", style = "color:mediumorchid"),
                                 br(),
                          sidebarLayout(
                              sidebarPanel(
                                  p("Analyze the musical sentiment of songs from Queen's 15 studio albums."),
                                  p("Each quadrant corresponds to a different sentiment. According to Spotify, valence is a measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry). A song with high energy and high valence would be happy and joyful sounding, whereas a song with low valence and low energy would be described as sad or depressing."),
                                  p("Hover over a data point to find out more information.")
                              ),
                              mainPanel(
                                  plotlyOutput("plot2")
                                  )
                              )
                          ),
                        
                        tabPanel("Statistical Analysis",
                                 h3("Statistical Analysis of Song Sentiment", style = "color:mediumorchid"),
                                 br(),
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
                                         h4("About This Graph", style = "color:mediumorchid"),
                                         p("This graph looks at the relationship between energy and valence. With each album selected, the R-squared value appears in the corresponding color to show the percentage of the album's energy that explains the variation in valence. The lines that appear are linear regressions of valence on energy. The slope shows how an increase in the valence (or positivity) of a song corresponds to an increase in the energy. A positive slope would mean that on the album, songs tend to be sad or happy, while a negative slope would indicate the songs are angry or peaceful.")
                                         )
                                     )
                                 )
                             )
                         ),
                 
                 tabPanel("Lyric Association",
                          h3("Lyric Association", style = "color:mediumorchid"),
                          h6("Is there anything about the lyrical composition of Queen's six chart-topping songs that made them so popular? Do any of these songs have repeated words in common that somehow appeal to listeners? Explore the word clouds to find out!",style = "color:gray"),
                          br(),
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("selection", "Choose a song:",
                                              choices = list("Killer Queen",
                                                             "Bohemian Rhapsody",
                                                             "You're My Best Friend",
                                                             "Somebody To Love",
                                                             "Crazy Little Thing Called Love",
                                                             "Another One Bites The Dust")),
                                  hr(),
                                  sliderInput("max",
                                              "Maximum Number of Words:",
                                              min = 1,  max = 100,  value = 30),
                                  p("Create a lyrical word cloud to see the most frequently used words in each of Queen's six chart-topping songs."),
                                  p("Select which song you would like to see, then use the slider to select how many words appear in the graphic.")),
                              
                              mainPanel(
                                  plotOutput("cloudPlot")
                              )
                              
                          )
                          
                 ),
                 
                 tabPanel("More Features!",
                    tabsetPanel(
                        tabPanel("Musical Key Makeup",
                            h3("Musical Key Makeup by Album", style = "color:mediumorchid"),
                          sidebarLayout(
                              sidebarPanel(
                                  helpText("Choose an album to view the number of songs in each key. Hover over the section of the bar to find out more information."),
                                  checkboxGroupInput("keyalb", h3("Album"),
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
                                                                    "Made In Heaven"), selected = c("Queen",
                                                                                                    "Queen II",
                                                                                                    "Sheer Heart Attack"))
                          ),
                          
                          mainPanel(
                              plotlyOutput("keyplot"),
                          )
                        )
                      ),
                        
                        tabPanel("Songs Duration",
                            h3("Song Duration by Album", style = "color:mediumorchid"),
                              sidebarLayout(
                                  sidebarPanel(
                                      helpText("Hover over a point on the scatter plot to retrieve the song title and information. You can also try using the zoom and pan feautres located in the top right corner of the graph.")),
                       
                                 mainPanel(
                                     plotlyOutput("durationplot"),
                                     h5("Notes About This Graph", style = "color:mediumorchid"),
                                     p("In this graph, you can see that there are many points that hover around the 4-5 minute mark for duration. However, it should be noted that the fitted scale is so large because of the album 'Made In Heaven'. Within this album, there are two songs that are either extremely long or extemely short, with 'Yeah' coming in at 4 seconds long and 'Untitled' coming in at 22 minutes and 33 seconds.")
                                )
                            )
                        )
                    )
                ),
                 
                 tabPanel("About",
                          h3("Project Summary", style = "color:mediumorchid"),
                          p("This project analyzes audio features of songs from Queen's 15 studio albums. The various tabs explore various songs' danceability, energy, valence, loudness, acousticness, speechiness, and sentiment. The data was sourced from Spotify, specifically from their Web API Reference website. Some of the data was also sourced from kaggle, which gave a dataset with the lyrics to Queen's top hits."),
                          h3("About Me", style = "color:mediumorchid"), 
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
    
    
    observe({
        qdenplot <- queen %>% filter(album_name %in% input$albden)
        
        output$denplot <- renderPlot(
            ggplot(qdenplot, aes(!!input$featden, group = album_name, fill = album_name, text = album_name)) +
                geom_density(alpha=0.7, color=NA)+
                xlab(input$featden) +
                ylab("Density") +
                guides(fill=guide_legend(title="Album"))+
                theme_minimal()
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
    
    
    observe({
        
        qcloud <- qtop %>%
            filter(Song == input$selection) %>%
            select(Lyrics)
        docs <- Corpus(VectorSource(qcloud))
        
        dtm <- TermDocumentMatrix(docs)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
        
        set.seed(1234)
        
        output$cloudPlot <- renderPlot(
            wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                      max.words=input$max, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2")))
    })
    
    observe({
        qkey <- queenkey %>% filter(album_name %in% input$keyalb)
        
        keyplot1 <- ggplot(qkey, aes(x = key_name, fill = album_name, y = n, 
                             text = paste("Number of Songs: ", n, "<br>",
                                          "Album: ", album_name))) +
            geom_bar(width=0.5, stat = "identity") +
            labs(x="Key", y="Number of Songs") +
            guides(fill=guide_legend(title="Album")) +
            theme_minimal() +
            ggtitle("Musical Key Makeup by Album")
        
        output$keyplot <- renderPlotly({
            ggplotly(keyplot1, tooltip=c("text"))
            
        })
    })
    
    output$durationplot <- renderPlotly({
        plot_ly(data = queen, x = ~album_name, y = ~(duration_ms/60000), color = ~album_name, type = "scatter",
                hoverinfo = 'text',
                text = ~paste("Song: ", track_name, "</br>",
                              "</br> Album: ", album_name,
                              "</br> Duration (mins): ", duration_ms/60000,
                              "</br> Duration (sec): ", duration_ms/1000)) %>%
        layout(title = "Song Duration by Album",
               xaxis = list(title = "Album Name", tickangle = 300),
               yaxis = list(title = "Duration (in Minutes)"))
    })
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
