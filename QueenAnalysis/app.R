#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Loading all necessary libraries to obtain access to necessary commands.

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

# Reading in the dataframes from the rds and csv files.

queen <- read_rds("queen.rds")
lyrics <- read_csv("https://raw.githubusercontent.com/walkerkq/musiclyrics/master/billboard_lyrics_1964-2015.csv")
qtop <- read_rds("qtop.rds")
queenkey <- read_rds("queenkey.rds")
lexical_diversity <- read_rds("lexical_diversity.rds")

# Define UI for application that draws a histogram
# For aesthetic purposes, set the theme to "cyborg" using the shinythemes.

ui <- fluidPage(theme = shinytheme("cyborg"),
                
                # Set up a navigation bar for easy exploration of the website.
                
                navbarPage("Analysis of Queen",
                           
                    # Created a tab called Audio Features with two sub-tabs, one with bar plots and one with density plots.
                    # Added headings to each tab and changed the color of headings to purple, which is carried out throughout the whole app.
                    
                    tabPanel("Audio Features",
                        tabsetPanel(
                            tabPanel("Bar Plot",
                                 h3("Bar Plot of Audio Features", style = "color:mediumorchid"),
                                 br(),
                                 
                                 # Added a sidebar with a panel providing information about the purpose of the graph and directions of how to explore the page.
                                 # Used the selectInput function to create a pull-down menu with the 15 albums as options. Only one can be selected at a time.
                                 
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
                                     
                                     # Added another pull-down menu for the features from the albums.
                                     # Gave five options of which features to explore.
                                     
                                  varSelectInput("feature", h3("Feature"), 
                                                 queen %>% select(danceability, energy, liveness, speechiness, acousticness)),
                                  
                                  # Added paragraphs describing what each feature is and how it is measured.
                                  
                                  p(strong("Danceability:"), "describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                                  p(strong("Energy:"), "a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                                  p(strong("Liveness:"), "detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."),
                                  p(strong("Speechiness:"), "detects the presence of spoken words in a track. The more exclusively speech-like the recording, the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech. Values below 0.33 most likely represent music and other non-speech-like tracks."),
                                  p(strong("Accousticness:"), "a confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.")
                                  
                                  ),
                              
                              # Added a plot for the main panel.
                                
                              mainPanel(
                                  plotOutput("plot")
                                  )
                              )
                          ),
                        
                        # For the second sub-tab, created a density plot with a header also in purple.
                        # Added a break for aesthetic purposes.
                          
                        tabPanel("Density Plot",
                                 h3("Density Plot of Audio Features", style = "color:mediumorchid"),
                                 br(),
                                 
                                 # Added a sidebar with the sidebar panel describing how to explore the page and what it shows.
                                 
                                 sidebarLayout(
                                     sidebarPanel(
                                         helpText("Choose out of Queen's 15 studio albums to obtain a density graph with the album's audio features."),
                                        
                                         # Added a drop-down menu with the features to choose between.
                                         # Added a checkbox group so that multiple albums could be chosen at once to create a layered density plot.
                                         # Automatically selects for their first album, "Queen", upon arrival to the page.
                                         
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
                                                                    "Made In Heaven"), selected = "Queen")),
                                     
                                     # Added a main panel with density plot output.
                                     
                                     mainPanel(
                                         plotOutput("denplot")
                                     )
                                 )
                            )
                        )
                    ),
                 
                # Created a new tab called "Song Sentiment".
                       
                 tabPanel("Song Sentiment",
                    tabsetPanel(
                        
                        # Added a sub-tab to explore the song sentiment plot.
                        # Added a break for aesthetic purposes.
                        
                        tabPanel("Discover",
                                 h3("Song Sentiment", style = "color:mediumorchid"),
                                 br(),
                          
                          # Added a sidebar panel with information about what the graph shows and instructions on how to use it.
                                 
                          sidebarLayout(
                              sidebarPanel(
                                  p("Analyze the musical sentiment of songs from Queen's 15 studio albums."),
                                  p("Each quadrant corresponds to a different sentiment. According to Spotify, valence is a measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry). A song with high energy and high valence would be happy and joyful sounding, whereas a song with low valence and low energy would be described as sad or depressing."),
                                  p("Hover over a data point to find out more information.")
                              ),
                              
                              # Added a plotly graph to the main panel output.
                              # Wrote a paragraph about the graph and what it shows. Discussed trends or things to look for on the graph.
                              
                              mainPanel(
                                  plotlyOutput("plot2"),
                                  h4("About This Graph", style = "color:mediumorchid"),
                                  p("This graph shows a point for each of Queen's songs that appear on their 15 studio albums. Between the quadrants, there is not much of a pattern of where the songs fall, as they are scattered fairly evenly between the four moods. There are noticably less songs in the chill/peaceful quadrant, and there are not many songs that fall in the extremeties of the quadrants (the corners). It would also be helpful to note that this graph does not analyze the senitment of the songs lyrics, so some songs may not actually correspond with their corresponding quadrant.")
                                  )
                              )
                          ),
                        
                        # Created another sub-tab with statistical analysis of the same sentiment plot.
                        
                        tabPanel("Statistical Analysis",
                                 h3("Statistical Analysis of Song Sentiment", style = "color:mediumorchid"),
                                 br(),
                                 
                                 # Added a sidebar panel with instructions of how to interact with the graph.
                                 # Used checkbox inputs to select multiple albums from a list that will then display on the graph, with one album automatically selected.
                                 
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
                                     
                                     # Created a main panel with a plot output and included a paragraph explaining the statistical relationship and notes about the graph.
                                     
                                     mainPanel(
                                         plotOutput("plot3"),
                                         h4("About This Graph", style = "color:mediumorchid"),
                                         p("This graph looks at the relationship between energy and valence. With each album selected, the R-squared value appears in the corresponding color to show the percentage of the album's energy that explains the variation in valence. The lines that appear are linear regressions of valence on energy. The slope shows how an increase in the valence (or positivity) of a song corresponds to an increase in the energy. A positive slope would mean that on the album, songs tend to be sad or happy, while a negative slope would indicate the songs are angry or peaceful.")
                                         )
                                     )
                                 )
                             )
                         ),
                 
                # Created a new tab for Lyric Association.
                # Added a header paragraph with the questions that drove me to look into the lyrical composition of Queen's six chart-topping songs.
                
                 tabPanel("Lyric Association",
                          h3("Lyric Association", style = "color:mediumorchid"),
                          h6("Is there anything about the lyrical composition of Queen's six chart-topping songs that made them so popular? Do any of these songs have repeated words in common that somehow appeal to listeners? Explore the word clouds to find out!",style = "color:gray"),
                          br(),
                          
                          # Added a sidebar with a drop-down menu that gives a choice of Queen's six hit songs.
                          
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
                                  
                                  # Added a slider that adjusts the number of words in the wordcloud from 1 to 100 words, automatically starting at 30.
                                  # Wrote some instructions about how to use the panel and the word cloud.
                                  
                                  sliderInput("max",
                                              "Maximum Number of Words:",
                                              min = 1,  max = 100,  value = 30),
                                  p("Create a lyrical word cloud to see the most frequently used words in each of Queen's six chart-topping songs."),
                                  p("Select which song you would like to see, then use the slider to select how many words appear in the graphic.")),
                              
                              # Set the main panel to output the wordcloud plot.
                              
                              mainPanel(
                                  plotOutput("cloudPlot")
                              )
                              
                          )
                          
                 ),
                 
                # Created another tab with additional features that do not fall into one category.
                
                 tabPanel("More Features!",
                          
                    # Made a sub-tab about musical key makeup of songs. 
                          
                    tabsetPanel(
                        tabPanel("Musical Key Makeup",
                            h3("Musical Key Makeup by Album", style = "color:mediumorchid"),
                          
                            # Added a sidebar with instructions of how to interact with the graph.
                            # Used a checkbox list for selecting which albums appear on the graph. Automatically selects their first three albums.
                            
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
                          
                          # Used the plot output as the main panel.
                          
                          mainPanel(
                              plotlyOutput("keyplot"),
                          )
                        )
                      ),
                        
                        # Created a new sub-tab with song duration.
                      
                        tabPanel("Song Duration",
                            h3("Song Duration by Album", style = "color:mediumorchid"),
                            
                             # Added a sidebar panel with instructions about how to interact with the graph.
                              
                             sidebarLayout(
                                  sidebarPanel(
                                      helpText("Hover over a point on the scatter plot to retrieve the song title and information. You can also try using the zoom and pan feautres located in the top right corner of the graph.")),
                       
                                 # Input the plot to be the output on the mainPanel.
                                 # Wrote about things to note about the graph.
                                  
                                 mainPanel(
                                     plotlyOutput("durationplot"),
                                     h4("About This Graph", style = "color:mediumorchid"),
                                     p("In this graph, you can see that there are many points that hover around the 4-5 minute mark for duration. However, it should be noted that the fitted scale is so large because of the album 'Made In Heaven'. Within this album, there are two songs that are either extremely long or extemely short, with 'Yeah' coming in at 4 seconds long and 'Untitled' coming in at 22 minutes and 33 seconds.")
                                )
                            )
                        ),
                      
                      # Created another sub-tab about lexical diversity.
                      
                      tabPanel("Lexical Diversity",
                        h3("Lexical Diversity by Album", style = "color:mediumorchid"),
                        
                            # Created a sidebar with definitions and information about what the graph shows.
                        
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("Lexical Diversity is a measure of the different number of words used in a song or text. A high lexical diversity means that very few words are repeated throughout, while a low lexical diversity refers to a song that is repetitive."),
                                    helpText("Hover over a point on the scatter plot to retrieve information about the corresponding song.")),
                                
                                    # Made the output of the mainPanel to be the lexical diversity plotly.
                                    # Wrote about what the graph shows and things to note when looking at it.
                                
                                    mainPanel(
                                        plotlyOutput("lexdivplot"),
                                        h4("About This Graph", style = "color:mediumorchid"),
                                        h6("The song 'Yeah' only has one word and therefore has a lexical diversity of 1.0. Also, lyrics to all of Queen's songs could not be retreived, therefore this graph is a sampling of some of their songs, but some may be missing. The least lexically diverse songs are ", a("Get Down, Make Love", href = "https://genius.com/Queen-get-down-make-love-lyrics"),", ", a("Let Me Live", href =  "https://genius.com/Queen-let-me-live-lyrics"),", and ", a("Mustapha", href = "https://genius.com/Queen-mustapha-lyrics"),", all of which have fairly repetitive lyrics. Click on these song titles to see the lyrics to each and observe the repetition.", style = "color:gray")
                                    )
                                )
                            ),
                      
                      # Created another sub-tab about the Key of songs on albums.
                      
                      tabPanel("Song Keys",
                            h3("Key of Songs by Album", style = "color:mediumorchid"),
                            
                                # Created a sidebar panel that describes what the graph shows.
                            
                                sidebarLayout(
                                    sidebarPanel(
                                        helpText("Observe the ratio of major and minor songs in each album.")),
                                    
                                    # Made the main panel output to be the song key plot.
                                    # Wrote about what can be observed from the plot.
                                    
                                    mainPanel(
                                         plotOutput("keysongplot"),
                                         h4("About This Graph", style = "color:mediumorchid"),
                                         p("Most songs are written in the major key, as they sound brighter and more cheerful. This graph shows that for most of Queen's albums, the majority of the album's songs are in the major key. However, the last two albums that Freddie Mercury worked on and was alive for, 'Innuendo' and 'The Miracle', noticably have more songs in the minor key. Songs in the minor key often sound darker and sadder. Both of these albums were released after Mercury's HIV diagnosis, which one could speculate is why there are more sad songs.")
                                    )
                                )
                            )
                        )
                    ),
                 
                 # Created an about tab with information about the project, sources, and the creator (myself).
                
                 tabPanel("About",
                          h3("Project Summary", style = "color:mediumorchid"),
                          p("This project analyzes audio features of songs from Queen's 15 studio albums. The various tabs look at various information gathered about the songs and albums, from their audio features to the senitment they give off. Some of the data was sourced from Spotify, specifically from their ", a("Web API Reference website", href = "https://developer.spotify.com/documentation/web-api/"),". Other data, such as the lyrics and features of Queen's chart-topping songs, was sourced from GitHub user walkerq and can be found ", a("here", href = "https://github.com/walkerkq/musiclyrics"),". I also scraped lyrics from ", a("Genius API for developers", href = "https://docs.genius.com/"),"."),
                          h3("About Me", style = "color:mediumorchid"), 
                          p("My name is Hannah Valencia and I am a sophomore in the Gov 1005 class at Harvard University. I am concentrating in Economics but I enjoy data science immensely! I also love Queen, which is why I decided to create this project. Feel free to contact me at hvalencia@college.harvard.edu with any comments or questions.")
                 )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Used the observe function to filter through the data interactively before it goes into creating the plot.
    # Filtering the data but the ablum name that is selected by the user.
    
    observe({
    qtable <- queen %>% filter(album_name == input$album)
    
    # Setting the plot output to render a ggplot
    # Set the x and y aesthetics, with the y aesthetic set by the user's input from the drop-down menu.
    # Set labels and titles.
    
    output$plot <- renderPlot(
        ggplot(qtable, aes(x = track_name, !!input$feature)) + 
            geom_col(fill = "mediumorchid") + 
            xlab(input$track_name) +
            ylab(input$feature) +
            labs(title = input$feature) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
    })
    
    
    # Used observe to filter the data prior to it being used in the graph.
    # Filtered the album by the user's input.
    
    observe({
        qdenplot <- queen %>% filter(album_name %in% input$albden)
       
        # Set the plot output to create a ggplot using the filtered data.
        # Set the x aesthetic to be the feature chosen by the user.
        # Grouped the data within aesthics and set the fill color.
        # Used geom_denisty to create a density plot.
        # Changed the alpha level so that the various density plots can layer but can still be seen when overlapping.
         
        output$denplot <- renderPlot(
            ggplot(qdenplot, aes(!!input$featden, group = album_name, fill = album_name, text = album_name)) +
                geom_density(alpha=0.7, color=NA)+
                xlab(input$featden) +
                ylab("Density") +
                guides(fill=guide_legend(title="Album"))+
                theme_minimal()
        )
    })
    
    
    # Set the plot output to render a Plotly object.
    
    output$plot2 <- renderPlotly({
        
        # Used the plot_ly function to create a plotly graph.
        # Set the x and y variables from the dataset and colored the data points by album name.
        # Made the plot into a scatterplot.
        # Added the hover-over feature so that when hovering over a point, the song, album, valence, and energy for that specific point will appear in a text box.
        
       plot_ly(data = queen, x = ~valence, y = ~energy, color = ~album_name, colors = "Set1",
               type = 'scatter', mode = 'markers',
               hoverinfo = 'text',
               text = ~paste("Song: ", track_name, "</br>",
                             "</br> Album: ", album_name,
                             "</br> Valence: ", valence,
                             "</br> Energy: ", energy)) %>%
            
            # Created quadrants on the graph.
            
            add_segments(x = 0.5, xend = 0.5, y = 0, yend = 1) %>%
            add_segments(x = 0, xend = 1, y = 0.5, yend = 0.5) %>%
            
            # Added labels and titles.
            # Added annotations to the quadrants to show what mood each quadrant corresponds to.
            
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
    
    
    # 
    
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
    
    
    output$lexdivplot <- renderPlotly({
        plot_ly(data = lexical_diversity, x = ~album_name, y = ~lex_div, color = ~album_name, type = "scatter",
                hoverinfo = 'text',
                text = ~paste("Song: ", track_name, "</br>",
                              "</br> Album: ", album_name,
                              "</br> Lexical Diversity ", lex_div)) %>%
            layout(title = "Lexical Diversity by Album",
                   xaxis = list(title = "Album Name", tickangle = 300),
                   yaxis = list(title = "Lexical Diversity"))
    })
    
    
    observe({
        keysong <- queen %>% group_by(mode_name, album_name) %>% count()
        
        output$keysongplot <- renderPlot(
            ggplot(keysong, aes(x = album_name, y = n, fill = mode_name)) +
                geom_col() +
                scale_fill_manual(values=c("orchid1", "darkorchid4")) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(title = "Key of Songs by Album", x = "Album Name", y = "Number of Songs", fill = "Mode")
        )
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
