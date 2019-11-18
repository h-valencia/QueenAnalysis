#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Navbar!",
                 tabPanel("Audio Features",
                          sidebarLayout(
                              sidebarPanel(
                                  helpText("Choose one of Queen's 15 studio albums to obtain a graph with each song's liveness"),
                                  selectInput("feature", h3("Feature"),
                                              choices = list("Danceability" = 1,
                                                             "Energy" = 2,
                                                             "Liveness" = 3,
                                                             "Speechiness" = 4,
                                                             "Accousticness" = 5)),
                                  selectInput("album", h3("Album"), 
                                              choices = list("Queen" = 1, 
                                                             "Queen II" = 2, 
                                                             "Sheer Heart Attack" = 3, 
                                                             "A Night At The Opera" = 4, 
                                                             "A Day At The Races" = 5,
                                                             "News Of The World" = 6,
                                                             "Jazz" = 7,
                                                             "The Game" = 8,
                                                             "Flash Gordon" = 9,
                                                             "Hot Space" = 10,
                                                             "The Works" = 11,
                                                             "A Kind Of Magic" = 12,
                                                             "The Miracle" = 13, 
                                                             "Innuendo" = 14,
                                                             "Made In Heaven" = 15), selected = 1),
                                  p(strong("Danceability:"), "describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                                  p(strong("Energy:"), "a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                                  p(strong("Liveness:"), "detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."),
                                  p(strong("Speechiness:"), "detects the presence of spoken words in a track. The more exclusively speech-like the recording, the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech. Values below 0.33 most likely represent music and other non-speech-like tracks."),
                                  p(strong("Accousticness:"), "a confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.")
                                  
                                  )
                              ,
                              mainPanel(
                                  plotOutput("distPlot")
                              ))    
                 ),
                 tabPanel("About",
                          h2("Summary"),
                          p("This project analyzes audio features of songs from Queen's 15 studio albums.")
                 ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
