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
                 tabPanel("Danceability",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("select", h3("Album"), 
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
                                                                      "Made In Heaven" = 15), selected = 1))
                                  
                              
                              ,
                              mainPanel(
                                  plotOutput("distPlot")
                              ))    
                          ),
                 tabPanel("Energy",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("select", h3("Album"), 
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
                                                             "Made In Heaven" = 15), selected = 1))
                              
                              
                              ,
                              mainPanel(
                                  plotOutput("distPlot")
                              ))    
                 )
                 )
                 tabPanel("About",
                          h2("Summary")
                 )

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
