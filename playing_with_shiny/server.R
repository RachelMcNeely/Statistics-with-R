#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  #output$broken <- renderText({"If broken please email WHOEVER."})
  output$dotPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- iris[, 1] 
    y <- iris[,3]
    
    # draw the histogram with the specified number of bins
    plot(x, y, type= input$linetype, col = input$linecol) #anything you type into text input will be used to change line color
    
  })
  output$linetype <- renderUI({    #all of the renders go with a uiOutput in ui.R
    lt = c("p","l","b", "s","o")
    names(lt) <- c("Plot Points", "Plot Line", "Plot both points and lines", "Plot Steps", "OverPlot")
    selectInput("linetype", label= "Choose a plot type", choices = lt, selected = lt[1])
  })
  
})
  
  

