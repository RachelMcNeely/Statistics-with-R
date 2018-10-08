#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
#This is the part that uses DASHBOARD!
dashboardPage(
  # Application title
  dashboardHeader(title="Old Faithful Geyser Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "plot", icon=icon("bar-chart-o"),text = "Histogram",selected = TRUE),
      menuItem(tabName = "dotplot", icon=icon("bar-chart-o"),text = "Dot Plot")
      #use awesome font to change icon
      
    )
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot", 
              #slider input for number of bins
              box(sliderInput("bins","Number of bins:", min = 1, max = 50,value = 30)),
              #Plot of the generated distribution
              box(plotOutput("distPlot"))),
      tabItem(tabName = "dotplot", 
              #Color for line
              textInput("linecol", label = "Color fo Line", value = "blue"),
              box(uiOutput("linetype")),
              #Plot of the generated distribution
              box(plotOutput("dotPlot")))
    )
    
    
  ),
  skin="red")
