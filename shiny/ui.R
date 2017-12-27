#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

#options(browser = "/Applications/Safari.app")

fluidPage(theme = 'speckle.css',

  titlePanel("Speckle data reduction"),
  
  fluidRow(
    column(4, wellPanel(
      sliderInput("n", "Frame number:", min = 1, max = 2000,
                  value = 1, step = 1, animate=animationOptions(1000, loop=TRUE))
    )),
    
    column(4, wellPanel(
      sliderInput("gamma", "Gamma factor:", min = 0.00, max = 2,
                  value = 1, step = 0.01)
    )),
    
    column(1, wellPanel(
      radioButtons("log", label = "log10", choices = c("Yes", "No"), selected = "No")
    )),
    
    column(2, wellPanel(
      numericInput("Nbins", label = 'N bins', value = 30,
                   min = 3, max = 500, step = 1) 
    ))
  ),

    fluidRow(
    column(4,
           imageOutput("frame") #, height = 100, width = '50%')
    ),

    column(4, #offset = 1,
           plotOutput("hist", width = "auto")
    ),
    
    column(4,
           rglwidgetOutput("rgl", width = "400px", height = "400px")
     )
  )
)
