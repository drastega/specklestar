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

  titlePanel("Speckle binary star generator"),

  fluidRow(
    column(2, wellPanel(
      sliderInput("m1", label = 'm1', value = 1000,
                  min = 1000, max = 2000, step = 1)
    )),

    column(2, wellPanel(
      sliderInput("m2", label = 'm2', value = 800,
                  min = 50, max = 1000, step = 1)
    )),

    column(2, wellPanel(
      sliderInput("Seeing", label = 'Seeing', value = 15,
                   min = 3, max = 50, step = 1)
    ))
  ),

    fluidRow(
    column(4,
           imageOutput("frame") #, height = 100, width = '50%')
    )
  )
)
