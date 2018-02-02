# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(specklestar)
library(imager)

data <- matrix(speckle_generator(seeing = 30, speckle_sigma = 1, m1 = 1000, m2 = 800, rho_x = 50, rho_y = 50, wind = 0), 512, 512)

function(input, output, session) {
  # frame creates a new PNG file each time frame number n changes
  output$frame <- renderImage({
    # A temp file to save the output.
    outfile <- tempfile(fileext = ".png")
    # Generate the image and write it to file
    data <- matrix(speckle_generator(seeing = input$Seeing,
                                     speckle_sigma = 1,
                                     m1 = input$m1,
                                     m2 = input$m2,
                                     rho_x = 50,
                                     rho_y = 50,
                                     wind = 0
                                     ), 512, 512)
    pic <- as.cimg(data)
    save.image(pic, outfile)
    # Return a list containing information about the image
    list(src = outfile,
         contentType = "image/png",
         alt = "This is alternate text",
         width = '400px')

  }, deleteFile = TRUE)
}
