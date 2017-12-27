# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)
library(ff)
library(imager)
library(fftwtools)
library(mrbsizeR) # for fftshift
library(rgl)
library(misc3d)

file_name <- file.choose()
data <- ff(filename = file_name, readonly = TRUE, dim = c(512,512,2000), vmode = 'ushort')

#PS <- 0
#N <- 20
#frame <- array(0, c(1024, 1024))
#for (i in 1:N) {
#  frame[1:512, 1:512] <- data[, , i]
#  PS <- PS + abs(fftw2d(frame))^2
#}

#PS <- fftshift(PS, dimension = -1) / N

function(input, output, session) {
  # frame creates a new PNG file each time frame number n changes
  output$frame <- renderImage({
    # A temp file to save the output.
    outfile <- tempfile(fileext = ".png")
    # Generate the image and write it to file
    if (input$log == "No") {
      pic <- as.cimg(data[, , input$n]^(input$gamma))
    } else {
      pic <- as.cimg(log10(data[, , input$n]^(input$gamma)))
    }
    save.image(pic, outfile)
    # Return a list containing information about the image
    list(src = outfile,
         contentType = "image/png",
         alt = "This is alternate text",
         width = '400px')
    
  }, deleteFile = TRUE)
  
  #   png(filename = outfile)
  #   plot(hist, main='Histogram of current frame')
  #   dev.off()


  output$hist <- renderPlot({
    if (input$log == "No") {
      hist(data[, , input$n]^(input$gamma), main='Histogram of current frame', breaks = input$Nbins)
    } else {
      hist(log10(data[, , input$n]^(input$gamma)), main='Histogram of current frame', breaks = input$Nbins)
    }
  })

  output$hist1 <- renderPlot({
    if (input$log == "No") {
      hist(data[, , input$n]^(input$gamma), main='Histogram of current frame')
    } else {
      hist(log10(data[, , input$n]^(input$gamma)), main='Histogram of current frame')
    }
  })

  
#  open3d(useNULL = TRUE)
#  ids <- plot3d(rnorm(100), rnorm(100), rnorm(100))[1]
# options(rgl.useNULL = TRUE)
# persp3d(1:512, 1:512, data[, , 2])
# scene <- scene3d()
# rgl.close()
# 
# plot3d(scene)
# dev <- rgl.cur()
# save <- options(rgl.inShiny = TRUE)
# on.exit(options(save))
# 
# session$onSessionEnded(function() {
#   rgl.set(dev)
#   rgl.close()
# })

#  options(rgl.useNULL = TRUE)
#  save <- options(rgl.inShiny = TRUE)
#  on.exit(options(save)) 
  
  output$rgl <- renderRglwidget({
    try(rgl.close())
#    rgl.open(useNULL=T)
#    open3d()
#    bg3d("slategray")
    par3d(mouseMode = "trackball")
#    open3d(useNULL = TRUE, windowRect=c(1,1,512,512))
    persp3d(1:512, 1:512, data[, , 2])
#    persp3d(1:512, 1:512, z = log10(ACF_short), col = 'green')
#     scene <- scene3d()
#     plot3d(scene)
#    scatter3d(x = cars$speed, y=cars$dist, z=cars$time) #, surface=FALSE)#, ellipsoid = TRUE)
#    filename_rgl <- writeWebGL(dir = file.path(tempdir(), "webGL"), width = 500, reuse = TRUE)
#    browseURL(paste0("file://", filename_rgl))
    scene <- scene3d()
    plot3d(scene)
    rglwidget()
  })

#  output$PS <- renderImage({
#    # A temp file to save the output.
#    outfile1 <- tempfile(fileext = ".png")
#    # Generate the image and write it to file
#    pic1 <- as.cimg(log10(PS))
#    save.image(pic1, outfile1)
#    list(src = outfile1,
#         contentType = "image/png",
#         alt = "This is alternate text",
#         width = '300px')
#  }, deleteFile = TRUE)
}
