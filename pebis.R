# install.packages("magick")
# install.packages("imager")

main <- function() {
  # absolute filepath :zany_face:
  read_file("D:\\Bored\\resources\\frieren.jpg")
}

read_file <- function(path) {
  library(imager)
  library(here)
  img <- load.image(here(path))
  width <- dim(img)[1]
  height <- dim(img)[2]
  gray_img <- grayscale(img)
  
  ASCII_WIDTH = 150
  ASCII_HEIGHT = floor((ASCII_WIDTH/width) * (height/2))
  create_ascii(width, height, ASCII_WIDTH, ASCII_HEIGHT, gray_img)

}

create_ascii <- function(width, height, ascii_w, ascii_h, gray_img) {
  options(width=1000)
  write('', file="output.txt")
  for (i in 0:(ascii_h - 1)) {
    for (j in 0:(ascii_w - 1)) {
      bottom_x <- floor((width/ascii_w) * (j)) + 1
      top_x <- floor((width/ascii_w) * (j+1))
      bottom_y <- floor((height/ascii_h) * (i)) + 1
      top_y <- floor((height/ascii_h) * (i+1))
      
      # for each region
      # average the 'brightness' and pick an ascii to use
      roi <- imsub(gray_img, x %in% bottom_x:top_x, y %in% bottom_y:top_y)
      average_brightness = mean(roi)
      # .:-=+*#%@
      thresholds <- list(0.2, 0.4, 0.6, 0.7, 0.8, 0.85, 0.9, 0.94, 0.97, 1.1)
      asciis <- list(" ",".",":","-","=","+","*","#","%","@")
      
      for (k in 1:10) {
        if (average_brightness < thresholds[k]) {
          ascii <- as.character(asciis[(11 - k)])
          cat(ascii)
          cat(ascii, file="output.txt", append=TRUE)
          break
        }
      }
        
      
    }
    cat('\n')
    cat('\n', file="output.txt", append=TRUE)
  }
}

main()
