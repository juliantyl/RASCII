# install.packages("magick")
# install.packages("imager")

add_numbers <- function(a, b) {
  sum <- a + b
  return(sum)
}

main <- function() {
  read_file("D:\\Bored\\frieren3.jpg")
}

read_file <- function(path) {
  library(imager)
  
  img <- load.image(path)
  width <- dim(img)[1]
  height <- dim(img)[2]
  print(width)
  print(height)
  gray_img <- grayscale(img)
  roi <- imsub(gray_img, x %in% 0:1000, y %in% 0:1080)
  # print(roi)
  print(dim(roi))
  
  # plot(roi)
  
  ASCII_WIDTH = 350
  ASCII_HEIGHT = floor((ASCII_WIDTH/width) * (height/2))
  print(ASCII_HEIGHT)
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
      # count the shaded pixels
      # determine if more than 50% are shaded
      # then add either '#' or ' '
      roi <- imsub(gray_img, x %in% bottom_x:top_x, y %in% bottom_y:top_y)
      # print(bottom_x)
      # print(top_x)
      # print(bottom_y)
      # print(top_y)
      # print(as.numeric(roi))
      average_brightness = mean(roi)
      # print(average_brightness)
      # .:-=+*#%@
      # thresholds <- list(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
      thresholds <- list(0.2, 0.4, 0.6, 0.7, 0.8, 0.85, 0.9, 0.94, 0.97)
      # thresholds <- list(0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.55, 0.7, 0.9)
      if (average_brightness < thresholds[1]) {
        cat('@')
        cat('@', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[2]) {
        cat('%')
        cat('%', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[3]) {
        cat('#')
        cat('#', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[4]) {
        cat('*')
        cat('*', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[5]) {
        cat('+')
        cat('+', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[6]) {
        cat('=')
        cat('=', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[7]) {
        cat('-')
        cat('-', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[8]) {
        cat(':')
        cat(':', file="output.txt", append=TRUE)
      }
      else if (average_brightness < thresholds[9]) {
        cat('.')
        cat('.', file="output.txt", append=TRUE)
      }
      else {
        cat(' ')
        cat(' ', file="output.txt", append=TRUE)
      }
    }
    cat('\n')
    cat('\n', file="output.txt", append=TRUE)
  }
}

main()
