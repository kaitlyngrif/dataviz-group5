library(tidyverse)
library(hexbin)
library(Rcpp)

`%ni%` <- Negate(`%in%`)
seconds_calc <- function(timestr) {
  if (is.vector(timestr)) {
    # If timestr is a vector, apply the function element-wise
    total_secs <- sapply(timestr, function(x) {
      temp <- strsplit(x, split = ':')
      mins <- as.numeric(temp[[1]][1])
      secs <- as.numeric(temp[[1]][2])
      mins * 60 + secs
    })
  } else {
    # If timestr is a single value, process it directly
    temp <- strsplit(timestr, split = ':')
    mins <- as.numeric(temp[[1]][1])
    secs <- as.numeric(temp[[1]][2])
    total_secs <- mins * 60 + secs
  }
  return(total_secs)
}

fun.draw_rink <- function(lines_scale = 0.5, rink_type = "full") {
  
  ## Necessary objects for plotting
  xseq <- seq(-4, 4, length = 100)
  theta1 <- seq(0, 2 * pi, length = 300)
  theta <- seq(0, 2 * pi, length = 300)
  dd <- (5 + 7 / 12) / 2
  lines_mult <- lines_scale #<- 0.5
  
  
  ## Full NHL Rink
  if (rink_type == "full") { 
    
    rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
      
      ## Rink border
      geom_path(
        data = data.frame(
          x = c(15, 87 + 13 * sin(seq(0, pi / 2, length = 20)), 87 + 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
          y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5)
        )
      ) + 
      geom_path(
        data = data.frame(
          x = c(15, -87 - 13 * sin(seq(0, pi / 2, length = 20)), -87 - 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
          y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5)
        )
      ) + 
      
      ## Goal Lines
      geom_path(
        data = data.frame(
          x = c(89),
          y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))
        ), 
        color = 'red', 
        size = .5 * lines_mult
      ) + 
      geom_path(
        data = data.frame(
          x = c(-89), 
          y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))
        ), 
        color = 'red', 
        size = .5 * lines_mult
      ) +
      
      ## Nets
      geom_path(
        data = data.frame(
          x = c(90, 92, 92, 90), 
          y = c(-3, -3, 3, 3)
        ), 
        size = .5 * lines_mult
      ) + 
      geom_path(
        data = data.frame(
          x = c(-90, -92, -92, -90), 
          y = c(-3,-3, 3, 3)
        ), 
        size = .5 * lines_mult
      ) +
      
      ## Restricted Area
      geom_segment(
        aes(x = 89, y = -11, xend = 100, yend = -14), 
        color = 'red', 
        size = .5 * lines_mult
      ) + 
      geom_segment(
        aes(x = 89, y = 11, xend = 100, yend = 14), 
        color = 'red', 
        size = .5 * lines_mult
      ) + 
      geom_segment(
        aes(x = -89, y = -11, xend = -100, yend = -14), 
        color = 'red', 
        size = .5 * lines_mult
      ) + 
      geom_segment(
        aes(x = -89, y = 11, xend =-100, yend = 14), 
        color = 'red', 
        size = .5 * lines_mult
      ) +
      
      ## Red Line (Center Ice)
      geom_segment(
        aes(x = 0, y = -42.5, xend = 0, yend = 42.5), 
        color = 'red', 
        size = .75 * lines_mult
      ) +
      
      ## Blue Lines
      geom_segment(
        aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), 
        color = 'blue', 
        size = .75 * lines_mult
      ) + 
      geom_segment(
        aes(x = -25, y = -42.5, xend = -25,  yend = 42.5), 
        color = 'blue', 
        size = .75 * lines_mult
      ) +
      
      ## Crease
      geom_polygon(
        data = data.frame(
          x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
          y = c(-4, xseq, 4), 
          size = .5 * lines_mult
        ), 
        color = 'red', 
        fill = 'deepskyblue2', 
        size = .5 * lines_mult, 
        alpha = 0.75
      ) + 
      geom_polygon(
        data = data.frame(
          x = -1 * c(89, 83 + xseq^2 / 4^2 * 1.5, 89),
          y = c(-4, xseq, 4)
        ), 
        color = 'red', 
        fill = 'deepskyblue2', 
        size = .5 * lines_mult, 
        alpha = .75
      ) +
      ## Center Ice Circle
      geom_path(
        data = data.frame(x = 15 * sin(theta1)), 
        y = 15 * cos(theta1), 
        color = 'deepskyblue2', 
        size = .75 * lines_mult
      ) +
      ## Faceoff Dots
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = 20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = -20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = -20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = 20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = -69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = 69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = -69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = 69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) +
      
      ## Faceoff Circles
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 0.75, xend = 69 - 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                       yend = 22 + 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                       yend = 22 - 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                       yend = 22 + 0.75, xend = -69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                       yend = -22 + 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                       yend = 22 - 0.75, xend = -69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                       yend = -22 + 0.75, xend = -69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                       yend = -22 - 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                       yend = -22 - 0.75, xend = -69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 - dd, 
                       yend = 22 - 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 + dd, 
                       yend = 22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 + dd, 
                       yend = 22+17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 - dd, 
                       yend = 22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 - dd, 
                       yend = -22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 + dd, 
                       yend = -22 + 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 - dd, 
                       yend = -22 - 17, xend = 69 - dd), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 + dd, 
                       yend = -22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = -69 + dd, 
                       yend = -22 + 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = -69 - dd, 
                       yend = -22 - 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = -69 + dd, 
                       yend = -22 - 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = -69 - dd, 
                       yend = -22 + 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = -69 + dd, 
                       yend = 22 - 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = -69 - dd, 
                       yend = 22 - 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = -69 - dd, 
                       yend = 22 + 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = -69 + dd, 
                       yend = 22 + 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                       yend = 22 + 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                       yend = 22 + 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                       yend = 22 - 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                       yend = 22 - 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                       yend = -22 - 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                       yend = -22 - 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                       yend = -22 + 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                       yend = -22 + 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                  x = -69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                  x = -69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      theme_void()
    
  }
  
  ## Half NHL Rink
  else if (rink_type == "half") {
    
    rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
      ## Rink
      geom_path(
        data = data.frame(
          x = c(0, 87 + 13 * sin(seq(0, pi / 2, length = 20)), 87 + 13 * sin(seq(pi / 2, 0, length = 20)), 0), 
          y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5)
        )
      ) + 
      
      ## Goal Lines
      geom_path(
        data = data.frame(
          x = c(89),
          y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))
        ), 
        color = 'red', 
        size = .5 * lines_mult
      ) + 
      geom_path(
        data = data.frame(x = c(90, 92, 92, 90)), y = c(-3, -3, 3, 3), 
        size = .5 * lines_mult
      ) + 
      
      ## Restricted Area
      geom_segment(
        aes(x = 89, y = -11, xend = 100, yend = -14), 
        color = 'red', 
        size = .5 * lines_mult
      ) + 
      geom_segment(
        aes(x = 89, y = 11, xend = 100, yend = 14), 
        color = 'red', 
        size = .5 * lines_mult
      ) + 
      
      ## Red Line (Center Ice)
      geom_segment(
        aes(x = 0, y = -42.5, xend = 0, yend = 42.5), 
        color = 'red', 
        size = .75 * lines_mult
      ) +
      
      ## Blue Lines
      geom_segment(
        aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), 
        color = 'blue', 
        size = .75 * lines_mult
      ) + 
      
      ## Crease
      geom_polygon(
        data = data.frame(
          x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
          y = c(-4, xseq, 4), 
          size = .5 * lines_mult
        ), 
        color = 'red', 
        fill = 'deepskyblue2', 
        size = .5 * lines_mult, 
        alpha = 0.75
      ) + 
      
      ## Center Ice Circle
      geom_path(data = data.frame(x = abs(15 * sin(theta1))), 
                y = 15 * cos(theta1), color = 'deepskyblue2', 
                size = .75 * lines_mult) +
      
      ## Faceoff Dots
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                     x = 20 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                     x = 20 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                     x = 69 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                     x = 69 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      
      ## Faceoff Circles
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 0.75, xend = 69 - 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 - dd, 
                       yend = 22 - 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 + dd, 
                       yend = 22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 + dd, 
                       yend = 22+17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 - dd, 
                       yend = 22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 - dd, 
                       yend = -22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 + dd, 
                       yend = -22 + 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 - dd, 
                       yend = -22 - 17, xend = 69 - dd), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 + dd, 
                       yend = -22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      
      coord_flip(xlim = c(-1, 100), expand = TRUE) + 
      
      theme_void() + 
      theme(aspect.ratio = 1.165)
    
  }
  
  return(rink)
  
}


filename = "roughEvents.RData"
load(filename)

# Calculate percentage of events in each game with unknown skater strength  
bug_check = events %>%
  group_by(gameID) %>%
  mutate(bugR = sum(is.na(situationCode)) / n()) %>%
  ungroup() 

# Remove games with greater than 90% bugged situationCode, add running clock, redefine side
master_bug = bug_check %>%
  filter(bugR >= .9)
master_raw = bug_check %>%
  filter(bugR < .9) %>%
  mutate(homeTeamDefendingSide = NA, periodSec = seconds_calc(timeInPeriod), 
         gameSec = ifelse(number != 5 & gameType != 3, 
                          periodSec + 1200 * 3 + 300, 
                          periodSec + 1200 * (number - 1)),
         homeEvent = ifelse(homeID == eventOwnerTeamId, 1, 0),
         homeOffense = ifelse(homeEvent == 1 & zoneCode == 'O', sign(xCoord), NA)) %>%
  group_by(gameID, number) %>%
  fill(homeOffense, .direction = 'downup') %>%
  ungroup() %>%
  mutate(homeTeamDefendingSide = ifelse(homeOffense == -1, 'right', 'left')) %>%
  select(-homeOffense)
  



# Add skater numbers
{
  master_state = master_raw %>%
    mutate(situationCode = as.character(situationCode), sitReverse = stringi::stri_reverse(situationCode)) %>%
    mutate(homeEvent = ifelse(is.na(eventOwnerTeamId), NA, as.numeric(homeID == eventOwnerTeamId))) %>%
    mutate(situationCode = case_when(
      is.na(situationCode)           ~ '0000',
      str_length(situationCode) == 3 ~ paste('0', situationCode, sep = ''), 
      TRUE                           ~ situationCode
    )) %>%
    separate_wider_position(situationCode,
                            c(awayGoalie = 1, awaySkaters = 1,
                              homeSkaters = 1, homeGoalie = 1)) %>%
    mutate_at(c('awayGoalie', 'awaySkaters', 'homeSkaters', 'homeGoalie'), as.numeric)
  
  
  # master_state = master_raw %>%
  #   mutate(situationCode = as.character(situationCode), sitReverse = stringi::stri_reverse(situationCode)) %>%
  #   mutate(situationCode = ifelse(homeTeamDefendingSide != 'right', situationCode, sitReverse),
  #          homeEvent = ifelse(is.na(eventOwnerTeamId), NA, as.numeric(homeID == eventOwnerTeamId))) %>%
  #   select(-sitReverse) %>%
  #   mutate(situationCode = case_when(
  #     is.na(situationCode)           ~ '0000',
  #     str_length(situationCode) == 3 ~ paste('0', situationCode, sep = ''), 
  #     TRUE                           ~ situationCode
  #   )) %>%
  #   separate_wider_position(situationCode,
  #                           c(homeGoalie = 1, homeSkaters = 1,
  #                             awaySkaters = 1, awayGoalie = 1)) %>%
  #   mutate_at(c('homeGoalie', 'homeSkaters', 'awaySkaters', 'awayGoalie'), as.numeric)
  
}


# Add Previous events
{
  master_full   = master_state %>%
    mutate(
      # Add Seconds
      timeSec       = seconds_calc(timeInPeriod),
      # Previous event type
      prev_event    = lag(typeDescKey, n = 1),
      prev_event_2  = lag(typeDescKey, n = 2),
      # Time to previous event
      prev_time     = timeSec - lag(timeSec, n = 1),
      prev_time_2   = timeSec - lag(timeSec, n = 2),
      # x distance to previous event
      prev_dist_x   = xCoord - lag(xCoord, n = 1),
      prev_dist_x_2 = lag(xCoord, n = 1) - lag(xCoord, n = 2), 
      # y distance to previous event
      prev_dist_y   = yCoord - lag(yCoord, n = 1),
      prev_dist_y_2 = lag(yCoord, n = 1) - lag(yCoord, n = 2),
      # Distance to previous event
      prev_distance = sqrt((xCoord - lag(xCoord, n = 1))^2 + (yCoord - lag(yCoord, n = 1))^2),
      prev_distance_2 = sqrt((lag(xCoord, n = 1) - lag(xCoord, n = 2))^2 + (lag(yCoord, n = 1)-lag(yCoord, n = 2))^2),
      # previous team event
      prev_home_event = lag(homeEvent, n = 1),
      prev_home_event_2 = lag(homeEvent, n = 2),
      # Previous event zone
      prev_zone     = ifelse(
        lag(eventOwnerTeamId, 1) != eventOwnerTeamId & lag(zoneCode, 1) %in% c('O','D'),
        ifelse(lag(zoneCode, 1) == 'O', 'D', 'O'), 
        lag(zoneCode, 1)
        ),
      prev_zone_2   = ifelse(
        lag(eventOwnerTeamId, 2) != eventOwnerTeamId & lag(zoneCode, 1) %in% c('O','D'),
        ifelse(lag(zoneCode, 2) == 'O', 'D', 'O'), 
        lag(zoneCode, 2)
      ),
      homeScore = ifelse(typeDescKey == 'period-start' & number == 1, 0, homeScore), 
      awayScore = ifelse(typeDescKey == 'period-start' & number == 1, 0, awayScore), 
      goalies = homeGoalie + awayGoalie,
      skaters = homeSkaters + awaySkaters,
      skaterDiff = ifelse(homeEvent == 1, (homeSkaters - awaySkaters), (awaySkaters - homeSkaters)),
      netEmpty = case_when(
        homeEvent == 1 & homeGoalie == 0 ~ 1,
        homeEvent == 0 & awayGoalie == 0 ~ 1,
        TRUE ~ 0
      ),
      str_state = ifelse(
        goalies == 2, 
        case_when(
          skaterDiff == 0 ~ 'EV',
          skaterDiff < 0 ~ 'PK',
          skaterDiff > 0 ~ 'PP'
        ),
        case_when(
          skaters == 1 ~ 'PS',
          netEmpty == 1 ~ 'EN',
          goalies + skaters == 0 ~ 'NA',
          netEmpty == 0 ~ 'NG'
        )
      )
    ) %>%
    fill(homeScore) %>%
    fill(awayScore)
}

# Limit to shots and add shot info
{
  shots = master_full %>%
    mutate(shotOutcome = case_when(
      typeDescKey == 'shot-on-goal' ~ 'save',
      typeDescKey == 'blocked-shot' ~ 'block',
      typeDescKey == 'missed-shot'  ~ 'miss',
      typeDescKey == 'goal'         ~ 'goal',
      typeDescKey == 'failed-shot-attempt' ~ 'fail'
    )) %>%
    filter(grepl('shot', typeDescKey, ignore.case = TRUE) | typeDescKey == 'goal') %>%
    mutate(shotDistance = sqrt((89 - abs(xCoord))**2 + yCoord**2), 
           shotAngle = abs(atan(yCoord / (89 - abs(xCoord))) * 180 / pi),
           isGoal = ifelse(shotOutcome == 'goal', 1, 0))
    
  
}

PP = shots %>% 
  filter(str_state == 'PP') %>%
  select(typeDescKey, homeID, awayID, eventOwnerTeamId, homeEvent, homeGoalie, homeSkaters, 
         awaySkaters, awayGoalie, homeTeamDefendingSide, goalies, skaters, skaterDiff, shotOutcome)

str_analysis = shots %>%
  group_by(str_state) %>%
  summarise(shperc = sum(isGoal) / n())
# Check data
goals = shots %>%
  filter(isGoal == 1)

ggplot(goals, aes(x = str_state)) + geom_bar()
ggplot(str_analysis, aes(x = str_state, y = shperc)) + geom_col()

filename = "shots.RData"
save(shots, file = filename)

fun.draw_rink() + 
  stat_density2d_filled(data = shots, aes(x = xCoord, y = yCoord, alpha = .5))





