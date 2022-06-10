ind_line_fade <- function(xy.df){
  library(ggplot2)
  library(gganimate)
  library(tidyverse)
  
  unique(xy.df$id)
  
  n <- max(xy.df$Frame)
  
  newdf <- xy.df %>%
    uncount(n, .id = "newframe") %>%
    filter(Frame <= newframe) %>%
    arrange(newframe, Frame) %>%
    group_by(newframe, id) %>%
    mutate(x_lag = lag(x), 
           y_lag = lag(y),
           tail = last(Frame) - Frame,
           # Make the points solid for 1 frame then alpha 0
           point_alpha = if_else(tail == 0, 1, 0),
           # Make the lines fade out over 5 frames
           segment_alpha = pmax(0, (5-tail)/5)) %>%
    ungroup()
  
  tail(newdf)
  
  ggplot(newdf, 
         aes(x = y, y = x, xend = y_lag, yend = x_lag, group = Frame, color = id)) +
    geom_segment(aes(alpha = segment_alpha)) +
    geom_point(aes(alpha = point_alpha)) +
    scale_alpha(range = c(0,1)) +
    guides(alpha = F) +
    transition_manual(newframe) +
    theme_void() +
    scale_color_manual(values = c("red","blue","green", "orange", "black", "purple")) 
  
}

ind_line_fade(xy.df)