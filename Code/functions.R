## this is a script containing code for noise data that has been turned into functions
library(ggplot2)

df1 <- read.csv("data/Cohort1_hab3_1_LocationOutput.csv")
df2 <- read.csv("data/Cohort1_hab3_2_LocationOutput.csv")
df3 <- read.csv("data/Cohort1_hab3_3_LocationOutput.csv")
df4 <- read.csv("data/Cohort1_hab3_4_LocationOutput.csv")
df6 <- read.csv("data/Cohort1_hab3_6_LocationOutput.csv")
df7 <- read.csv("data/Cohort1_hab3_7_LocationOutput.csv")

##### 1 - GET COORDINATES #####
get_coords_habit <- function(df, secs=300, fr=30, burnin = 0){
  
  library(tidyverse)
  library(stringi)
  library(jsonlite)
  
  nframes <- (secs * fr) + burnin
  
  dx.ROI <- df %>% select(ROI_coordinates)
  str1 <- (dx.ROI[1,])
  str1<-stri_sub(str1,15, -2) 
  x1 <- gsub("'", '"', str1) 
  dx.ROI <- data.frame( jsonlite::fromJSON(x1)) 
  
  dx.flip <- data.frame(t(dx.ROI[]))
  
  box.range <- data.frame(
    left = mean(dx.flip[c(1,4),1]),
    top = mean(dx.flip[c(5,6),1]),
    right = mean(dx.flip[c(2,3),1]),
    bottom = mean(dx.flip[c(7,8),1]))
  
  full <- cbind(df,box.range) %>%   select(-ROI_coordinates)
  
  full %>%
    mutate(plotX = ifelse(X < left, left, ifelse(X > right, right, X))) %>%
    mutate(plotY = ifelse(Y < top, top, ifelse(Y > bottom, bottom, Y))) -> full
  
  full %>% select(Frame,X,Y,plotX,plotY) -> full
  
  full$rescaleX <- ((full$plotX - box.range$left) / (box.range$right - box.range$left))*1000
  full$rescaleY <- ((full$plotY - box.range$top) / (box.range$bottom - box.range$top))*1000
  
  full <- full %>% filter (Frame <= nframes)
  full <- full[(1+burnin):(nrow(full)+burnin),]
  
  return(list('box' = box.range, 'xy' = full))
  
}

xy1 <- get_coords_habit(df1)
xy2 <- get_coords_habit(df2)
xy3 <- get_coords_habit(df3)
xy4 <- get_coords_habit(df4)
xy6 <- get_coords_habit(df6)
xy7 <- get_coords_habit(df7)

xy.df <- 
  rbind(
    xy1$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=1),
    xy2$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=2),
    xy3$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=3),
    xy4$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=4),
    xy6$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=6),
    xy7$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=7)
  )
head(xy.df)
tail(xy.df)

##### 2 - GET INDIVIDUAL PATH #####

get_ind_path <- function(df, ID, FRAME_MIN, FRAME_MAX){
  df %>% 
    filter(id==ID) %>%
    filter(Frame > FRAME_MIN | Frame < FRAME_MAX) %>%
    ggplot(aes(x, y, label=id)) +
    geom_point(size=0.5) +
    geom_segment(aes(
      xend=c(tail(x, n=-1), NA), 
      yend=c(tail(y, n=-1), NA)
    )
    ) +
    theme_void()
}

get_ind_path(xy.df, 1, FRAME_MIN = 500, FRAME_MAX = 1500)
get_ind_path(xy.df, 2, FRAME_MIN = 500, FRAME_MAX = 1500)
get_ind_path(xy.df, 3, FRAME_MIN = 500, FRAME_MAX = 1500)
get_ind_path(xy.df, 4, FRAME_MIN = 500, FRAME_MAX = 1500)
get_ind_path(xy.df, 6, FRAME_MIN = 500, FRAME_MAX = 1500)
get_ind_path(xy.df, 7, FRAME_MIN = 500, FRAME_MAX = 1500)

xy.df %>% 
  ggplot(aes(x, y, color = factor(id))) +
  geom_point(size=0.5) +
  geom_segment(aes(
    xend=c(tail(x, n=-1), NA), 
    yend=c(tail(y, n=-1), NA)
  )
  ) + scale_color_manual(values = c("#FFD833", "#FFBA33", "#FF5733", "#C70039", "#900C3F", "#581845")) +
  theme_void()

library(gganimate)
library(gifski)
library(transformr)

get_ind_animate <- function(df, ID, FRAME_MIN, FRAME_MAX){
  z <- df %>%
    filter(id == ID | Frame > FRAME_MIN | Frame < FRAME_MAX) 
  
  ggplot()+
    geom_point(data = z, aes(x,y))+
    transition_time(Frame)+
    shadow_wake(wake_length = 0.3, alpha = 0.7)+
    theme_classic() + theme(legend.position = "none")
}

get_ind_animate(xy.df, 1, 500, 1500)

##### 3 - PLOT ANIMATED CONVEX HULL #####
plot_hull_animate <- function(xy.df, FRAME_MIN, FRAME_MAX){
  xy.dfsim <- subset(xy.df[,2:3])
  out <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    verts <- chull(xy.dfsim)
    poly <- xy.dfsim[verts,]
    
    pts <- as.data.frame(xy.dfsim)
    
    poly <- rbind(poly, poly[1, ])
    poly <- as.data.frame(poly)
    n <- nrow(poly)
    x <- rev(poly[, 1])
    y <- rev(poly[, 2])
    w <- 1:(n-1)
    
    poly$A <- sum(c(x[w] * y[w+1] - x[w+1] * y[w])) / 2
    
    poly$Frame <- i
    
    out[[i]] <- poly
    
  }
  
  out <- data.table::rbindlist(out)
  
  points <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    crk <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crk <- as.data.frame(crk)
    
    crk$id <- xy.dfrandtime$id
    
    crk$Frame <- i
    
    points[[i]] <- crk
  }
  
  points <- data.table::rbindlist(points)
  
  ggplot()+
    geom_point(data = points, aes(x,y, color = factor(id)))+
    geom_polygon(
      aes(x,y),
      color="seagreen",
      fill="lightseagreen",
      alpha = 0.3, data = out,
      show.legend = FALSE)+
    scale_y_continuous(limits = c(0, 1000)) +
    scale_x_continuous(limits = c(0, 1000)) +
    scale_color_manual(values = c("#FFD833", "#FFBA33", "#FF5733", "#C70039", "#900C3F", "#581845")) +
    transition_time(Frame)+
    theme_classic()
}

plot_hull_animate(xy.df, 1, 50)

##### 4 - PLOT ANIMATED CONVEX HULL WITH CENTROID #####
plot_hull_cent_animate <- function(xy.df, FRAME_MIN, FRAME_MAX){
  xy.dfsim <- subset(xy.df[,2:3])
  out <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    verts <- chull(xy.dfsim)
    poly <- xy.dfsim[verts,]
    
    pts <- as.data.frame(xy.dfsim)
    
    poly <- rbind(poly, poly[1, ])
    poly <- as.data.frame(poly)
    n <- nrow(poly)
    x <- rev(poly[, 1])
    y <- rev(poly[, 2])
    w <- 1:(n-1)
    
    poly$A <- sum(c(x[w] * y[w+1] - x[w+1] * y[w])) / 2
    
    poly$Frame <- i
    
    out[[i]] <- poly
    
  }
  
  out <- data.table::rbindlist(out)
  
  points <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    crk <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crk <- as.data.frame(crk)
    
    crk$id <- xy.dfrandtime$id
    
    crk$Frame <- i
    
    points[[i]] <- crk
  }
  
  points <- data.table::rbindlist(points)
  
  cents <- NULL
  for (i in 1:50) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:4])
    
    verts <- chull(xy.dfsim)
    poly <- xy.dfsim[verts,]
    
    poly <- rbind(poly, poly[1, ])
    n <- nrow(poly)
    x <- rev(poly[, 1])
    y <- rev(poly[, 2])
    w <- 1:(n-1)
    
    xy.dfsim$A <- sum(c(x[w] * y[w+1] - x[w+1] * y[w])) / 2
    xy.dfsim$Cx <- sum((x[w] + x[w+1]) * (x[w] * y[w+1] - x[w+1] * y[w])) / (6 * xy.dfsim$A)
    xy.dfsim$Cy <- sum((y[w] + y[w+1]) * (x[w] * y[w+1] - x[w+1] * y[w])) / (6 * xy.dfsim$A)
  
    crj <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crj <- as.data.frame(crj)
    
    crj$Frame <- i
    
    cents[[i]] <- crj
  }
  
  cents <- data.table::rbindlist(cents)
  
  ggplot()+
    geom_point(data = points, aes(x,y, color = factor(id)))+
    geom_polygon(
      aes(x,y),
      color="seagreen",
      fill="lightseagreen",
      alpha = 0.3, data = out,
      show.legend = FALSE)+
    geom_point(data = cents, aes(Cx, Cy), shape = "asterisk", colour = "red", size = 3.5)+
    scale_y_continuous(limits = c(0, 1000)) +
    scale_x_continuous(limits = c(0, 1000)) +
    scale_color_manual(values = c("#FFD833", "#FFBA33", "#FF5733", "#C70039", "#900C3F", "#581845")) +
    transition_time(Frame)+
    theme_classic()
}
plot_hull_cent_animate(xy.df, 50, 100)

##### 5 - PLOT INDIVIDUAL LINES TO CENTROID
cent_dist_animate <- function(xy.df, FRAME_MIN, FRAME_MAX){
  xy.dfsim <- subset(xy.df[,2:3])
  out <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    verts <- chull(xy.dfsim)
    poly <- xy.dfsim[verts,]
    
    pts <- as.data.frame(xy.dfsim)
    
    poly <- rbind(poly, poly[1, ])
    poly <- as.data.frame(poly)
    n <- nrow(poly)
    x <- rev(poly[, 1])
    y <- rev(poly[, 2])
    w <- 1:(n-1)
    
    poly$A <- sum(c(x[w] * y[w+1] - x[w+1] * y[w])) / 2
    
    poly$Frame <- i
    
    out[[i]] <- poly
    
  }
  
  out <- data.table::rbindlist(out)
  
  points <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    crk <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crk <- as.data.frame(crk)
    
    crk$id <- xy.dfrandtime$id
    
    crk$Frame <- i
    
    points[[i]] <- crk
  }
  
  points <- data.table::rbindlist(points)
  
  cents <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:4])
    
    verts <- chull(xy.dfsim)
    poly <- xy.dfsim[verts,]
    
    poly <- rbind(poly, poly[1, ])
    n <- nrow(poly)
    x <- rev(poly[, 1])
    y <- rev(poly[, 2])
    w <- 1:(n-1)
    
    xy.dfsim$A <- sum(c(x[w] * y[w+1] - x[w+1] * y[w])) / 2
    xy.dfsim$Cx <- sum((x[w] + x[w+1]) * (x[w] * y[w+1] - x[w+1] * y[w])) / (6 * xy.dfsim$A)
    xy.dfsim$Cy <- sum((y[w] + y[w+1]) * (x[w] * y[w+1] - x[w+1] * y[w])) / (6 * xy.dfsim$A)
    
    crj <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crj <- as.data.frame(crj)
    
    crj$Frame <- i
    
    cents[[i]] <- crj
  }
  
  cents <- data.table::rbindlist(cents)
  
  dists <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    centsprogtime <- cents[cents$Frame == i,]
    centsprogtime$distance = sqrt( abs (centsprogtime$x - centsprogtime$Cx )^2 + abs( centsprogtime$y - centsprogtime$Cy )^2 )
    crh <- centsprogtime
    crh <- as.data.frame(crh)
    crh$Frame <- i
    dists[[i]] <- crh
  }
  dists <- data.table::rbindlist(dists)
  
  ggplot()+
    geom_point(data = points, aes(x,y))+
    geom_point(data = cents, aes(Cx, Cy), shape = "asterisk", colour = "red", size = 3.5)+
    geom_segment(data = dists, aes(
      x = x,
      y = y, 
      xend = Cx, 
      yend = Cy
    )
    ) +
    transition_time(Frame)+
    theme_classic()
}

cent_dist_animate(xy.df, 50, 150)

##### 6 -  PLOT INDIVIDUAL DISTANCES FROM CENTROID #####
cent_dist <- function(xy.df, FRAME_MIN, FRAME_MAX){
  xy.dfsim <- subset(xy.df[,2:3])
  out <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    verts <- chull(xy.dfsim)
    poly <- xy.dfsim[verts,]
    
    pts <- as.data.frame(xy.dfsim)
    
    poly <- rbind(poly, poly[1, ])
    poly <- as.data.frame(poly)
    n <- nrow(poly)
    x <- rev(poly[, 1])
    y <- rev(poly[, 2])
    w <- 1:(n-1)
    
    poly$A <- sum(c(x[w] * y[w+1] - x[w+1] * y[w])) / 2
    
    poly$Frame <- i
    
    out[[i]] <- poly
    
  }
  
  out <- data.table::rbindlist(out)
  
  points <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    crk <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crk <- as.data.frame(crk)
    
    crk$id <- xy.dfrandtime$id
    
    crk$Frame <- i
    
    points[[i]] <- crk
  }
  
  points <- data.table::rbindlist(points)
  
  cents <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:4])
    
    verts <- chull(xy.dfsim)
    poly <- xy.dfsim[verts,]
    
    poly <- rbind(poly, poly[1, ])
    n <- nrow(poly)
    x <- rev(poly[, 1])
    y <- rev(poly[, 2])
    w <- 1:(n-1)
    
    xy.dfsim$A <- sum(c(x[w] * y[w+1] - x[w+1] * y[w])) / 2
    xy.dfsim$Cx <- sum((x[w] + x[w+1]) * (x[w] * y[w+1] - x[w+1] * y[w])) / (6 * xy.dfsim$A)
    xy.dfsim$Cy <- sum((y[w] + y[w+1]) * (x[w] * y[w+1] - x[w+1] * y[w])) / (6 * xy.dfsim$A)
    
    crj <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crj <- as.data.frame(crj)
    
    crj$Frame <- i
    
    cents[[i]] <- crj
  }
  
  cents <- data.table::rbindlist(cents)
  
  dists <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    centsprogtime <- cents[cents$Frame == i,]
    centsprogtime$distance = sqrt( abs (centsprogtime$x - centsprogtime$Cx )^2 + abs( centsprogtime$y - centsprogtime$Cy )^2 )
    crh <- centsprogtime
    crh <- as.data.frame(crh)
    crh$Frame <- i
    dists[[i]] <- crh
  }
  dists <- data.table::rbindlist(dists)
  
  ggplot()+
    geom_point(data = dists, aes(Frame,distance, color = factor(id)))+
    transition_time(Frame)+
    scale_color_manual(values = c("#FFD833", "#FFBA33", "#FF5733", "#C70039", "#900C3F", "#581845")) +
    shadow_mark()+
    theme_classic()
}

#Example of it running
cent_dist(xy.df, 50, 150)


##### 7 - PLOT DYADIC LINES OVER TIME #####
plot_dyad_animate <- function(xy.df, ID1, ID2, FRAME_MIN, FRAME_MAX){
  library(gganimate)
  library(gifski)
  library(transformr)
  library(tidyverse)
  library(ggplot2)
  
  points <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    crk <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crk <- as.data.frame(crk)
    
    crk$id <- xy.dfrandtime$id
    
    crk$Frame <- i
    
    points[[i]] <- crk
  }
  
  points <- data.table::rbindlist(points)
  
  dists2 <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    pointsprogtime <- points[points$Frame == i,]
    
    pointsprogtime$Ax <- pointsprogtime$x[ pointsprogtime$id == ID1 ]
    pointsprogtime$Bx <- pointsprogtime$x[ pointsprogtime$id == ID2 ]
    pointsprogtime$Ay <- pointsprogtime$y[ pointsprogtime$id == ID1 ]
    pointsprogtime$By <- pointsprogtime$y[ pointsprogtime$id == ID2 ]
    
    pointsprogtime$distance = sqrt( abs (pointsprogtime$Ax - pointsprogtime$Bx )^2 + abs( pointsprogtime$Ay - pointsprogtime$By )^2 )
    
    cri <- pointsprogtime
    
    cri <- as.data.frame(cri)
    
    cri$Frame <- i
    
    dists2[[i]] <- cri
  }
  
  dists2 <- data.table::rbindlist(dists2)
  
  ggplot()+
    geom_point(data = points, aes(x,y, color = factor(id)))+ geom_text(data = points, aes(x, y, label=id),hjust=1, vjust=1) +
    geom_segment(data = dists2, aes(
      x = Ax,
      y = Ay, 
      xend = Bx, 
      yend = By, 
      color = "black"
    )
    ) +
    scale_color_manual(values = c("#FFD833", "#FFBA33", "#FF5733", "#C70039", "#900C3F", "#581845", "black")) +
    transition_time(Frame)+
    theme_classic()
  
}

#Testing
plot_dyad_animate(xy.df, 1, 6, 50, 150)
 
 
##### 8 - PLOT DYADIC DISTANCES OVER TIME #####
dyad_dist <- function(xy.df, ID1, ID2, FRAME_MIN, FRAME_MAX){
  library(gganimate)
  library(gifski)
  library(transformr)
  library(tidyverse)
  library(ggplot2)
  
  points <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    xy.dfrandtime <- xy.df[xy.df$Frame == i,]
    xy.dfsim <- subset(xy.dfrandtime[,2:3])
    
    crk <- xy.dfsim
    
    pts <- as.data.frame(xy.dfsim)
    
    crk <- as.data.frame(crk)
    
    crk$id <- xy.dfrandtime$id
    
    crk$Frame <- i
    
    points[[i]] <- crk
  }
  
  points <- data.table::rbindlist(points)
  
  dists2 <- NULL
  for (i in FRAME_MIN:FRAME_MAX) {
    pointsprogtime <- points[points$Frame == i,]
    
    pointsprogtime$Ax <- pointsprogtime$x[ pointsprogtime$id == ID1 ]
    pointsprogtime$Bx <- pointsprogtime$x[ pointsprogtime$id == ID2 ]
    pointsprogtime$Ay <- pointsprogtime$y[ pointsprogtime$id == ID1 ]
    pointsprogtime$By <- pointsprogtime$y[ pointsprogtime$id == ID2 ]
    
    pointsprogtime$distance = sqrt( abs (pointsprogtime$Ax - pointsprogtime$Bx )^2 + abs( pointsprogtime$Ay - pointsprogtime$By )^2 )
    
    cri <- pointsprogtime
    
    cri <- as.data.frame(cri)
    
    cri$Frame <- i
    
    dists2[[i]] <- cri
  }
  
  dists2 <- data.table::rbindlist(dists2)
  
  ggplot()+
    geom_point(data = dists2, aes(Frame,distance))+
    transition_time(Frame)+
    shadow_mark()+
    theme_classic()
  
}

#testing
dyad_dist(xy.df, 1, 6, 50, 150)
