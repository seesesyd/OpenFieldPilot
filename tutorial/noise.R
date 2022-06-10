library(tidyverse)

df1 <- read.csv("data/Cohort1_hab3_1_LocationOutput.csv")
df2 <- read.csv("data/Cohort1_hab3_2_LocationOutput.csv")
df3 <- read.csv("data/Cohort1_hab3_3_LocationOutput.csv")
df4 <- read.csv("data/Cohort1_hab3_4_LocationOutput.csv")
df6 <- read.csv("data/Cohort1_hab3_6_LocationOutput.csv")
df7 <- read.csv("data/Cohort1_hab3_7_LocationOutput.csv")

###The function we use to scale our data to the box 
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