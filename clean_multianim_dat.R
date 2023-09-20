###Cleaning Multi-Animal Data Code 
#Inspired by workflow for single animal from DLCAnalyzer


#Read in some 5 animal id data
ex1 <- read.csv("https://github.com/seesesyd/OpenFieldPilot/blob/1efab7477da763f6d61bbdfa02d1501c91e31ebd/data/D2V2RDLC_resnet50_balJun1shuffle1_100000_el.csv")
ex2 <- read.csv("https://github.com/seesesyd/OpenFieldPilot/blob/aa66c4b9f422622f340726146e5234214bd494cf/data/D3V1ADLC_resnet50_balJun1shuffle1_100000_el.csv")
#These links aren't working, ask James what could be wrong. Brute force for now: 
ex3 <- read.csv("D1V1LDLC_resnet50_balJun1shuffle1_100000_el.csv") #long
ex4 <- read.csv("D3V1ADLC_resnet50_balJun1shuffle1_100000_el.csv") #short


#Filter out missing rows
ex3[ex3 == ""] <- NA
ex4[ex4 == ""] <- NA

subsetex3 <- ex3[,2:163]
subsetex4 <- ex4[,2:163]

check_non_na <- function(row) {
  any(!is.na(row))
}

result3 <- apply(subsetex3, 1, check_non_na)
result4 <- apply(subsetex4, 1, check_non_na)

cleaned_ex3 <- ex3[result3 != FALSE, ]
cleaned_ex4 <- ex4[result4 != FALSE, ]

likelihood_columns3 <- grep("likelihood", cleaned_ex3[3, ])
likelihood_columns4 <- grep("likelihood", cleaned_ex4[3, ])

for (col in likelihood_columns3) {
  cleaned_ex3[, col] <- ifelse(is.na(cleaned_ex3[, col]), 0, cleaned_ex3[, col])
}

for (col in likelihood_columns4) {
  cleaned_ex4[, col] <- ifelse(is.na(cleaned_ex4[, col]), 0, cleaned_ex4[, col])
}

#last formatting thing, I want to pivot this dataset longer to have "individual" and "bodypart" as specific columns
#changing the colnames to the values in row 1 
#colnames(results3) <- results3[1,]
#results3 <- results3[-1,]

#This doesn't work, only seems to be recognizing unique column values?

#creating unique column values for individual-bodypart-x/y/likelihood value
selected_rows3 <- cleaned_ex3[1:3,]
selected_rows4 <- cleaned_ex4[1:3,]

colnames(cleaned_ex3) <- paste(selected_rows3[1,], selected_rows3[2,], selected_rows3[3,], sep = "-")
colnames(cleaned_ex4) <- paste(selected_rows4[1,], selected_rows4[2,], selected_rows4[3,], sep = "-")

maxrow3 <- nrow(cleaned_ex3)
maxrow4 <- nrow(cleaned_ex4)

testing3 <- cleaned_ex3[4:maxrow3,]
testing4 <- cleaned_ex4[4:maxrow4,]

selected_rows3longer <- testing3 %>%
  pivot_longer(cols = 2:163, names_to="individual") #very memory intensive

colnames(selected_rows3longer) <- c("frame", "individual", "value")

selected_rows4longer <- testing4 %>%
  pivot_longer(cols = 2:163, names_to="individual")

colnames(selected_rows4longer) <- c("frame", "individual", "value")

split_text3 <- strsplit(selected_rows3longer$individual, "-") #also very memory intensive
split_text4 <- strsplit(selected_rows4longer$individual, "-")

selected_rows3longer$id <- sapply(split_text3, function(x) ifelse(length(x) > 1, x[[1]], NA))
selected_rows3longer$bodypart <- sapply(split_text3, function(x) ifelse(length(x) > 1, x[[2]], NA))
selected_rows3longer$coordlik <- sapply(split_text3, function(x) ifelse(length(x) > 1, x[[3]], NA))

selected_rows4longer$id <- sapply(split_text4, function(x) ifelse(length(x) > 1, x[[1]], NA))
selected_rows4longer$bodypart <- sapply(split_text4, function(x) ifelse(length(x) > 1, x[[2]], NA))
selected_rows4longer$coordlik <- sapply(split_text4, function(x) ifelse(length(x) > 1, x[[3]], NA))

formatted3 <- selected_rows3longer[,-2]

formatted3 <- formatted3 %>%
  pivot_wider(values_from = value, names_from = coordlik) #also very memory intensive

formatted4 <- selected_rows4longer[,-2]

formatted4 <- formatted4 %>%
  pivot_wider(values_from = value, names_from = coordlik)

#Filter out likelihoods below 95%
cleaned_ex3[4:maxrow3,] <- as.numeric(unlist(cleaned_ex3[4:maxrow3,])) #takes a very long time, is there a way to optimize?
cleaned_ex4[4:maxrow4,] <- as.numeric(unlist(cleaned_ex4[4:maxrow4,])) #takes a very long time, is there a way to optimize?

results3 <- cleaned_ex3[(cleaned_ex3[, likelihood_columns3] > 0.95), ]
#created a bunch of NA rows (like hundreds of thousands?), reduce the dataset
results3 <- results3[1:51884,]
results4 <- cleaned_ex4[(cleaned_ex4[, likelihood_columns4] > 0.95), ]
#created a bunch of NA rows (like thousands?), reduce the dataset
results4 <- results4[1:4826,]


#Cropping reads to be within the OF (OF is 24 inches by 24 inches or 60.96 by 60.96 cms)
#trying to get an idea of what the dimensions probably are based on point predictions
ggplot(results3, aes(x = )) #fill this in with points once dataset is pivoting longer

#get an average of recorded cage location and see if it lines up with the plot
boxsumupL <- results3 %>%
  filter(individual = "box" & loc = "lt") %>%
  summarize(avgx = mean(x),
            avgy = mean(y))

boxsumupR <- results3 %>%
  filter(individual = "box" & loc = "rt") %>%
  summarize(avgx = mean(x),
            avgy = mean(y))

boxsumlowL <- results3 %>%
  filter(individual = "box" & loc = "lb") %>%
  summarize(avgx = mean(x),
            avgy = mean(y))

boxsumlowR <- results3 %>%
  filter(individual = "box" & loc = "rb") %>%
  summarize(avgx = mean(x),
            avgy = mean(y))

#find pixel distance from box corners
distlowx <- boxsumlowR$avgx - boxsumlowL$avgx
distly <- boxsumupL$avgy - boxsumlowL$avgy

#60.96=(pixeldist)*(x)
#converting function is then convertdata = 60.96/distinpixel

pixelconvx <- 60.96/distlowx
pixelconvy <- 60.96/distly







