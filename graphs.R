library(dplyr)
library(ggplot2)
library(stringr)

`%notin%` = function(x,y) !(x %in% y)
hearings25 <- read.csv(paste(getwd(), "/first25.csv", sep = ""))

#### cleaning up the punctiation split output #### 
hearings25$worddf[hearings25$worddf == "s"] <- "is"
hearings25$worddf[hearings25$worddf == "re"] <- "are"
hearings25$worddf[hearings25$worddf == "m"] <- "am"
hearings25$worddf[hearings25$worddf == "don"] <- "do"
hearings25$worddf[hearings25$worddf == "t"] <- "not"
hearings25$worddf[hearings25$worddf == "ve"] <- "have"
hearings25$worddf[hearings25$worddf == "ll"] <- "will"

hearings25$worddf <- str_trim(hearings25$worddf)
hearings25$speaker_titledf <- str_trim(hearings25$speaker_titledf)
hearings25$speaker_lastnamedf <- str_trim(hearings25$speaker_lastnamedf)

#### playing with summary ####
summary(hearings25$worddf)
summary(hearings25$speaker_lastnamedf)
# order(summary(hearings25$speaker_titledf))

summary(hearings25$speaker_titledf)[c(order(summary(hearings25$speaker_titledf)))]
# hearings25[,1:13]


##### making gender variables ####


hearings25$speaker_titledf <- as.character(hearings25$speaker_titledf)

hearings25$gender <- NA

hearings25[hearings25$speaker_titledf == "Mrs.", ][, "gender"] <- "Female"
hearings25[hearings25$speaker_titledf == "Ms.", ][, "gender"] <- "Female"
hearings25[hearings25$speaker_titledf == "Mr.", ][, "gender"] <- "Male"

hearings25$gender <- as.factor(hearings25$gender)
summary(hearings25$gender)

hearings25$speaker_titledf <- as.factor(hearings25$speaker_titledf)
summary(hearings25$speaker_titledf)

hearings25$worddf



hearings25$worddf <- tolower(hearings25$worddf)

hearings25 <- hearings25 %>% filter(worddf != "") 
hearings25$worddf <- str_extract(hearings25$worddf, "[aA-zZ]+")
hearings25$worddf <- gsub("[[:punct:]]", "", hearings25$worddf)
hearings25 <- hearings25 %>% filter(worddf != "")
hearings25 <- hearings25 %>% filter(worddf %notin% c("the", "to", "and", "is", "that", "of",
                                   "a", "have", "it", "in", "are", "do", "for", "was",
                                   "this", "be", "on", "with", "so", "what", "about",
                                   "at", "can", "would", "when"))
check <- hearings25 %>% group_by(worddf) %>% summarize(n = n()) 


