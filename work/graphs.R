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
summary(hearings25$speaker_titledf)[c(order(summary(hearings25$speaker_titledf)))]
# hearings25$worddf


### MORE CLEANING UP - ZIPF LAW AND NUMBERS + PUNCTUATION ### 
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



### MAKING COMMON TIMELINE  and counts ####
# line_orig <- unique(total_words$linedf)
# document_orig <- unique(total_words$datedf)
data <- as.Date(hearings25$datedf)
summary(data)
hearings25$count <- 1
hearings25$documentdf <- as.factor(hearings25$documentdf)

total_words <- hearings25 %>% 
  filter(!is.na(datedf)) %>% 
  arrange(datedf) %>% 
  group_by(chamberdf) %>% 
  mutate(words_chamber = cumsum(count))

total_words <- total_words %>% 
  ungroup %>% 
  group_by(committeedf, datedf, documentdf) %>% 
  mutate(documents_chamber = cumsum(count)) 

total <- total_words %>% arrange(datedf) %>% 
  select(datedf, namedf, linedf, count)
total <- unique(total)
total <- total %>% ungroup %>%  arrange(datedf, linedf) %>% 
  mutate(time = cumsum(count)) 

total_words <- left_join(total_words, total)

total_words <- total_words %>% ungroup %>%  arrange(time, indexdf) %>% 
  group_by(sessiondf) %>% 
  mutate(session_words = cumsum(count)) 

total_words <- total_words %>% ungroup %>%  arrange(time, indexdf) %>% 
  group_by(sessiondf) %>% 
  mutate(session_words = cumsum(count)) 

total_words <- total_words %>% ungroup %>%  arrange(time, indexdf) %>% 
  group_by(committeedf) %>% 
  mutate(committee_words = cumsum(count)) 

total_words <- total_words %>% ungroup %>%  arrange(time, indexdf) %>% 
  group_by(namedf) %>% 
  mutate(name_words = cumsum(count)) 

total_words <- total_words %>% ungroup %>%  arrange(time, indexdf) %>% 
  group_by(namedf) %>% 
  mutate(laugh_words = cumsum(linelaughdf)) 

total_words <- total_words %>% ungroup %>%  arrange(time, indexdf) %>% 
  group_by(committeedf) %>% 
  mutate(laugh_committee = cumsum(linelaughdf)) 


### Graphs #### 


first <- total_words %>% ggplot(aes(x = time,
                           y = words_chamber,
                           color = chamberdf)) +
  geom_line() + ggtitle("Cumulative words per Chamber of Congress")
ggsave(paste(getwd(), "/first.png", sep = ""),
       first, height = 7, width = 10)



second <- total_words %>% ggplot(aes(x = time,
                           y = session_words,
                           color = sessiondf)) +
  geom_line() + ggtitle("Cumulative words per Congress session")
ggsave(paste(getwd(), "/second.png", sep = ""), second, height = 7, width = 10)


fourth <- total_words %>% ggplot(aes(x = time,
                                     y = log(name_words),
                                     color = namedf)) +
  geom_line() +
  theme(legend.position="none") + 
  ggtitle("Cumulative words (log) per Congressman")
# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/fourth.png", sep = ""), fourth, height = 14, width = 10)

fourth1 <- total_words %>% ggplot(aes(x = time,
                                     y = name_words,
                                     color = namedf)) +
  geom_line() +
  theme(legend.position="none") + 
  ggtitle("Cumulative words per Congressman")
# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/fourth1.png", sep = ""), fourth1, height = 14, width = 10)


third <- total_words %>% ggplot(aes(x = time,
                                    y = committee_words,
                                    color = committeedf)) +
  geom_line() + facet_grid(chamberdf~.) + 
  ggtitle("Cumulative words per Congress committee")
ggsave(paste(getwd(), "/third.png", sep = ""), third, height = 14, width = 10)


fifth <- total_words %>% ggplot(aes(x = time,
                                     y = laugh_words,
                                     color = namedf)) +
  geom_line() + theme(legend.position="none") + 
  ggtitle("Cumulative times a Congressman \n made other Congressmen laugh")
# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/fourth1.png", sep = ""), fourth, height = 14, width = 10)

sixth <- total_words %>% ggplot(aes(x = time,
                                    y = laugh_committee,
                                    color = committeedf)) +
  geom_line() + 
  ggtitle("Cumulative times laughter was heard in a committee")
# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/sixth.png", sep = ""), sixth, height = 7, width = 10)

sixth1 <- total_words %>% ggplot(aes(x = time,
                                    y = laugh_committee,
                                    color = committeedf)) +
  geom_line() + facet_grid(chamberdf~.) + 
  ggtitle("Cumulative times laughter was heard in a committee")
# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/sixth1.png", sep = ""), sixth1, height = 7, width = 10)


  

