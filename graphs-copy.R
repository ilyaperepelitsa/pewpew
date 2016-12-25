library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
install.packages("gridExtra")
library(gridExtra)
source("/users/ilyaperepelitsa/quant/ilyaperepelitsa.github.io/hear_theme.R")
install.packages("ggrepel")
library(ggrepel)
install.packages("directlabels")
library(directlabels)

install.packages("RColorBrewer")
library(RColorBrewer)
# devtools::install_github("slowkow/ggrepel")
# options(expressions=10000)

`%notin%` = function(x,y) !(x %in% y)
hearings25 <- read.csv(paste(getwd(), "pew123.txt", sep = "/"))
hearings25 <- read.csv("/users/ilyaperepelitsa/quant/total.csv", sep = ",")
write.csv(total_words, "total.csv", row.names = FALSE, sep = "/")

#### cleaning up the punctiation split output #### 
hearings25 <- df_postgres1
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
hearings25$worddf <- as.factor(hearings25$worddf)
summary(hearings25$worddf)
hearings25$speaker_lastnamedf <- as.factor(hearings25$speaker_lastnamedf)
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
hearings25 <- hearings25 %>% filter(datedf %notin% c("	
AND 20, 2002", "	
AND 23, 2004", "AND 28, 2000", "	
AND 28, 2009"))
# check <- hearings25 %>% group_by(datedf) %>% summarize(n = n())



### MAKING COMMON TIMELINE  and counts ####
# line_orig <- unique(total_words$linedf)
# document_orig <- unique(total_words$datedf)
hearings25$datedf <- str_to_title(hearings25$datedf)
hearings25$datedf <- as.Date(hearings25$datedf, "%B %d, %Y")


# hearings25$datedf <- as.factor(hearings25$datedf)
hearings251 <- hearings25  %>% 
  group_by(datedf) %>% summarize(counting = n())
summary(hearings25$datedf)

hearings25 <- hearings25 %>% filter(!is.na(datedf))
class(hearings25$datedf)
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

# total_words <- total_words %>% ungroup %>%  arrange(time, indexdf) %>% 
#   group_by(sessiondf) %>% 
#   mutate(session_words = cumsum(count)) 

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

class(total_words$datedf)




### Graphs #### 
total_words$datedf <- as.Date(total_words$datedf)
attach(total_words)


first <- total_words %>% ggplot(aes(x = time,
                           y = words_chamber,
                           color = chamberdf)) +
  geom_line() + 
  ggtitle("Total words per Chamber of Congress", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) 
  # geom_text_repel(data = subset(total_words, 
  #                               words_chamber == max(words_chamber)),
  #   aes(label = chamberdf,
  #   size = 6,
  #   nudge_x = 45,
  #   segment.color = NA)


for (i in 1:length(levels(chamberdf))){
  
   first <- first +


     geom_text_repel(
       data = subset(total_words %>%
                       filter(chamberdf == as.character(levels(chamberdf)[i])),
                     words_chamber == max(words_chamber)),
       aes(label = paste("", words_chamber)),
       size = 4,
       nudge_x = 0,
     segment.color = NA) +
     

     
       geom_text_repel(
       data = subset(total_words %>%
                       filter(chamberdf ==  as.character(levels(chamberdf)[i])),
                     words_chamber == max(words_chamber)),
       aes(label = paste("", chamberdf)),
       size = 4,
       nudge_x = 0,
       segment.color = NA)}





ggsave(paste(getwd(), "/ilyaperepelitsa.github.io/cleaner_first.png", sep = ""), first , height = 7, width = 10)

summary(total_words$session_words)

second <- total_words %>% ggplot(aes(x = time,
                           y = session_words/1000,
                           color = sessiondf)) +
  geom_line() + 
  ggtitle("Total words per Congress session", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Pastel2") +
  geom_dl(aes(label = sessiondf), method = list(dl.trans(x = x + 0.3), "last.bumpup"))


ggsave(paste(getwd(), "/ilyaperepelitsa.github.io/cleaner_second1.pdf", sep = ""), second, height = 7, width = 10)

total_words

fourth <- total_words %>% ggplot(aes(x = time,
                                     y = log(name_words),
                                     color = namedf)) +
  geom_line() +
  theme(legend.position="none") + 
  ggtitle("Total words (log) per Congressman", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
# + facet_grid(chamberdf~.)

for (i in 1:length(levels(namedf))){
  
  fourth <- fourth +
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(namedf == as.character(levels(namedf)[i])),
                    name_words == max(name_words)),
      aes(label = paste("", name_words)),
      size = 4,
      nudge_x = 0,
      segment.color = NA) +
    
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(namedf ==  as.character(levels(namedf)[i])),
                    name_words == max(name_words)),
      aes(label = paste("", namedf)),
      size = 4,
      nudge_x = 0,
      segment.color = NA)}


ggsave(paste(getwd(), "/publish/cleaner_fourth.png", sep = ""), fourth, height = 14, width = 10)

fourth1 <- total_words %>% ggplot(aes(x = time,
                                     y = name_words/1000,
                                     color = namedf)) +
  geom_line() +
  theme(legend.position="none") + 
  ggtitle("Total words per Congressman", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_brewer(palette = "Pastel2") +
  geom_dl(aes(label = name_words/1000), method = list(dl.trans(x = x + 0.3), "last.bumpup"))

for (i in 1:length(levels(namedf))){
  
  fourth1 <- fourth1 +
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(namedf == as.character(levels(namedf)[i])),
                    name_words == max(name_words)),
      aes(label = paste("", name_words)),
      size = 4,
      nudge_x = 0,
      segment.color = NA) +
    
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(namedf ==  as.character(levels(namedf)[i])),
                    name_words == max(name_words)),
      aes(label = paste("", namedf)),
      size = 4,
      nudge_x = 0,
      segment.color = NA)}


# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/ilyaperepelitsa.github.io/cleaner_fourth123.pdf", sep = ""), fourth1, height = 14, width = 10)


third <- total_words %>% ggplot(aes(x = time,
                                    y = committee_words,
                                    color = committeedf)) +
  geom_line() + facet_grid(chamberdf~.) + 
  ggtitle("Total words per Congress committee", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))

for (i in 1:length(levels(namedf))){
  
  third <- third +
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(committeedf == as.character(levels(committeedf)[i])),
                    committee_words == max(committee_words)),
      aes(label = paste("", committee_words)),
      size = 4,
      nudge_x = 0,
      segment.color = NA) +
    
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(committeedf ==  as.character(levels(committeedf)[i])),
                    committee_words == max(committee_words)),
      aes(label = paste("", committeedf)),
      size = 4,
      nudge_x = 0,
      segment.color = NA)}
ggsave(paste(getwd(), "/publish/cleaner_third.png", sep = ""), third, height = 14, width = 10)


fifth <- total_words %>% ggplot(aes(x = time,
                                     y = laugh_words,
                                     color = namedf)) +
  geom_line() + theme(legend.position="none") + 
  ggtitle("Total times a Congressman \n made other Congressmen laugh", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_brewer(palette = "Pastel2") +
  geom_dl(aes(label = namedf), method = list(dl.trans(x = x + 0.3), "last.bumpup"))


for (i in 1:length(levels(namedf))){
  
  fourth <- fourth +
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(namedf == as.character(levels(namedf)[i])),
                    laugh_words == max(laugh_words)),
      aes(label = paste("", laugh_words)),
      size = 4,
      nudge_x = 0,
      segment.color = NA) +
    
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(namedf ==  as.character(levels(namedf)[i])),
                    laugh_words == max(laugh_words)),
      aes(label = paste("", namedf)),
      size = 4,
      nudge_x = 0,
      segment.color = NA)}

# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/ilyaperepelitsa.github.io/cleaner_fifth.pdf", sep = ""), fifth, height = 14, width = 10)

sixth <- total_words %>% ggplot(aes(x = time,
                                    y = laugh_committee,
                                    color = committeedf)) +
  geom_line() + 
  ggtitle("Total times laughter was heard in a committee", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) 

for (i in 1:length(levels(namedf))){
  
  sixth <- sixth +
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(committeedf == as.character(levels(committeedf)[i])),
                    laugh_committee == max(laugh_committee)),
      aes(label = paste("", laugh_committee)),
      size = 4,
      nudge_x = 0,
      segment.color = NA) +
    
    
    
    geom_text_repel(
      data = subset(total_words %>%
                      filter(committeedf ==  as.character(levels(committeedf)[i])),
                    laugh_committee == max(laugh_committee)),
      aes(label = paste("", committeedf)),
      size = 4,
      nudge_x = 0,
      segment.color = NA)}

# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/publish/cleaner_sixth.png", sep = ""), sixth, height = 7, width = 10)

sixth1 <- total_words %>% ggplot(aes(x = time,
                                    y = laugh_committee,
                                    color = committeedf)) +
  geom_line() + facet_grid(chamberdf~.) + 
  ggtitle("Total times laughter was heard in a committee", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) 
# + facet_grid(chamberdf~.)
ggsave(paste(getwd(), "/publish/sixth1.png", sep = ""), sixth1, height = 7, width = 10)


new_total <- total_words %>%
  group_by(committeedf, gender) %>% 
  summarize(total = sum(count)) %>% 
  filter(!is.na(gender)) %>% 
  spread(gender, total) %>% 
  mutate(sum = Female + Male)

new_total$Female <- new_total$Female/new_total$sum
new_total$Male <- new_total$Male/new_total$sum

new_total <- new_total  %>% gather("gender", "proportion", 2:3)

genderprop <- new_total %>% ggplot(aes(x = proportion, y = reorder(committeedf, -proportion), color = gender)) + 
  geom_point() + 
  ggtitle("Proportion of all words in Congress Committees, by gender.") + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  scale_x_continuous(labels = scales::percent) + 
  scale_fill_brewer(palette = "Pastel2")
ggsave(paste(getwd(), "/ilyaperepelitsa.github.io/sixth1.pdf", sep = ""), genderprop, height = 7, width = 10)


library(tidyr)
second <- total_words %>% ggplot(aes(x = time,
                                     y = session_words/1000,
                                     color = sessiondf)) +
  geom_line() + 
  ggtitle("Total words per Congress session", 
          subtitle = paste(length(unique(total_words$documentdf)),
                           " documents subset from ", 
                           length(unique(total_words$sessiondf)),
                           " sessions of US Congress from ",
                           min(total_words$datedf),
                           " to ",
                           max(total_words$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Pastel2") +
  geom_dl(aes(label = sessiondf), method = list(dl.trans(x = x + 0.3), "last.bumpup"))


ggsave(paste(getwd(), "/ilyaperepelitsa.github.io/cleaner_second1.pdf", sep = ""), second, height = 7, width = 10)

















laughterhearings <- hearings25 %>% select(sessiondf, chamberdf, committeedf, documentdf, datedf, linedf, namedf, gender, linelaughdf, speaker_titledf)
laughterhearings1 <- unique(laughterhearings)

laughterhearings1 %>% ggplot(aes(x = gender, y = linelaughdf)) + geom_bar(position = "stack")
laughterhearings1 %>% ggplot(aes(x = linelaughdf)) + geom_bar(position = "stack")
laughterhearings1 %>% ggplot(aes(x = linelaughdf, fill = chamberdf)) + geom_bar(position = "fill")


order_plot <- laughterhearings1 %>%
  group_by(committeedf) %>%                              # calculate the counts
  summarize(counts = sum(linelaughdf)) %>%
  arrange(-counts) %>%                                # sort by counts
  mutate(committeedf = factor(committeedf, committeedf)) 

ggplot(aes(x = committeedf, y = linelaughdf)) + geom_bar(stat = "identity") + coord_flip()

# order_vector <- data.frame(order_plot[,1])
class(order_vector)
unname(unlist(order_plot[,1]))

laughterhearings1$committeedf <- ordered(laughterhearings1$committeedf, levels = rev(unname(unlist(order_plot[,1]))))
laughterhearings1$datedf <- as.Date(laughterhearings1$datedf)
hearings25$datedf <- as.Date(hearings25$datedf)


laughterhearings1 %>% filter(linelaughdf > 0) %>% 
  ggplot(aes(x = committeedf, y = linelaughdf, fill = gender)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  hear_theme + 
  scale_fill_brewer(palette="Pastel1") + 
  ggtitle("Number of times laughter was heard in a Congress Committee", 
          subtitle = paste(length(unique(laughterhearings1$documentdf)),
                           " documents sample from ", 
                           length(unique(laughterhearings1$sessiondf)),
                           " sessions of US Congress from ",
                           min(laughterhearings1$datedf),
                           " to ",
                           max(laughterhearings1$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_y_continuous(expand = c(0, 0)) 

laughterhearings1 %>% filter(linelaughdf > 0 & gender %in% c("Male", "Female")) %>% 
  ggplot(aes(x = committeedf, y = linelaughdf, fill = gender)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  hear_theme + 
  scale_fill_brewer(palette="Pastel1") + 
  ggtitle("Number of times laughter was heard in a Congress Committee", 
          subtitle = paste(length(unique(laughterhearings1$documentdf)),
                           " documents sample from ", 
                           length(unique(laughterhearings1$sessiondf)),
                           " sessions of US Congress from ",
                           min(laughterhearings1$datedf),
                           " to ",
                           max(laughterhearings1$datedf), 
                           ".",
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_y_continuous(expand = c(0, 0)) 







new2 <- hearings25 %>%
  filter(gender %in% c("Male", "Female")) %>% 
  group_by(committeedf, gender) %>%                              # calculate the counts
  summarize(counts = n()) 
new2 <- new2 %>%  spread(gender, counts)
new2[is.na(new2)] <- 0
new2$total <- new2$Female + new2$Male 
new2$Female <- new2$Female/new2$total
new2$Male <- new2$Male/new2$total

new2 <- data.frame(new2)
new2 <- new2 %>% arrange(-Female)

hearings25$committeedf <- ordered(hearings25$committeedf, levels = rev(new2[,1]))



# laughterhearings1$committeedf <- ordered(laughterhearings1$committeedf, levels = rev(new2[,1])




words_gender <- hearings25 %>% filter(gender %in% c("Male", "Female")) 
words_gender %>% 
  ggplot(aes(x = committeedf, fill = gender)) + 
  geom_bar(position = "fill") + coord_flip() + 
  hear_theme + 
  scale_fill_brewer(palette="Pastel1") + 
  ggtitle("Women speaking in Congress.", 
          subtitle = paste("Words said by speakers with titles 'Mr', 'Ms', 'Mrs' in a ", length(unique(words_gender$documentdf)),
                           " documents sample from ", 
                           length(unique(words_gender$sessiondf)),
                           " sessions of US Congress from ",
                           min(words_gender$datedf),
                           " to ",
                           max(words_gender$datedf), 
                           ". \n Total words (excluding zipf-like words): ", dim(words_gender)[1], 
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) 
 
  
ggsave("/users/ilyaperepelitsa/ilyaperepelitsa.github.io/gender3.pdf", gender2, height = 7, width = 10)





laughterhearings <- hearings25 %>% select(sessiondf, chamberdf, committeedf, documentdf, datedf, linedf, namedf, gender, linelaughdf, speaker_titledf) %>% 
  filter(gender %in% c("Male", "Female"))
laughterhearings1 <- unique(laughterhearings)

laughterhearings1 %>% 
  ggplot(aes(x = committeedf, fill = gender)) + 
  geom_bar(position = "fill") + coord_flip() + 
  hear_theme + 
  scale_fill_brewer(palette="Pastel1") + 
  ggtitle("Women speaking in Congress.", 
          subtitle = paste("Words said by speakers with titles 'Mr', 'Ms', 'Mrs' in a ", length(unique(words_gender$documentdf)),
                           " documents sample from ", 
                           length(unique(words_gender$sessiondf)),
                           " sessions of US Congress from ",
                           min(words_gender$datedf),
                           " to ",
                           max(words_gender$datedf), 
                           ". \n Total words (excluding zipf-like words): ", dim(words_gender)[1], 
                           sep = "")) + 
  labs(caption = "US Government Publishing Office : \n https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=CHRG") +
  hear_theme + 
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) 


