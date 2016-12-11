library(dplyr)
library(tidyr)
library(stringr)
install.packages("gridExtra")
library(gridExtra)
source("DebateTHM.R")


empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

# 
# 
debate <- read.csv("~/Downloads/EX3/debate.csv")
debate$audience <- rep("(SILENCE)", dim(debate)[1])
attach(debate)
# 
debate$Text <- as.character(debate$Text)
class(debate$Text)
for(i in 1:dim(debate)[1]) {
  if(Speaker[i] == "Audience") {
    debate$audience[i-1] <- debate$Text[i]
    }
}
# 
sum(Speaker == "Audience")
# 
summary(debate$Speaker)
summary(debate$Text)
# 
# 
# 
# 
debate <- debate %>% filter(Speaker != "Audience") %>% filter(Speaker %in% c("Trump", "Clinton"))

debate$Text <- str_replace(debate$Text, "Mr.", "Mr")
debate$Text <- str_replace(debate$Text, "...", "")
debate$Text <- str_replace(debate$Text, ".com", "")
debate$Text <- str_replace(debate$Text, "--", ".")




# detach(debate)
# attach(debate2)
# class(debate2$Speaker)
# debate2$Speaker <- as.character(debate2$Speaker)
# class(debate2$Text)
# class(debate2$Date)
# debate2$Date <- as.character(debate2$Date)
# debate2$audience <- as.character(debate2$audience)
# class(debate2$Speaker)
# 
# 
# df <- data.frame(linedf = "", datedf = "", speakerdf = "", audiencedf = "", worddf = "", indexdf = "")
# df <- data.frame()
# dim(df)
# entry <- vector()
# 
# 
# counter <- 0
# 
# for(i in 1:dim(debate)[1]){
# 
#   string_to_process <- unlist(strsplit(Text[i], "\\W"))
# 
#   string_to_process <- string_to_process[string_to_process != ""]
#   for(ind_word in 1:length(string_to_process)){
#     
#     linedf        <- debate2$Line[i]
#     
#     datedf        <- debate2$Date[i]
#     
#     speakerdf     <- debate2$Speaker[i]
#     
#     audiencedf  <- debate2$audience[i]
#     
#     worddf        <- string_to_process[ind_word]
#     indexdf      <- ind_word
#     
# 
#     entry <- data.frame(linedf, datedf, speakerdf, audiencedf, worddf, indexdf)
#     df <- rbind(df, entry)
#     
#     print(entry)
#   }
#   
# }
# 
# debate_words <- na.omit(df)
# 
# 
# 
# write.csv(debate_words, "~/debate_words.csv")

debate_words <- read.csv("~/debate_words.csv")
debate_words <- debate_words[, 2:7]


debate_words$worddf <- as.character(debate_words$worddf)
debate_words$worddf[debate_words$worddf == "s"] <- "is"
debate_words$worddf[debate_words$worddf == "re"] <- "are"
debate_words$worddf[debate_words$worddf == "m"] <- "am"
debate_words$worddf[debate_words$worddf == "don"] <- "do"
debate_words$worddf[debate_words$worddf == "t"] <- "not"
debate_words$worddf[debate_words$worddf == "ve"] <- "have"
debate_words$worddf[debate_words$worddf == "ll"] <- "will"

debate_words$worddf <- as.factor(debate_words$worddf)
summary(debate_words$worddf)

check_words <- debate_words %>% group_by(tolower(worddf)) %>% summarize(n = n()) %>% 
  filter(worddf %notin% c("the", "to", "and", "is", "that", "of",
                          "a", "have", "it", "in", "are", "do", "for", "was",
                          "this", "be", "on", "with", "so", "what", "about",
                          "at", "can", "would", "when"))

`%notin%` = function(x,y) !(x %in% y)



colnames(debate_words) <- c("line", "date", "speaker",
                            "audience", "word", "index")

debate_words$count <- 1
debate_words$word <- tolower(debate_words$word)
attach(debate_words)
total_words<- debate_words %>% filter(speaker %in% c("Trump", "Clinton")) %>% group_by(date, speaker) %>% 
        mutate(words = cumsum(count))

# total_words[15553:dim(total_words)[1], 9] <- total_words[15553:dim(total_words)[1], 9] + 349

debate_words %>% filter(speaker %in% c("Trump", "Clinton")) %>%  
  filter(word %in% c("i", "me"))


# debate_words %>% filter(date == "2016-10-04") %>% group_by(speaker) %>%  summarize (n=n())


class(debate_words$date)
levels(debate_words$date)
# install.packages("ggrepel")
library(ggrepel)

first <- total_words %>% filter(date == "2016-09-26") 
ggplot(first, aes(x = line, y = words, color = speaker)) + geom_line() + 
  geom_text_repel(
    data = subset(first %>% filter(speaker == "Trump"),  words == max(words)),
    aes(label = paste("", words)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first %>% filter(speaker == "Clinton"),  words == max(words)),
    aes(label = paste("", words)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) 


second <- total_words %>% filter(date == "2016-10-09") 
ggplot(second, aes(x = line, y = words, color = speaker)) + geom_line() + 
  geom_text_repel(
    data = subset(second %>% filter(speaker == "Trump"),  words == max(words)),
    aes(label = paste("", words)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second %>% filter(speaker == "Clinton"),  words == max(words)),
    aes(label = paste("", words)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) +
  geom_text_repel(
    data = subset(second %>% filter(speaker == "Clinton"),  words == max(words)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = 5,
    segment.color = NA
  )  +
  geom_text_repel(
    data = subset(second %>% filter(speaker == "Trump"),  words == max(words)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = 5,
    segment.color = NA
  ) 
#   size = 4, colour = "grey50")
 
################# I and ME ######################

i_me_word <- total_words %>% filter(word %in% c("i", "me"))  %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))
first_i_me <- i_me_word %>% filter(date == "2016-09-26") 
second_i_me <- i_me_word %>% filter(date == "2016-10-09") 


plot1 <- ggplot(first_i_me, aes(x = line, y = target, color = speaker)) + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_i_me %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_i_me %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_i_me %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = -8,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_i_me %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = -8,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))
  

# plot2 <- ggplot(first_i_me, aes(x = line, fill = speaker)) + 
#   geom_density(alpha = 0.1, color = "grey")  + 
#   DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

ggplot(first_i_me, aes(x = line, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.4, stackdir = "centerwhole",
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM 

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

  # ggplot(first_i_me, aes(x = line, color = speaker)) + geom_rug()
#   grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))




plot1 <- ggplot(second_i_me, aes(x = line, y = target, color = speaker)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_i_me %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_i_me %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_i_me %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = -8,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_i_me %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = -8,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_i_me, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

ggplot(second_i_me, aes(x = line, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.4, stackdir = "centerwhole",
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM 

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


#################### WE , US and OUR ############################


we_us_our_word <- total_words %>% filter(word %in% c("we", "us", "our"))  %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_we_us_our_word <- we_us_our_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_we_us_our_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_we_us_our_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_we_us_our_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_we_us_our_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = -8,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_we_us_our_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", speaker)),
    size = 4,
    nudge_x = -8,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_we_us_our_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

ggplot(first_we_us_our_word, aes(x = line, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.3, stackdir = "centerwhole",
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM 


grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_we_us_our_word <- we_us_our_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_we_us_our_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_we_us_our_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_we_us_our_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_we_us_our_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

ggplot(second_we_us_our_word, aes(x = line, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.4, stackdir = "centerwhole",
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM 

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))
######################### NOT ################################


not_word <- total_words %>% filter(word %in% c("not", "no")) %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_not_word <- not_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_not_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_not_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_not_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_not_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

ggplot(first_not_word, aes(x = line, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.4, stackdir = "centerwhole",
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM 

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_not_word <- not_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_not_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_not_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_not_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_not_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

ggplot(second_not_word, aes(x = line, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.4, stackdir = "centerwhole",
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM 

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))
################## HE ################


he_word <- total_words %>% filter(word == "he") %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_he_word <- he_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_he_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_he_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_he_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_he_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_he_word <- he_word %>% filter(date == "2016-10-09") 
plot1<-ggplot(second_he_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_he_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_he_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_he_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


################## SHE ################


she_word <- total_words %>% filter(word == "she") %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_she_word <- she_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_she_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_she_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_she_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_she_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_she_word <- she_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_she_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_she_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_she_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_she_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

################## THEY ################


they_word <- total_words %>% filter(word == "they") %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_they_word <- they_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_they_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_they_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_they_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_they_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_they_word <- they_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_they_word, aes(x = line, y = target, color = speaker))  +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_they_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_they_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_they_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

################## PEOPLE ################


people_word <- total_words %>% filter(word == "people") %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_people_word <- people_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_people_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_people_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_people_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_people_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_people_word <- people_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_people_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_people_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_people_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_people_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


################## THINK and KNOW ################


think_know_word <- total_words %>% filter(word %in% c("think", "know")) %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_think_know_word <- think_know_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_think_know_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_think_know_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_think_know_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_think_know_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_think_know_word <- think_know_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_think_know_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_think_know_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_think_know_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_think_know_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

################## PRESIDENT ################


president_word <- total_words %>% filter(word == "president") %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_president_word <- president_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_president_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_president_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_president_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_president_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_president_word <- president_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_president_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_president_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_president_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_president_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

################## AMERICAN ################


american_americans_word <- total_words %>% filter(word %in% c("american", "americans")) %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_american_americans_word <- american_americans_word %>% filter(date == "2016-09-26") 
plot1<- ggplot(first_american_americans_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_american_americans_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_american_americans_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_american_americans_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_american_americans_word <- american_americans_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_american_americans_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_american_americans_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_american_americans_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_american_americans_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

################## WAR ################


war_word <- total_words %>% filter(word == "war") %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_war_word <- war_word %>% filter(date == "2016-09-26") 
plot1 <- ggplot(first_war_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_war_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_war_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_war_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_war_word <- war_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_war_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_war_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_war_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_war_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

################## NUCLEAR ################


nuclear_word <- total_words %>% filter(word == "nuclear") %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_nuclear_word <- nuclear_word %>% filter(date == "2016-09-26") 
plot1<- ggplot(first_nuclear_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_nuclear_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(first_nuclear_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(first_nuclear_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_nuclear_word <- nuclear_word %>% filter(date == "2016-10-09") 
plot1 <- ggplot(second_nuclear_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_nuclear_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_nuclear_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_nuclear_word, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


################## WOMAN and WOMEN ################


woman_women_word <- total_words %>% filter(word %in% c("woman", "women")) %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_woman_women_word <- woman_women_word %>% filter(date == "2016-09-26") 
ggplot(first_woman_women_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_woman_women_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_i_me, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))


second_woman_women_word <- woman_women_word %>% filter(date == "2016-10-09") 
ggplot(second_woman_women_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_woman_women_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_woman_women_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_i_me, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))

################## ACTUALLY


actually_word <- total_words %>% filter(word %in% c("i", "me" )) %>% 
  group_by(date, speaker) %>% 
  mutate(target = cumsum(count))

first_actually_word <- actually_word %>% filter(date == "2016-09-26") 
ggplot(first_actually_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(first_actually_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) +
  geom_text_repel(
    data = subset(first_actually_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA) + 
  
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_i_me, aes(x = line, fill = speaker)) + 
  geom_density(alpha = 0.1, color = "grey")  + 
  DebateTHM + scale_fill_manual(values=c("#597cea", "#ea9459"))

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4)) 


second_actually_word <- actually_word %>% filter(date == "2016-10-09") 


plot1 <- ggplot(second_actually_word, aes(x = line, y = target, color = speaker))  + 
  geom_line(size = 1.5, alpha = 0.5) +
  geom_text_repel(
    data = subset(second_actually_word %>% filter(speaker == "Trump"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(second_actually_word %>% filter(speaker == "Clinton"),  target == max(target)),
    aes(label = paste("", target)),
    size = 4,
    nudge_x = 0,
    segment.color = NA
  ) + 
  DebateTHM + scale_color_manual(values=c("#597cea", "#ea9459"))


plot2 <- ggplot(second_actually_word, aes(x = line, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.4, stackdir = "center",
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM 

grid.arrange(plot2, empty, plot1, 
             ncol=2, nrow=2, 
             widths=c(6, 1), heights=c(1, 4))






perclength <- total_words %>% group_by(line, date, speaker) %>% summarize(maxin = max(index))

total_words <- left_join(total_words, perclength)

iandme <- total_words %>% filter(word %in% c("i", "me", "my")) %>% 
  group_by(speaker) %>% mutate(cumul = cumsum(count))

ggplot(iandme, aes(x = index/maxin, fill = speaker, alpha = 0.3, color = speaker)) + 
  geom_dotplot(stackgroups = TRUE, dotsize = 0.5,
               method = "histodot") + 
  scale_fill_manual(values=c("#597cea", "#ea9459")) +
  scale_color_manual(values=c("#597cea", "#ea9459")) +
  DebateTHM +

  geom_text_repel(
    data = subset(iandme %>% filter(speaker == "Trump" &  cumul == max(cumul))),
    aes(y = 0, label = paste("", cumul)),
    size = 4,
    nudge_x = index,
    segment.color = NA
  ) + 
  geom_text_repel(
    data = subset(iandme %>% filter(speaker == "Clinton" &  cumul == max(cumul))),
    aes(y = 0, label = paste("", cumul)),
    size = 4,
    nudge_x = index,
    segment.color = NA
  ) 
# + ggtitle("Trump and CLinton saying \"I\" and \"me\" \n 
             # September 26th and October 9th ")

 


























