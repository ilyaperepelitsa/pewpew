library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

### libraries for training and discarding trash rows
library(ISLR)
library(leaps)

options(max.print = 50000)




write.csv(sample_hearings, 
          paste(
            getwd(), 
            "sample_hearings_done.csv", 
          sep = "/"), 
          row.names = FALSE)
hearings <- read.csv("~/variables/tagged_hearings.csv")
# hearings <- read.csv("~/variables/tagged_hearings.csv")
# 
sample_hearings <- read.csv(paste(getwd(), "sample_hearings_done.csv", sep = "/"))
str(sample_hearings)
attach(sample_hearings)
### CLEANING UP AND SETTING UP THE SAMPLE FOR INSTRUCTORS #####



# sample_hearings <- hearings %>% filter(paragraph != "")

sample_hearings$date <- as.Date(sample_hearings$date, "%B %d, %Y")




### SELECTING RANGE######



  
sample_hearings <-hearings %>% 
  filter(paragraph != "") %>% 
  filter(document > 0 & document < 25) %>% 
  select(-(c(path, link)))


# hearings <- hearings %>% filter(date > "1990-01-01")
### CREATING  VARIABLES  AND CLEANING UP######


sample_hearings$countlaugh <- lengths(regmatches(sample_hearings$meta, 
                                                 gregexpr("laughter", sample_hearings$meta, 
                                                          ignore.case = TRUE))) + 
  lengths(regmatches(sample_hearings$paragraph, 
                     gregexpr("laughter", sample_hearings$paragraph, 
                              ignore.case = TRUE)))


for(i in 1:length(sample_hearings$meta)) {
 
  sample_hearings$countlaugh[i] <- lengths(regmatches(sample_hearings$meta[i], 
                     gregexpr("laughter", sample_hearings$meta[i], 
                              ignore.case = TRUE))) + 
    lengths(regmatches(sample_hearings$paragraph[i], 
                       gregexpr("laughter", sample_hearings$paragraph[i], 
                                ignore.case = TRUE)))
  print(i)
}

sample_hearings$name <- paste(sample_hearings$speaker_title, sample_hearings$speaker_lastname)
sample_hearings$paragraph <- as.character(sample_hearings$paragraph)
# sample_hearings$paragraph <- gsub("--", "", sample_hearings$paragraph)
# sample_hearings$paragraph <- str_replace(sample_hearings$paragraph, 'Mr.', "Mr")
# sample_hearings$paragraph <- str_replace(sample_hearings$paragraph, "Mrs.", "Mrs")
# sample_hearings$paragraph <- str_replace(sample_hearings$paragraph, "Ms.", "Ms")
# sample_hearings$paragraph <- str_replace(sample_hearings$paragraph, "Dr.", "Dr")
# sample_hearings$paragraph <- str_replace(sample_hearings$paragraph, "...", "")
# sample_hearings$paragraph <- str_replace(sample_hearings$paragraph, ".com", "")
tiff_annoyance <-  "<GRAPHIC(S) NOT AVAILABLE IN TIFF FORMAT>"
sample_hearings$paragraph <- gsub(tiff_annoyance, '', sample_hearings$paragraph, fixed = TRUE)
sample_hearings$paragraph <- gsub("--", "", sample_hearings$paragraph)
sample_hearings$paragraph <- gsub("..", "", sample_hearings$paragraph, fixed = TRUE)



######### TRAINING AND CLEANING #########

sample_hearings$punctuation <- sample_hearings$punctuation + 1
sample_hearings$whitespace <- sample_hearings$whitespace + 1
sample_hearings$upper <- sample_hearings$upper + 1
sample_hearings$lower <- sample_hearings$lower + 1


sample_hearings$total_letters <- sample_hearings$lower + sample_hearings$upper
sample_hearings$punctuation_prop <- sample_hearings$punctuation/sample_hearings$total_letters
sample_hearings$whitespace_prop <- sample_hearings$whitespace/sample_hearings$total_letters
sample_hearings$upper_prop <- sample_hearings$upper/sample_hearings$total_letters


sample_hearings %>% filter(discard != "") %>% 
  ggplot(aes(x = log(punctuation_prop), y = log(upper_prop), color = discard)) +
  geom_point()

sample_hearings %>% filter(discard != "") %>% 
  ggplot(aes(x = log(punctuation_prop), y = log(whitespace_prop), color = discard)) +
  geom_point()

sample_hearings %>% filter(discard != "") %>% 
  ggplot(aes(x = log(upper_prop), y = log(whitespace_prop), color = discard)) +
  geom_point()

sample_hearings %>% ggplot(aes(x = log(punctuation_prop), y = log(upper_prop), color = discard)) +
  geom_point()

sample_hearings %>% ggplot(aes(x = log(punctuation_prop), y = log(whitespace_prop), color = discard)) +
  geom_point()

sample_hearings %>%  ggplot(aes(x = log(upper_prop), y = log(whitespace_prop), color = discard)) +
  geom_point()

##### DATA WRANGLING #############

sample_hearings$discard[sample_hearings$discard == ""] <- NA
sample_hearings$discard <- factor(sample_hearings$discard)
summary(sample_hearings$discard)

str(sample_hearings)
set.seed(23)
sample_model <- sample_hearings %>% filter(!is.na(discard))
sample_fit <- sample_hearings %>% filter(is.na(discard))
sdf <- sample(nrow(sample_model), nrow(sample_model)/2)
sample_train <- sample_model[sdf,]
sample_test <- sample_model[-sdf,]

summary(sample_train$discard)
summary(sample_test$discard)
# levels(sample_test$discard)


train.X <- sample_train[,21:23]
train.Y <- sample_train[,17]
test.X <- sample_test[,21:23]
test.Y <- sample_test[,17]





knn.pred<- knn(train.X, test.X, train.Y, k=3)
summary(knn.pred)
# plot(knn.pred)

## Match - predicted and real "yes" as % of real - success rates
sum(((test.Y =="discard") & (knn.pred=="discard")) == TRUE)/sum(test.Y =="discard")
## Number predicted right
sum(((test.Y =="discard") & (knn.pred=="discard")) == TRUE)
## Cirrectly predicted out of predicted
sum(((test.Y =="discard") & (knn.pred=="discard")) == TRUE)/sum(knn.pred=="discard")




train.X <- sample_model[,21:23]
train.Y <- sample_model[,17]
test.X <- sample_fit[,21:23]
test.Y <- sample_fit[,17]

knn.pred.fit <- knn(train.X, test.X, train.Y, k=3)
sample_fit$discard <- knn.pred.fit

newcsv <- rbind(sample_fit, sample_model)
summary(newcsv$discard)

newcsv <- newcsv %>% filter(discard == "keep")


write.csv(newcsv, 
          paste(
            getwd(), 
            "new.csv", 
            sep = "/"), 
          row.names = FALSE)
