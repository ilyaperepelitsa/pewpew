library(dplyr)
install.packages("RPostgreSQL")

library(RPostgreSQL)
drv = dbDriver("PostgreSQL")



working_hearings <- read.csv(paste(getwd(), 
                                  "new.csv", 
                                  sep = "/")
                            )



pw <- {"isn033k01+42lib7867"}


con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
# rm(pw)



# df_postgres <- dbGetQuery(con, "SELECT * from work_table")

# print(con)



# pew_hearings <- read.csv(paste(getwd(), "pew.csv",  sep = "/"), sep = "/")


# 
# working_hearings$discard <- factor(working_hearings$discard)
# working_hearings <- working_hearings %>% 
#   select(-c(meta, laughter, punctuation,
#             upper, lower, whitespace,
#             total_letters, punctuation_prop,
#             whitespace_prop, upper_prop))
# 
# write.csv(working_hearings, 
#           paste(
#             getwd(), 
#             "new.csv", 
#             sep = "/"), 
#           row.names = FALSE)

summary(working_hearings)

# working_hearings$date <- as.Date(working_hearings$date, "%B %d, %Y")

working_hearings$paragraph <- as.character(working_hearings$paragraph)
# str(working_hearings)

strsplit(working_hearings$paragraph[1], "\\W")

df <- data.frame()
dim(df)
entry <- vector()

names = data.frame("sessiondf", "chamberdf", "committeedf", 
          "documentdf", "datedf", "linedf", 
          "speaker_titledf", "speaker_lastnamedf", "namedf", 
          "linelaughdf", "worddf", "indexdf")
# write.table(names, "pew123.txt", row.names = F, col.names = F, append = T, sep = "/")
dbWriteTable(con, "first30", 
             value = names, append = TRUE, row.names = FALSE, col.names = FALSE)
df_postgres <- dbGetQuery(con, "SELECT * from first30")

for(par in 1:dim(working_hearings)[1]){

    string_to_process <- unlist(strsplit(working_hearings$paragraph[par], "\\W"))

    string_to_process <- string_to_process[string_to_process != ""]
    string_to_process <- string_to_process[!is.na(string_to_process)]
    # print(string_to_process)
    for(ind_word in 1:length(string_to_process)){

      sessiondf          <- working_hearings$session[par]
      chamberdf          <- working_hearings$chamber[par]
      committeedf        <- working_hearings$committee[par]
      documentdf         <- working_hearings$document[par]
      datedf             <- working_hearings$date[par]
      linedf             <- working_hearings$line[par]
      speaker_titledf    <- working_hearings$speaker_title[par]
      speaker_lastnamedf <- working_hearings$speaker_lastname[par]
      namedf             <- working_hearings$name[par]
      linelaughdf        <- working_hearings$countlaugh[par]
      worddf             <- string_to_process[ind_word]
      if(length(worddf) == 0L){
        next
      }
      indexdf            <- ind_word


      # print(sessiondf)
      # print(chamberdf)
      # print(committeedf)
      # print(documentdf)
      # print(datedf)
      # print(linedf)
      # print(speaker_titledf)
      # print(speaker_lastnamedf)
      # print(namedf)
      # print(linelaughdf)
      # print(worddf)
      # print(indexdf)
      
      print("---------------------------------")
      entry <- data.frame(sessiondf, chamberdf, committeedf,
                          documentdf, datedf, linedf,
                          speaker_titledf, speaker_lastnamedf, 
                          namedf, linelaughdf, worddf, indexdf)
      print(entry)
      # write.table(entry, "pew123.txt", row.names = F, col.names = F, append = T, sep = "/")
      dbWriteTable(con, "first30", 
                   value = entry, append = TRUE, row.names = FALSE, col.names = FALSE)
      
    }

}
summary(df)
str(working_hearings)
entry <- c("sessiondf", "chamberdf", "committeedf",
           "documentdf", "datedf", "linedf",
           "speaker_titledf", "speaker_lastnamedf", 
           "namedf", "linelaughdf", "worddf", "indexdf")
df <- read.csv(paste(getwd(), "pew123.txt", sep = "/"), sep = "/", col.names = entry)



colnames(df_postgres) <- df_postgres[1,]
df_postgres <- df_postgres[2:dim(df_postgres)[1],]

df_postgres <- as.data.frame(df)
summary(df_postgres)
df_postgres$sessiondf <- as.factor(df_postgres$sessiondf)
df_postgres$chamberdf <- as.factor(df_postgres$chamberdf)
df_postgres$documentdf <- as.numeric(df_postgres$documentdf)
df_postgres$datedf <- as.factor(df_postgres$datedf)
df_postgres$linedf <- as.numeric(df_postgres$linedf)
df_postgres$speaker_titledf <- as.factor(df_postgres$speaker_titledf)
df_postgres$speaker_lastnamedf <- as.factor(df_postgres$speaker_lastnamedf)
df_postgres$linelaughdf <- as.numeric(df_postgres$linelaughdf)
df_postgres$worddf <- as.character(df_postgres$worddf)
df_postgres$indexdf <- as.numeric(df_postgres$indexdf)

df_postgres1 <- df_postgres %>% filter(!(speaker_lastnamedf %in% c("Chairman", "Secretary")))

write.csv(df_postgres_small, 
          paste(
            getwd(), 
            "first25.csv", 
            sep = "/"), 
          row.names = FALSE)

