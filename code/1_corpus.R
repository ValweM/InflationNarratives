## Create Topics
Sys.setenv(LANG = "en")
rm(list=ls())
gc()

## load all required packages

mypackages <- c("dplyr", "quanteda", "keyATM", "spacyr", "quanteda.corpora", "lubridate",
                "stringr", "tidytext", "data.table", "stopwords")

lapply(mypackages, require, character.only = TRUE)

getwd()

## Load dj_news Data ##
#########################

load("../data/datatable/dj_news_raw.rds") ##12.141.064 obs

dj_news <- dj_news %>%
  arrange(docdate) %>%
  select(-column_label)

head(dj_news$docdate)
tail(dj_news$docdate)


## Overview 

colnames(dj_news)
dj_news <- dj_news %>%
  dplyr::rename(doc_id = ID)

## create date variable ##
#########################

head(dj_news$docdate)

dj_news$date<- ymd(dj_news$docdate)
dj_news$year <- year(dj_news$date)
dj_news$month <- month(dj_news$date)
dj_news$day <- day(dj_news$date)


## Filter out all docs pre 2018

dj_news <- dj_news %>%
  filter(date>=make_date(2018,01,01)) ## 8.322.223

## Remove duplicates if text is same 

dj_news <- dj_news[!duplicated(dj_news$text),]
dj_news <- dj_news[!(dj_news$text == "" | is.na(dj_news$text)), ] 






### make it smaller ### before: 
##############################



inflation <- paste(c("[Ii]nflation", "[Dd]eflation", "[Gg]eneral [Pp]rice", "[Gg]eneral [Pp]rices", "[Cc][Pp][Ii]",
                     "[Cc]onsumer [Pp]rice [Ii]ndex", 
                     "[Rr]ising [Pp]rices", "[Ii]ncreasing [Pp]rices", 
                     "[Pp]rice [Ii]ncrease", "[Ff]all in [Pp]rices",
                     "[Dd]ecreasing [Pp]rices", "[Pp]rice [Dd]ecrease"), collapse = "|") ## for LSS set


dj_news<- split(dj_news, (seq(nrow(dj_news))-1) %/% 200000) 

for (i in 1:length(dj_news)){
  tmp<- as.data.frame(dj_news[[i]])                          
  tmp <- tmp %>%
    mutate(text = as.character(text)) %>%
    subset(str_detect(text, inflation) == TRUE)
  dj_news[[i]] <- tmp
  
}

dj_news <-  dj_news%>% ## 8.1 Mio // 1.8Mio
  bind_rows() %>%
  as.data.table()


length(dj_news$text)

save(dj_news, file = "../data/datatable/dj_news_inf.rds") 


#load("../data/datatable/dj_news_inf.rds")


## Filter based on subject id (product)

dj_news<- split(dj_news, (seq(nrow(dj_news))-1) %/% 200000) 

for (i in 1:length(dj_news)){
  tmp<- as.data.frame(dj_news[[i]])                                 ## w = "WSJ", "AWSJ", "WSJE", "SMTM"
  strings <- paste(c( "DJIB", "DJG", "GPRW","DJAN","AWSJ", "WSJE", "PREL", "NRG",
                      "DJBN","AWP","BRNS", "JNL", "WAL", "WLS", "WSJ"), collapse="|") ## all = "DJIB", "DJG", "GPRW","DJAN","AWSJ", "WSJE", "PREL", "NRG", "DIJ", "DJBN","AWP","BRNS", 
  tmp <- tmp %>%
    mutate(text = as.character(text)) %>%
    subset(str_detect(subject, strings) == TRUE)
  dj_news[[i]] <- tmp
  
}

dj_news <-  dj_news%>% ## 274k
  bind_rows() %>%
  as.data.table()


## remove all Tabular materials and technical reports  ## before 245727

dj_news <- dj_news %>%
  mutate(text = as.character(text)) %>%
  subset(!str_detect(subject, paste(c("TAB", "TAN"), collapse = "|")) == TRUE)

## remove Fitch Ratings and others ## before  241999

dj_news <- dj_news %>%
  mutate(text = as.character(text)) %>%
  subset(!str_detect(subject, "FTH") == TRUE)


## remove Calendar ## before  230613

dj_news <- dj_news %>%
  mutate(text = as.character(text)) %>%
  subset(!str_detect(subject, "CAL") == TRUE)


## remove PREL

dj_news <- dj_news %>%
  mutate(text = as.character(text)) %>%
  subset(!str_detect(subject, "PRL") == TRUE)



## Remove additional irrelevant documents #####

### Extract time for future research
string <- ("[0-9]{4} [0-9]{1,2}:[0-9]{1,2} [a-zA-Z]{2,3} \\([0-9]{1,2}:[0-9]{1,2} [a-zA-Z]{2,3}\\)$") 
dj_news$time<- str_extract(dj_news$text, string)



## Create time / hour variable

dj_news$time <- dj_news$time %>%
  str_remove_all("[0-9]{4}") %>%
  str_remove_all("\\([0-9]{1,2}:[0-9]{1,2} [a-zA-Z]{2,3}\\)") %>%
  str_remove_all("[a-zA-Z]{2,3}")

dj_news$hour <- str_remove_all(dj_news$time, ":[0-9]{2}")


save(dj_news, file ="../data/datatable/dj_news.rds")


## Subsample only Wall Street Journal Content

dj_news_w <- dj_news %>%
 subset(str_detect(subject, paste(c("JNL", "WAL", "WLS", "WSJ"), collapse = "|")) == TRUE) 

save(dj_news_w, file ="../data/datatable/dj_news_w.rds")

