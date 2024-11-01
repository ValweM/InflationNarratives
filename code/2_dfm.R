## Create Topics
Sys.setenv(LANG = "en")
rm(list=ls())
gc()



mypackages <- c("dplyr", "quanteda", "keyATM", "quanteda.corpora", "lubridate", 
            "stringr", "tidytext", "data.table", "quanteda.textstats", 
            "quanteda.textplots", "textdata", "spacyr")

lapply(mypackages, require, character.only = TRUE)

getwd()

load("../data/datatable/dj_news_w.rds")
load("../data/datatable/dj_news.rds")
write.csv(dj_news_w, "../data/datatable/dj_news_w.csv")
write.csv(dj_news, "../data/datatable/dj_news.csv")
## Create a document-feature matrix (a dfm object) from a token object

## read spacy lemmatised file 
dj_news_w <- read.csv(file = "../data/datatable/lemma_w.csv")
dj_news <- read.csv(file = "../data/datatable/dj_news_lemma.csv")

## save as rds file

save(dj_news, file = "../data/datatable/dj_news_lemma.rds")
save(dj_news_w, file = "../data/datatable/dj_news_w_lemma.rds")


load("../data/datatable/dj_news.rds")
## create corpus
dj_news <- dj_news %>%
  select(-Unnamed..0)

## filter date
dj_news <- dj_news %>%
  arrange(date)

dj_news <- dj_news %>%
  filter(date >= make_date(2018,01,01))


## check if correct dates selected

head(dj_news$date)
## 2018-07-25

tail(dj_news$date)
## 2023-01-31




## Create Corpus 
key_corpus <- corpus(dj_news, text_field = "text")
key_corpus_w <- corpus(dj_news_w, text_field = "text")
save(key_corpus, file = "../data/corpus/corpus.rds")
save(key_corpus, file = "../data/corpus/corpus_w.rds")

## Tokenization and remove stopwords etc.

strings <- paste(c("quot", "wsj.com", "dow", "jones", "end", "dowjones.com", "padd", "212-416-2800", "renae.dyer", "wt", "c", "may", "shall", "can",
                   "must", "upon", "with", "without", "apos", "s", "gmt", "newswires",
                   "t", "also", "amp", "mr", "ms", "amp", "na", "n", "s", "et", "y", 
                   "z", "p", "r", "y", "b", "idx", "index.htm",  "[a-zA-Z]{3}\\d{2}$", "d", "@[A-Za-z0-9]{5-20}", 
                   "@lorena_rbal", "com", "m", "da", "-0.01", "@dougcameron", "--"))#, lessfrequent$feature))



key_corpus<- key_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE, remove_url = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("en", source = "marimo"))) %>%
  tokens_remove(pattern = c("^[a-zA-Z]{3}\\d{2}$")) %>%
  tokens_remove(pattern = strings)
key_corpus_w<- key_corpus_w %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE, remove_url = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("en", source = "marimo"))) %>%
  tokens_remove(pattern = c("^[a-zA-Z]{3}\\d{2}$")) %>%
  tokens_remove(pattern = strings)

save(key_corpus, file ="../data/corpus/tokens.rds")
save(key_corpus_w, file = "../data/corpus/tokens_w.rds")





## Create DFM File dfm_trim(min_docfreq = 0.2, termfreq_type = "prop") %>% 

DFM<- dfm(key_corpus) %>%
  dfm_trim(min_docfreq = 0.01, docfreq_type = "prop") %>%
  dfm_subset(ntoken(.) > 0)

DFM_w<- dfm(key_corpus_w) %>%
  dfm_trim(min_docfreq = 0.01, docfreq_type = "prop") %>%
  dfm_subset(ntoken(.) > 0)


save(DFM, file ="../data/dfm/dfm.rds")
save(DFM_w, file ="../data/dfm/dfm_w.rds")


## remove lessfrequent words and most frequent ones
word_list <- textstat_frequency(DFM)
mostfrequent <- head(word_list, 1000)
lessfrequent<- tail(word_list, 3000)


write.csv(mostfrequent, file = "../data/Output/mostfrequent.csv")
write.csv(lessfrequent, file = "../data/Output/lessfrequent.csv")


strings <- paste(c("quot", "wsj.com", "dow", "jones", "end", "dowjones.com", "padd", "212-416-2800", "renae.dyer", "wt", "c", "may", "shall", "can",
                   "must", "upon", "with", "without", "apos", "s", "gmt", "newswires",
                   "t", "also", "amp", "mr", "ms", "amp", "na", "n", "s", "et", "y", 
                   "z", "p", "r", "y", "b", "idx", "index.htm",  "[a-zA-Z]{3}\\d{2}$", "d", "@[A-Za-z0-9]{5-20}", "@lorena_rbal", "com", "m", "da", "-0.01", "@dougcameron"))#, lessfrequent$feature))


DFM<- dfm_remove(DFM, strings)


DFM <- dfm_select(DFM, "[a-zA-Z][a-zA-Z][a-zA-Z][1-9][1-9]", selection = "remove", valuetype = "regex")

save(DFM, file = "../data/dfm/dfm.rds")
save(DFM_w, file = "../data/dfm/dfm_w.rds")




## Wordlcoud of (weighted) Frequency


dev.new(width=1000, height = 1000, unit = "px")
png(file ="../text/figures/Wordcloud_w.png")

set.seed(12567864)
textplot_wordcloud(DFM)


dev.off()




