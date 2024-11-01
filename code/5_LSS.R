## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL = "en_US.UTF-8")
rm(list = ls())
gc()


## load required packages

mypackages <- c("dplyr", "quanteda", "quanteda.corpora", "LSX", "text2vec", "textdata",
                "stringr", "ggplot2", "ggrepel", "readxl")

lapply(mypackages, require, character.only = TRUE)

load(file = "../data/localprojections/djn_data.rds")

load(file="../data/corpus/tokens_s.rds")
load(file ="../data/dfm/dfm.rds")


## LSS dictionary

## Define dictionary

dict <- dictionary(list(increasing = c("persistent","surge",
                                       "persist", "elevate", "rise", "increase",
                                       "high", "accelerate", "pressure", "acceleration"),
                        decreasing = c("deflation", "lower", "low", "decline", 
                                       "fall", "weak", "reduction", "decrease",
                                       "reduce", "weakening")))

seed <- as.seedwords(dict)
print(seed)

## identify context words 

inf_lss <- char_context(key_corpus, pattern = c("infla*", "price*"), window = 5, p = 0.05)

## run LSS model

tmod_lss <- textmodel_lss(DFM, seeds = seed,
                          terms = inf_lss, k = 300, cache = TRUE)

head(coef(tmod_lss), 20) # most positive words

tail(coef(tmod_lss), 20) # most negative words

## plot of words (decreasing)

png("../text//figures/decreasing_words.png",  width = 1500, height = 1000)
textplot_terms(tmod_lss, dict["decreasing"])
dev.off()


## plot of words (increasing)

png("../text//figures/increasing_words.png",  width = 1500, height = 1000)
textplot_terms(tmod_lss, dict["increasing"])
dev.off()

## plot of words (all)

term_data <- tibble(term =as.vector(attributes(tmod_lss$beta)[[1]]), frequency = tmod_lss$frequency,
                    polarity = tmod_lss$beta)

dict_terms <- paste(c("^persistent$",
                          "^persist$", "^elevate$", "^rise$", "^increase$",
                          "^high$", "^accelerate$", "^pressure$", "^acceleration$", "^surge$",
                          "^deflation$", "^lower$", "^low$", "^decline$", 
                          "^fall$", "^weak$", "^reduction$", "^decrease$",
                          "^reduce$", "^weakening$"), collapse = "|")



png("../text//figures/lss_words.png",  width = 1500, height = 700)

ggplot(term_data, aes(x = polarity, y = log(frequency), label = term)) + 
  geom_point(aes(color = str_detect(term, dict_terms)), size = 3, alpha = 0.8, show.legend = FALSE) +
  geom_text_repel(
    aes(color = str_detect(term, dict_terms)),
    show.legend = FALSE,
    max.overlaps = 50,
    label.size = 0.7,   # Increase this value to make the labels larger
    box.padding = 0.5,   # Adjust this value to reduce the distance from the labels to the box
    point.padding = 0.2  # Adjust this value to reduce the distance from the labels to the points
  ) +
  scale_color_manual(values = c("FALSE" = "lightgrey", "TRUE" = "black")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 30))

dev.off()



## Similar words
bs_term <- bootstrap_lss(tmod_lss, mode = "terms")
knitr::kable(head(bs_term, 10))

## create file

DFM_doc <- dfm_group(DFM)
dat <- docvars(DFM_doc)
dat$doc_id <- DFM_doc@docvars$docid_
dat$fit <- predict(tmod_lss, newdata = DFM_doc)
dat$date <- as.Date(dat$date)

dat_smooth <- smooth_lss(dat, span = 0.02, engine = "locfit")
head(dat_smooth)


##  plot sentiment all documents

png("../text//figures/sentiment.png",  width = 1500, height = 700)


ggplot(dat_smooth, aes(x = date, y = fit)) + 
  geom_line(size=1) +
  geom_ribbon(aes(ymin = fit - se.fit * 1.96, ymax = fit + se.fit * 1.96), 
              alpha = 0.3, colour = NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y") +
  labs(x = "Date", y = "Inflation sentiment") + 
  theme(text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 30),
    legend.position = c(0.9, 0.15))

dev.off()

## for wall street only 

load("../data/corpus/tokens_w.rds")
load("../data/dfm/dfm_w.rds")
## identify context words 

inf_lss <- char_context(key_corpus, pattern = c("infla*", "price*"), window = 5, p = 0.05)

## run LSS model

tmod_lss <- textmodel_lss(DFM, seeds = seed,
                          terms = inf_lss, k = 300, cache = TRUE)

head(coef(tmod_lss), 20) # most positive words

tail(coef(tmod_lss), 20) # most negative words

## plot of words (decreasing)

png("../text//figures/decreasing_words_wsj.png",  width = 1800, height = 1400)
textplot_terms(tmod_lss, dict["decreasing"])
dev.off()


## plot of words (increasing)

png("../text//figures/increasing_words_wsj.png",  width = 1800, height = 1400)
textplot_terms(tmod_lss, dict["increasing"])
dev.off()

## Similar words
bs_term <- bootstrap_lss(tmod_lss, mode = "terms")
knitr::kable(head(bs_term, 10))

## create file

DFM_doc <- dfm_group(DFM)
dat_wsj <- docvars(DFM_doc)
dat_wsj$doc_id <- DFM_doc@docvars$docid_
dat_wsj$fit <- predict(tmod_lss, newdata = DFM_doc)
dat_wsj$date <- as.Date(dat_wsj$date)

dat_smooth_wsj <- smooth_lss(dat_wsj, span = 0.02, engine = "locfit")
head(dat_smooth_wsj)

save(dat_wsj, file = "../data/localprojections/sent_dat_wsj.rds")

##  plot sentiment all documents

png("../text//figures/sentiment_wsj.png",  width = 1500, height = 700)

ggplot(dat_smooth_wsj, aes(x = date, y = fit)) + 
  geom_line(size=1) +
  geom_ribbon(aes(ymin = fit - se.fit * 1.96, ymax = fit + se.fit * 1.96), 
              alpha = 0.3, colour = NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y") +
  labs(x = "Date", y = "Inflation sentiment") + 
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 30),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.position = c(0.9, 0.15))

dev.off()


## Correlations?

inf <- read.csv("../data/localprojections/CPIAUCSL.csv")

inf <- inf %>%
  mutate(DATE = as.Date(DATE)) %>%
  arrange(DATE) %>%
  filter(DATE <= as.Date("2023-01-01") & DATE >= as.Date("2017-12-01")) %>%
  dplyr::rename(cpi_growth = CPIAUCSL) %>%
  dplyr::select(cpi_growth)

inf_ts <- ts(inf, frequency = 12, start = c(2017, 12))

inf_dlog<- ts((1 + diff(log(inf_ts)))^12-1, start = c(2018, 1), frequency = 12)

plot.ts(inf_dlog)

## correlation check

library(lubridate)

corr_data <- tibble(date = dat$date, sentiment = dat$fit)

corr_data <- corr_data %>%
  group_by(year_month = floor_date(date, "month")) %>%
  dplyr::summarize(sentiment = mean(sentiment)) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  mutate(cpi = inf_ts[2:62])

head(corr_data)

ts.plot(inf_ts)
plot(x=corr_data$year_month, y = corr_data$sentiment, type = "l")
lines(corr_data$year_month, corr_data$cpi)



cor(corr_data$sentiment, corr_data$cpi, method = "spearman")
cor(corr_data$sentiment, corr_data$cpi, method = "pearson")




exp <- read_xlsx("../data/localprojections/infl_exp_1y_3y.xlsx")

exp <- exp %>%
  as.data.frame() %>%
  mutate(date = as.Date(date)) %>%
  dplyr::rename(exp1y = median_1y) %>%
  dplyr::rename(exp3y = median_2y) %>%
  filter(date <= as.Date("2023-01-01") & date >= ("2017-12-01")) %>%
  dplyr::select(-c(date))


# Create Time Series
exp1y_ts <- ts(exp$exp1y, frequency = 12, start = c(2017, 12))
exp3y_ts <- ts(exp$exp3y, frequency = 12, start = c(2017, 12))



corr_data <- corr_data %>%
  mutate(exp1y = exp1y_ts[2:62]) %>%
  mutate(exp3y = exp3y_ts[2:62])

cor(corr_data$sentiment, corr_data$exp1y)
cor(corr_data$sentiment, corr_data$exp3y)

## save the data

save(dat, file = "../data/localprojections/sent_dat.rds")

















library(quanteda.sentiment)
library(tidyr)



## lexicon_loughran Harvard IV-4 Psychological Dictionary

## robustness from larsen thorsrud


lex <- lexicon_afinn(dir = NULL,
                     delete = FALSE,
                     return_path = FALSE,
                     clean = FALSE,
                     manual_download = FALSE)
afinn <- list()
afinn$pos <- lex[lex$value >= 2, 1]
afinn$neg <- lex[lex$value <= -2, 1]

library(SentimentAnalysis)
data(DictionaryGI)

dict_havard <- dictionary(list(neg = afinn$neg, pos = afinn$pos))
polarity(dict_havard) <- list(pos = "pos", neg = "neg")

seed_har <- as.seedwords(dict_havard)

tmod_lss <- textmodel_lss(DFM, seeds = seed_har,
                          terms = inf_lss, k = 300, cache = TRUE)

head(coef(tmod_lss), 20) # most positive words

tail(coef(tmod_lss), 20) # most negative words


DFM_doc <- dfm_group(DFM)
dat <- docvars(DFM_doc)
dat$doc_id <- DFM_doc@docvars$docid_
dat$fit <- predict(tmod_lss, newdata = DFM_doc)
dat$date <- as.Date(dat$date)

dat_smooth <- smooth_lss(dat, engine = "locfit")
head(dat_smooth)

plot(dat$date, dat$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.8, 0.8),
     xlab = "Time", ylab = "Economic sentiment")
lines(dat_smooth$date, dat_smooth$fit, type = "l")
lines(dat_smooth$date, dat_smooth$fit + dat_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, lty = c(1, 2))



lex<- lexicon_loughran(
  dir = NULL,
  delete = FALSE,
  return_path = FALSE,
  clean = FALSE,
  manual_download = FALSE
)

dict_loughran <-dictionary(list(pos = lex[lex$sentiment== "positive", "word"], neg = lex[lex$sentiment== "negative", "word"]))
polarity(dict_loughran) <- list(pos = "pos", neg = "neg")

seed_lou <- as.seedwords(dict_loughran)
print(seed_lou)



## run LSS model

tmod_lss <- textmodel_lss(DFM, seeds = seed_lou,
                          terms = inf_lss, k = 100, cache = TRUE)

head(coef(tmod_lss), 20) # most positive words

tail(coef(tmod_lss), 20) # most negative words

## plot of words (decreasing)


textplot_terms(tmod_lss, dict_loughran["neg"])


## plot of words (increasing)

textplot_terms(tmod_lss, dict_loughran["pos"])



DFM_doc <- dfm_group(DFM)
dat <- docvars(DFM_doc)
dat$doc_id <- DFM_doc@docvars$docid_
dat$fit <- predict(tmod_lss, newdata = DFM_doc)
dat$date <- as.Date(dat$date)

dat_smooth <- smooth_lss(dat, engine = "locfit")
head(dat_smooth)


##  plot sentiment all documents

plot(dat$date, dat$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.8, 0.8),
     xlab = "Time", ylab = "Economic sentiment")
lines(dat_smooth$date, dat_smooth$fit, type = "l")
lines(dat_smooth$date, dat_smooth$fit + dat_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, lty = c(1, 2))


## Alternative: Count strings
library(stringr)

load("../data/datatable/dj_news.rds")

search_string <- paste(c("rising inflation", "rising prices"), collapse = "|")

# Count occurrences in each cell of the specified column
dat$count_rising <- str_count(dj_news$text, search_string)

search_falling <- paste(c("falling inflation", "falling prices"), collapse = "|")

dat$count_falling <- str_count(dj_news$text, search_falling)

dat$sentiment_search <- ifelse(dat$count_rising > dat$count_falling, 1, 0)
dat$sentiment_search <- ifelse(dat$count_falling > dat$count_rising, -1, dat$sentiment_search)

dat$fit <- dat$sentiment_search

dat_smooth <- smooth_lss(dat, engine = "locfit")


plot(dat$date, dat$sentiment_search, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-1.5, 1.5),
     xlab = "Time", ylab = "Economic sentiment")
lines(dat$date, dat$sentiment_search, type = "l")
lines(dat_smooth$date, dat_smooth$fit, type = "l")
lines(dat_smooth$date, dat_smooth$sentiment_search + dat_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, lty = c(1, 2))