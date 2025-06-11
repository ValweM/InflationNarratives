## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL = "en_US.UTF-8")
rm(list = ls())
gc()


## load required packages

mypackages <- c("dplyr", "quanteda", "quanteda.corpora", "LSX", "text2vec", "textdata",
                "stringr", "ggplot2", "ggrepel", "readxl", "lubridate", "kableExtra")

lapply(mypackages, require, character.only = TRUE)

load(file = "./data/localprojections/djn_data.rds")
load(file="./data/corpus/tokens.rds") # It is not possible to reproduce due to data license restrictions
load(file ="./data/dfm/dfm.rds")


## LSS dictionary

## Define dictionary

dict <- dictionary(list(increasing = c("persistent","surge",
                                       "persist", "elevate", "rise", "increase",
                                       "high", "accelerate", "pressure", "acceleration"),
                        decreasing = c("deflation", "lower", "low", "decline", 
                                       "fall", "ease", "reduction", "decrease",
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

png("./text/figures/decreasing_words.png",  width = 1500, height = 1000)
textplot_terms(tmod_lss, dict["decreasing"])
dev.off()


## plot of words (increasing)

png("./text/figures/increasing_words.png",  width = 1500, height = 1000)
textplot_terms(tmod_lss, dict["increasing"])
dev.off()

## plot of words (all)

term_data <- tibble(term =as.vector(attributes(tmod_lss$beta)[[1]]), frequency = tmod_lss$frequency,
                    polarity = tmod_lss$beta)

dict_terms <- paste(c("^persistent$",
                      "^persist$", "^elevate$", "^rise$", "^increase$",
                      "^high$", "^accelerate$", "^pressure$", "^acceleration$", "^surge$",
                      "^deflation$", "^lower$", "^low$", "^decline$", 
                      "^fall$", "^ease$", "^reduction$", "^decrease$",
                      "^reduce$", "^weakening$"), collapse = "|")



png("./text/figures/lss_words.png",  width = 1500, height = 700)

ggplot(term_data, aes(x = polarity, y = log(frequency), label = term)) + 
  geom_point(aes(color = str_detect(term, dict_terms)), size = 3, alpha = 0.8, show.legend = FALSE) +
  geom_text_repel(
    aes(color = str_detect(term, dict_terms)),
    show.legend = FALSE,
    max.overlaps = 50,
    size = 7,   # Increase this value to make the labels larger
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
knitr::kable(bs_term, format = "latex")

## create file

DFM_doc <- dfm_group(DFM)
dat <- docvars(DFM_doc)
dat$doc_id <- DFM_doc@docvars$docid_
dat$fit <- predict(tmod_lss, newdata = DFM_doc)
dat$date <- as.Date(dat$date)

dat_smooth <- smooth_lss(dat, span = 0.02, engine = "locfit")
head(dat_smooth)


##  plot sentiment all documents

png("./text/figures/sentiment.png",  width = 1500, height = 700)


ggplot(dat_smooth, aes(x = date, y = fit)) + 
  geom_line(size=1) +
  geom_ribbon(aes(ymin = fit - se.fit * 1.96, ymax = fit + se.fit * 1.96), 
              alpha = 0.3, colour = NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y") +
  labs(x = "Date", y = "Inflation sentiment") + 
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 30),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.position = c(0.9, 0.15))

dev.off()

## for wall street only 

load("./data/corpus/tokens_w.rds")
load("./data/dfm/dfm_w.rds")
## identify context words 

inf_lss <- char_context(key_corpus, pattern = c("infla*", "price*"), window = 5, p = 0.05)

## run LSS model

tmod_lss <- textmodel_lss(DFM, seeds = seed,
                          terms = inf_lss, k = 300, cache = TRUE)

head(coef(tmod_lss), 20) # most positive words

tail(coef(tmod_lss), 20) # most negative words

## plot of words (decreasing)

png("./text/figures/decreasing_words_wsj.png",  width = 1800, height = 1400)
textplot_terms(tmod_lss, dict["decreasing"])
dev.off()


## plot of words (increasing)

png("./text/figures/increasing_words_wsj.png",  width = 1800, height = 1400)
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

save(dat_wsj, file = "./data/localprojections/sent_dat_wsj.rds")

##  plot sentiment all documents

png("./text/figures/sentiment_wsj.png",  width = 1500, height = 700)

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

inf <- read.csv("./data/localprojections/CPIAUCSL.csv")

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

corr_data <- tibble(date = dat$date, sentiment = dat$fit)

corr_data <- corr_data %>%
  group_by(year_month = floor_date(date, "month")) %>%
  dplyr::summarize(sentiment = mean(sentiment)) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  mutate(cpi = inf_dlog)

exp <- read_xlsx("./data/localprojections/infl_exp_1y_3y.xlsx")

exp <- exp %>%
  as.data.frame() %>%
  mutate(date = ym(date)) %>%
  dplyr::rename(exp1y = median_1y) %>%
  dplyr::rename(exp3y = median_3y) %>%
  filter(date <= "2023-01-01" & date >= "2017-12-01") %>%
  dplyr::select(-c(date))


# Create Time Series

exp1y_ts <- ts(exp$exp1y, frequency = 12, start = c(2017, 12))
exp3y_ts <- ts(exp$exp3y, frequency = 12, start = c(2017, 12))

corr_data <- corr_data %>%
  mutate(exp1y = exp1y_ts[2:62]) %>%
  mutate(exp3y = exp3y_ts[2:62])



## correlation with econ

econ <- read.csv("./data/localprojections/USPHCI.csv")
head(econ)

econ <- econ %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE <= as.Date("2023-01-01") & DATE >= as.Date("2017-12-01")) %>%
  dplyr::rename(econ_ac = USPHCI) %>%
  dplyr::select(econ_ac)


# create time series

econ_ts <- ts(econ, frequency = 12, start = c(2017, 12))


# seasonally adjusted at annual rates

econ_dlog<- ts((1 + diff(log(econ_ts)))^12-1, start = c(2018, 1), frequency = 12)

corr_data <- corr_data %>%
  mutate(econ = econ_dlog)



# Calculate all correlations
cor_sentiment_cpi     <- cor(corr_data$sentiment, corr_data$cpi, method = "pearson")
cor_sentiment_exp1y   <- cor(corr_data$sentiment, corr_data$exp1y, method = "pearson")
cor_sentiment_exp3y   <- cor(corr_data$sentiment, corr_data$exp3y, method = "pearson")
cor_sentiment_econ    <- cor(corr_data$sentiment, corr_data$econ, method = "pearson")

# Create a data frame for the table
cor_table <- data.frame(
  Variable = c("CPI Inflation (YoY, log diff)", 
               "1-Year Inflation Expectation", 
               "3-Year Inflation Expectation", 
               "Economic Activity Index (YoY, log diff)"),
  `Pearson Correlation` = round(c(
    cor_sentiment_cpi,
    cor_sentiment_exp1y,
    cor_sentiment_exp3y,
    cor_sentiment_econ
  ), 3)
)

# Create LaTeX table using kable
custom_caption <- "Pearson correlation coefficients between monthly direction score and selected macroeconomic indicators, including CPI inflation, short- and medium-term inflation expectations, and economic activity."
cor_table_latex <- cor_table %>%
  kable("latex", booktabs = TRUE, linesep = "", caption = custom_caption) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

write(cor_table_latex, file = "./text/tables/cor_table.tex")


## save the data

save(dat, file = "./data/localprojections/sent_dat.rds")











