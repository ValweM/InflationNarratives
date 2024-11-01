# This is a script to plot and analyse estimated keyATM models

## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL="en_US.UTF-8")
rm(list=ls())
gc()


## load required packages

mypackages <- c("dplyr", "keyATM", "lubridate", "stringr", "data.table",
                "ggplot2", "xtable", "xlsx", "tidyr", "patchwork", "hrbrthemes",
                "plotly", "svglite", "RColorBrewer", "ggstream")

lapply(mypackages, require, character.only = TRUE)


## load keyATM models

## load model with dow-jones newswires data set

#load("../data/models/dynamic_50_s_1_1.rds")
#load("../data/models/dynamic111.rds")
load("../data/models/dynamic_s_7.rds") # or 4
#load("../data/models/dynamic_30_gov_pan.rds") # Current!
djn_model <- dynamic
rm(dynamic)

load("../data/models/dynamic_final_10.rds") 

load("../data/models/dynamic_w.rds") 
wsj_model <- dynamic
rm(dynamic)

dynamic$keywords_raw
djn_model$keywords_raw

## read in data tables of dow-jones and WSJ model

load("../data/datatable/dj_news.rds")

load("../data/datatable/dj_news_inf_w.rds")
#wsj_data <- read.csv(file = "../data/datatable/dj_news_inf_w.csv")


## print words with highest probability of selection for both models

djn_words <- top_words(djn_model, 20)
head(djn_words, 20)
print(xtable(djn_words[,1:13]), type = "latex", file = "../text/tables/words_djn.txt")


## create word clouds
library(ggwordcloud)

cloud_words <- top_words(djn_model, 50)

for (i in 1:13){
  terms <- data.frame(words = cloud_words[[i]], freq = 50:1)
  terms$words <- str_remove(terms$words, "\\[[1-9]\\]")
  terms$words <- str_remove(terms$words, "\\[[1-9][1-9]\\]")
  terms$words <- str_remove(terms$words, "[✓]")
  terms$words <- str_remove(terms$words, "\\[\\]")
  terms
  
  graph <- ggplot(terms, aes(label = words, size = freq)) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 80) +
    theme_minimal()
  
  png(paste0("../text//figures/wordcloud", i, ".png"),  width = 1800, height = 1200)
  
  plot(graph)
  
  dev.off()
  
}




wsj_words <- top_words(wsj_model,20)
head(wsj_words,20)


## create new data frame to compare words

words <- djn_words %>%
  dplyr::select(c(1:13)) %>%
  dplyr::rename(djn_energy = "1_energy") %>%
  dplyr::rename(djn_war = "2_war") %>%
  dplyr::rename(djn_pandemic = "3_pandemic") %>%
  dplyr::rename(djn_labor = "4_labor_shortage") %>%
  dplyr::rename(djn_supplychains = "5_supply_chain") %>%
  dplyr::rename(djn_monetarypolicy = "6_monetary_policy") %>%
  dplyr::rename(djn_governmentspending = "7_government_spending") %>%
  dplyr::rename(djn_pentup = "8_pent_up_demand") %>%
  dplyr::rename(djn_demandshift = "9_demand_shift") %>%
  dplyr::rename(djn_profits = "10_profits") %>%
  dplyr::rename(djn_politics = "11_politics") %>%
  dplyr::rename(djn_debt = "12_debt") %>%
  dplyr::rename(djn_taxes = "13_taxes") %>%
  bind_cols(., wsj_words[1:13]) %>%
  dplyr::rename(wsj_energy = "1_energy") %>%
  dplyr::rename(wsj_war = "2_war") %>%
  dplyr::rename(wsj_pandemic = "3_pandemic") %>% 
  dplyr::rename(wsj_labor = "4_labor_shortage") %>%
  dplyr::rename(wsj_supplychains = "5_supply_chain") %>%
  dplyr::rename(wsj_monetarypolicy = "6_monetary_policy") %>%
  dplyr::rename(wsj_governmentspending = "7_government_spending") %>%
  dplyr::rename(wsj_pentup = "8_pent_up_demand") %>%
  dplyr::rename(wsj_demandshift = "9_demand_shift") %>%
  dplyr::rename(wsj_profits = "10_profits") %>%
  dplyr::rename(wsj_politics = "11_politics") %>%
  dplyr::rename(wsj_debt = "12_debt") %>%
  dplyr::rename(wsj_taxes = "13_taxes") %>%
  dplyr::relocate(wsj_energy, .after = djn_energy) %>%
  dplyr::relocate(wsj_war, .after = djn_war) %>%
  dplyr::relocate(wsj_labor, .after = djn_labor) %>%
  dplyr::relocate(wsj_supplychains, .after = djn_supplychains) %>%
  dplyr::relocate(wsj_monetarypolicy, .after = djn_monetarypolicy) %>%
  dplyr::relocate(wsj_governmentspending, .after = djn_governmentspending) %>%
  dplyr::relocate(wsj_pentup, .after = djn_pentup) %>%
  dplyr::relocate(wsj_demandshift, .after = djn_demandshift) %>%
  dplyr::relocate(wsj_profits, .after = djn_profits) %>%
  dplyr::relocate(wsj_politics, .after = djn_politics) %>%
  dplyr::relocate(wsj_debt, .after = djn_debt) %>%
  dplyr::relocate(wsj_taxes, .after = djn_taxes) 


print(xtable(words), type = "latex", file = "../text/tables/words.txt")
print(xtable(words), type = "html", file = "../text/tables/words.html")


###  First Analysis Dynamic Topics #####

result_figs <- function(model){

# model fit

fig_modelfit <- plot_modelfit(model)
string <- deparse(substitute(model))
save_fig(fig_modelfit, file = paste0("../text/figures/modelfit_", string, ".png"), width = 15, height = 15)


# latent states

p_alpha <- plot_alpha(model)
save_fig(p_alpha, file = paste0("../text/figures/alpha_", string, ".png"), width = 15, height = 15)



# probability of words drawn from keyword topic-word distribution

p_pi <- plot_pi(model)
save_fig(p_pi, file = paste0("../text/figures/pi_", string, ".png"))

# topic appearance likelihood in corpus

topicprop <-plot_topicprop(model, show_topic = 1:12)
save_fig(topicprop, file = paste0("../text/figures/topicprob_", string, ".png"), width = 15, height = 15)

}

result_figs(djn_model)
result_figs(wsj_model)




### Dynamic Analysis ####

dynamic_object <- function(model){

if (deparse(substitute(model)) == "djn_model"){
  load(file="../data/datatable/dj_news.rds")
  
  
  
  data <- cbind(dj_news, model$theta)
  

} else {
  load(file="../data/datatable/dj_news_w.rds")
  
  
  data <- cbind(dj_news_w, model$theta)
  

}

 
  
  data  <- data  %>%
    dplyr::select(-c(docdate, seq, "news-source", headline, text, subject, day, time, hour))
  
  
  ## load sentiment data
  
  if (deparse(substitute(model)) == "djn_model"){
    load(file = "../data/localprojections/sent_dat.rds")
    
    
  } else {
    load(file = "../data/localprojections/sent_dat_wsj.rds")
    dat <- dat_wsj
    
  }
  
  
  
  ## create sentiment score of -1/0/1
  
  dat <- dat %>%
    dplyr::mutate(sentiment = ifelse(fit > 0, 1, 0)) %>%
    dplyr::mutate(sentiment = ifelse(fit <0, -1, sentiment)) %>%
    dplyr::select(doc_id, fit, sentiment)
  
  
  ## merge data by docid
  
  data_sent <- inner_join(data , dat, by="doc_id")
 
  ## create topic-sentiment index
  data_sent <- data_sent %>%
    dplyr::rename_with(~str_replace(., "\\d{1,2}_", ""))
  

  ## calculate the conditional probability 
  
  data_sent <- data_sent %>%
    dplyr::mutate(sum = rowSums(data_sent[, c("energy", "profits", "politics", "debt",
                                                "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                                "government_spending", "pent_up_demand", "demand_shift") ])) %>%
    dplyr::mutate(across(all_of(c("energy", "profits", "politics", "debt",
                                  "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                  "government_spending", "pent_up_demand", "demand_shift" ) ), ~ ./sum))## calculate new probability without nokeyword topics
  
  data_sent <- data_sent %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(across(all_of(c("energy", "profits", "politics", "debt",
                                     "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                     "government_spending", "pent_up_demand", "demand_shift", "fit")), mean, na.rm = TRUE), .groups="drop") %>%
    dplyr::mutate(date = make_date(year, month)) %>%
    dplyr::mutate(sentiment = ifelse(fit > 0, 1, 0)) %>%
    dplyr::mutate(sentiment = ifelse(fit <0, -1, sentiment)) %>%
    dplyr::select(-c(year, month))
  
  

  return(data_sent)
  


}

djn_data <- dynamic_object(djn_model)
wsj_data <- dynamic_object(wsj_model)

## save file for the use of local projections in next script


save(djn_data, file = "../data/localprojections/djn_data.rds")
Sys.sleep(4)
save(wsj_data, file = "../data/localprojections/wsj_data.rds")
Sys.sleep(4)

## prepare dataset for different plot


djn_data <- djn_data %>%
  mutate(dataset = "djn") %>%
  tidyr::pivot_longer(c(1:13), names_to = "topic", values_to = "proportion")

wsj_data <- wsj_data %>%
  mutate(dataset = "wsj") %>%
  tidyr::pivot_longer(c(1:13), names_to = "topic", values_to = "proportion")


vis_data <- rbind(djn_data, wsj_data)


# Plot the time series of the different topics
# Load the required package

library(grDevices)

custom_order <- c("government_spending", "monetary_policy", "pent_up_demand", "demand_shift", 
                  "supply_chain", "labor_shortage", "energy", "pandemic", "politics",
                  "war", "debt", "taxes", "profits") # Specify your custom order

# Reorder the 'topic' factor variable based on custom_order
djn_data$topic <- factor(djn_data$topic, levels = custom_order)

djn_data <- djn_data %>%
  mutate(names = ifelse(topic == "government_spending", "Government Spending", NA)) %>%
  mutate(names = ifelse(topic == "monetary_policy", "Monetary Policy", names)) %>%
  mutate(names = ifelse(topic == "pent_up_demand", "Pent-up Demand", names)) %>%
  mutate(names = ifelse(topic == "demand_shift", "Demand Shift", names)) %>%
  mutate(names = ifelse(topic == "supply_chain", "Supply Chain", names)) %>%
  mutate(names = ifelse(topic == "energy", "Energy", names)) %>%
  mutate(names = ifelse(topic == "labor_shortage", "Labor Shortage", names)) %>%
  mutate(names = ifelse(topic == "pandemic", "Pandemic", names)) %>%
  mutate(names = ifelse(topic == "politics", "Politics", names)) %>%
  mutate(names = ifelse(topic == "war", "War", names)) %>%
  mutate(names = ifelse(topic == "debt", "Government Debt", names)) %>%
  mutate(names = ifelse(topic == "taxes", "Taxes", names)) %>%
  mutate(names = ifelse(topic == "profits", "Profits", names))
  
# Reorder the 'topic' factor variable based on custom_order wsj
wsj_data$topic <- factor(wsj_data$topic, levels = custom_order)

wsj_data <- wsj_data %>%
  mutate(names = ifelse(topic == "government_spending", "Government Spending", NA)) %>%
  mutate(names = ifelse(topic == "monetary_policy", "Monetary Policy", names)) %>%
  mutate(names = ifelse(topic == "pent_up_demand", "Pent-up Demand", names)) %>%
  mutate(names = ifelse(topic == "demand_shift", "Demand Shift", names)) %>%
  mutate(names = ifelse(topic == "supply_chain", "Supply Chain", names)) %>%
  mutate(names = ifelse(topic == "energy", "Energy", names)) %>%
  mutate(names = ifelse(topic == "labor_shortage", "Labor Shortage", names)) %>%
  mutate(names = ifelse(topic == "pandemic", "Pandemic", names)) %>%
  mutate(names = ifelse(topic == "politics", "Politics", names)) %>%
  mutate(names = ifelse(topic == "war", "War", names)) %>%
  mutate(names = ifelse(topic == "debt", "Government Debt", names)) %>%
  mutate(names = ifelse(topic == "taxes", "Taxes", names)) %>%
  mutate(names = ifelse(topic == "profits", "Profits", names))


# basic stream graph
#library(ggstream)
library(grDevices)

# Define the number of colors in your custom palette
num_colors <- 13

# Create a custom pastel-like palette
start_color <- rgb(94, 79, 162, maxColorValue = 255)
second_color <- rgb(92,183,170, maxColorValue = 255)
third_color <- rgb(232,246,155, maxColorValue = 255)
fourth_color <- rgb(253,183,104, maxColorValue = 255)
end_color <- rgb(158, 1, 66, maxColorValue = 255)



# Generate the custom color palette
custom_palette <- colorRampPalette(c(start_color, second_color, third_color, fourth_color, end_color))
colors <- custom_palette(num_colors)



png("../text//figures/narratives_all.png",  width = 1500, height = 1000)

ggplot(djn_data, aes(x = date, y = proportion, fill = topic)) +
  geom_stream(color = 1, lwd = 0.25, type = "proportional") +
  geom_stream_label(aes(label = names, fontface = "bold"), 
                    type = "proportional",
                    size = 10) +
                    #hjust = 0.2,  
                    #vjust = 1) +
  labs(x = "Year",
       y = "Smoothed Proportions",
       fill = "Topics") +
  scale_fill_manual(values = colors)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 25))


dev.off()

## simple demand 

djn_demand <- djn_data %>%
  filter(topic %in% c("pent_up_demand",
                      "demand_shift", "monetary_policy")) 



djn_demand$col <- ifelse(djn_demand$sentiment > 0, "grey80", "white")

plot_demand <- ggplot(djn_demand, aes(x = date, y = 100 * proportion, color = topic)) + 
  geom_line(linewidth = 5) +
  geom_rect(aes(xmin = date, xmax = dplyr::lead(date), ymin = -Inf, ymax = Inf), 
            alpha = .3,fill= djn_demand$col) +
  theme_bw() +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 30),
    legend.position = c(0.9, 0.15)
  ) +
  labs(
    y = "Proportions of Narratives (%)",
    x = "Date",
    color = "Demand Narratives"  # Renamed labs() to specify color legend title
  ) +
  guides(color = guide_legend(title = "Demand Narratives", # Change the legend title
                              override.aes = list(size = 5))) + # Adjust the legend key size
  
  # Change the legend labels
  scale_color_manual(
    values = c(
      "government_spending" = colors[1],
      "monetary_policy" = colors[2],
      "pent_up_demand" = colors[3],
      "demand_shift" = colors[4]
    ),
    breaks = c("government_spending", "monetary_policy", "pent_up_demand", "demand_shift"),
    labels = c("Govt. Spending", "Monetary Policy", "Pent-Up Demand", "Demand Shift")
  )



png(filename = "../text/figures/plot_demand.png", width = 1500, height = 700)
plot_demand
dev.off()



# simple supply

djn_supply <- djn_data %>%
  filter(topic %in% c("supply_chain", "labor_shortage",
                      "energy")) 





djn_supply$col <- ifelse(djn_supply$sentiment > 0, "grey80", "white")

plot_supply <- ggplot(djn_supply, aes(x = date, y = 100 * proportion, color = topic)) + 
  geom_line(linewidth = 5) +
  geom_rect(aes(xmin = date, xmax = dplyr::lead(date), ymin = -Inf, ymax = Inf), 
            alpha = .3,fill= djn_supply$col) +
  theme_bw() +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 30),
    legend.position = c(0.9, 0.15)
  ) +
  labs(
    y = "Proportions of Narratives (%)",
    x = "Date",
    color = "Supply Narratives",  # Renamed labs() to specify color legend title
  ) +
  guides(color = guide_legend(title = "Supply Narratives", # Change the legend title
                              override.aes = list(size = 5))) + # Adjust the legend key size
  
  # Change the legend labels
  scale_color_manual(
    values = c(
      "supply_chain" = colors[5],
      "energy" = colors[6],
      "labor_shortage" = colors[7]
    ),
    breaks = c("supply_chain", "energy", "labor_shortage"),
    labels = c("Supply Chain", "Energy", "Labor Shortage")
  )


png(filename = "../text/figures/plot_supply.png", width = 1500, height = 700)
plot_supply
dev.off()

## simple miscellanous

djn_others <- djn_data %>%
  filter(topic %in% c("pandemic", "politics",
                      "war", "debt", "taxes", 
                      "profits")) 

djn_others$col <- ifelse(djn_others$sentiment > 0, "grey80", "white")

plot_others <- ggplot(djn_others, aes(x = date, y = 100 * proportion, color = topic)) + 
  geom_line(linewidth = 5) +
  geom_rect(aes(xmin = date, xmax = dplyr::lead(date), ymin = -Inf, ymax = Inf), 
            alpha = .3,fill= djn_others$col) +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 30),
    legend.position = c(0.9, 0.15)
  ) +
  labs(
    y = "Proportions of Narratives (%)",
    x = "Date",
    color = "Miscellaneous Narratives",  # Renamed labs() to specify color legend title
    
  ) +
  guides(color = guide_legend(title = "Miscellaneous Narratives", # Change the legend title
                              override.aes = list(size = 5))) + # Adjust the legend key size
  
  # Change the legend labels
  scale_color_manual(
    values = c(
      "pandemic" = colors[8],
      "politics" = colors[9],
      "war" = colors[10],
      "debt" = colors[11],
      "taxes" = colors[12],
      "profits" = colors[13]
    ),
    breaks = c("pandemic", "politics", "war", "debt", "taxes", "profits"),
    labels = c("Pandemic", "Politics", "War", "Debt", "Taxes", "Profits")
  )


png(filename = "../text/figures/plot_others.png", width = 1500, height = 700)
plot_others
dev.off()
## Frequency before and after pandemic outbreak #####

## Only DJN-Data
djn_data <- djn_data %>% 
  mutate(period = ifelse(date <= as.Date("2020-02-01"), "before pandemic", "since pandemic")) %>%
  mutate(group = ifelse(topic %in% c("government_spending", "monetary_policy",
                                   "pent_up_demand", "demand_shift"), "demand", NA )) %>%
  mutate(group = ifelse(topic %in% c("supply_chain", "labor_shortage", "energy"), "supply", group)) %>%
  mutate(group = ifelse(topic %in% c("pandemic", "politics", "war", "debt", "taxes", "profits"), "miscellaneous", group)) %>%
  mutate(group = factor(group, levels = c("demand", "supply", "miscellaneous")))


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE), 
      se=  sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])),
      ic=  sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])) * qt((1-0.05)/2 + .5, length(x[[col]])-1))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

djn_change <- data_summary(djn_data, varname="proportion", 
                    groupnames=c("names", "period", "group"))


plot_change <- ggplot(djn_change, aes(x = names, y = proportion*100, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = proportion*100 - ic*100, ymax = proportion*100 + ic*100),
                position = position_dodge(width = 0.7), width = 0.25, colour = "orange", size = 1.3) +
  labs(x = "Topics", y = "Mean Proportions") +
  facet_grid(cols = vars(group), scales = "free_x") +
  theme_bw() +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 30), 
    plot.title = element_text(size = 40), 
    plot.subtitle = element_text(size=30), 
    legend.text = element_text(size = 25), 
    legend.title = element_text(size = 30), 
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
    legend.position = c(0.85, 0.85)
  ) +
  scale_x_discrete() +
  guides(color = guide_legend(title = "Period", # Change the legend title
                              override.aes = list(size = 5))) +
  facet_grid(~group, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c(colors[10], colors[12])) +
  labs(y = "Proportion of Topics",
       x = "Topics",
       fill = "Period")



png(filename = "../text/figures/plot_change.png", width = 1500, height = 700)
plot_change
dev.off()

## Compare data from djn with wsj

head(vis_data)



# Assuming your data is stored in the vis_data dataframe
png(filename = "../text/figures/comparision.png", width = 1800, height = 1400)

ggplot(vis_data, aes(x = date, y = proportion, color = dataset, group = interaction(topic, dataset))) +
  geom_line() +
  facet_wrap(~topic, scales = "free_y", ncol = 2) +
  labs(
    x = "Date",
    y = "Proportion"
  ) +
  theme_bw()

dev.off()





















## Check the documents

## Check docs with data.table 

dj_news <- read.csv("../data/datatable/dj_news_djn.csv")
head(djn_news)
colnames(djn_news)

djn_news <- djn_news %>%
  select(-X) %>%
  mutate(ID = row_number())


load("../data/datatable/dj_news.rds")

load("../data/datatable/dj_news_w.rds")

dj_news <- dj_news %>%
  dplyr::mutate(ID = row_number())

dj_news_w <- dj_news_w %>%
  dplyr::mutate(ID = row_number())

top_docs <- top_docs(djn_model, 500)
top_docs


top_docs <- top_docs(wsj_model, 500)
top_docs

dj_news_w %>%
  filter(ID == "17166")
dj_news %>%
  filter(ID == "72351")
djn_news %>%
  filter(ID == "159657")



test <- dj_news %>% 
  subset(str_detect(text, "[Dd]emand [Ss]hift") == TRUE)
  
test <- dj_news_w %>% 
  subset(str_detect(text, "[Dd]emand [Tt]rend") == TRUE)
  
## compare with andre t al november 2021
load(file = "../data/localprojections/sdjn_data.rds")

sdjn_data %>%
  filter(date == as.Date("2021-11-01"))

p_data <- sdjn_data %>%
  tidyr::pivot_longer(1:13, names_to = "topic", values_to = "proportion")

plot_change <- ggplot(data=p_data %>%
                        filter(date == as.Date("2021-11-01")), aes(x=topic, y=100*proportion,
                                         fill=topic)) + 
  geom_bar(stat = "summary", fun = mean, width = 0.85) +
  theme_bw() +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 30), 
    plot.title = element_text(size = 40), 
    plot.subtitle = element_text(size=30), 
    legend.text = element_text(size = 25), 
    legend.title = element_text(size = 30), 
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
    legend.position = c(0.9, 0.8)
  ) +
  scale_x_discrete() +
  scale_fill_manual(values = c(colors[10], colors[12])) +
  labs(y = "Proportion of Topics",
       x = "Topics")




png(filename = "../text/figures/plot_change.png", width = 1800, height = 1400)
plot_change
dev.off()


