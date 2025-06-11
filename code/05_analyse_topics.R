# This is a script transform and analyse estimated keyATM models
## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL="en_US.UTF-8")
rm(list=ls())
gc()


## load required packages

mypackages <- c("dplyr", "keyATM", "lubridate", "stringr", "data.table",
                "ggplot2", "xtable", "xlsx", "tidyr", "patchwork", "hrbrthemes",
                "plotly", "svglite", "RColorBrewer", "ggstream", "grDevices")


lapply(mypackages, require, character.only = TRUE)


## load keyATM models
## load model with dow-jones newswires data set

load("./data/models/dynamic.rds") 
djn_model <- dynamic
rm(dynamic)

djn_model$keywords_raw

## load model with WSJ only data set

load("./data/models/dynamic_w.rds") 
wsj_model <- dynamic_w
rm(dynamic_w)

wsj_model$keywords_raw

## print words with highest probability of selection for both models

djn_words <- top_words(djn_model, 20)
head(djn_words, 20)
print(xtable(djn_words[,1:14]), type = "latex", file = "./text/tables/words_djn.txt")


## create word clouds
library(ggwordcloud)

cloud_words <- top_words(djn_model, 50)


for (i in 1:14){
  terms <- data.frame(words = cloud_words[[i]], freq = 50:1)
  
  terms$words <- str_remove(terms$words, "\\[[1-9]\\]")
  terms$words <- str_remove(terms$words, "\\[[1-9][1-9]\\]")
  terms$words <- str_remove(terms$words, "[✓]")
  terms$words <- str_remove(terms$words, "\\[\\]")
  terms$words <- str_remove(terms$words, "^@[a-z][A-Z]")
  terms
  
  graph <- ggplot(terms, aes(label = words, size = freq)) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 50) +
    theme_minimal()
  
  postscript(paste0("./text/figures/wordcloud", i, ".eps"), width = 1800, height = 1200)

  plot(graph)
  
  dev.off()
  
}


## save the top 20 words for wsj model

wsj_words <- top_words(wsj_model,20)



## create new data frame to compare words

words <- djn_words %>%
  dplyr::select(c(1:14)) %>%
  dplyr::rename(djn_energy = "1_energy") %>%
  dplyr::rename(djn_war = "2_war") %>%
  dplyr::rename(djn_pandemic = "3_pandemic") %>%
  dplyr::rename(djn_labor = "4_labor_shortage") %>%
  dplyr::rename(djn_supplychains = "5_supply_chain") %>%
  dplyr::rename(djn_monetarypolicy = "6_monetary_policy") %>%
  dplyr::rename(djn_governmentspending = "7_government_spending") %>%
  dplyr::rename(djn_demand = "8_demand") %>%
  dplyr::rename(djn_demandshift = "9_demand_shift") %>%
  dplyr::rename(djn_profits = "10_profits") %>%
  dplyr::rename(djn_politics = "11_politics") %>%
  dplyr::rename(djn_debt = "12_debt") %>%
  dplyr::rename(djn_taxes = "13_taxes") %>%
  dplyr::rename(djn_supply = "14_supply") %>%
  bind_cols(., wsj_words[1:14]) %>%
  dplyr::rename(wsj_energy = "1_energy") %>%
  dplyr::rename(wsj_war = "2_war") %>%
  dplyr::rename(wsj_pandemic = "3_pandemic") %>% 
  dplyr::rename(wsj_labor = "4_labor_shortage") %>%
  dplyr::rename(wsj_supplychains = "5_supply_chain") %>%
  dplyr::rename(wsj_monetarypolicy = "6_monetary_policy") %>%
  dplyr::rename(wsj_governmentspending = "7_government_spending") %>%
  dplyr::rename(wsj_demand = "8_demand") %>%
  dplyr::rename(wsj_demandshift = "9_demand_shift") %>%
  dplyr::rename(wsj_profits = "10_profits") %>%
  dplyr::rename(wsj_politics = "11_politics") %>%
  dplyr::rename(wsj_debt = "12_debt") %>%
  dplyr::rename(wsj_taxes = "13_taxes") %>%
  dplyr::rename(wsj_supply = "14_supply") %>%
  dplyr::relocate(wsj_energy, .after = djn_energy) %>%
  dplyr::relocate(wsj_war, .after = djn_war) %>%
  dplyr::relocate(wsj_labor, .after = djn_labor) %>%
  dplyr::relocate(wsj_supplychains, .after = djn_supplychains) %>%
  dplyr::relocate(wsj_monetarypolicy, .after = djn_monetarypolicy) %>%
  dplyr::relocate(wsj_governmentspending, .after = djn_governmentspending) %>%
  dplyr::relocate(wsj_demand, .after = djn_demand) %>%
  dplyr::relocate(wsj_demandshift, .after = djn_demandshift) %>%
  dplyr::relocate(wsj_profits, .after = djn_profits) %>%
  dplyr::relocate(wsj_politics, .after = djn_politics) %>%
  dplyr::relocate(wsj_debt, .after = djn_debt) %>%
  dplyr::relocate(wsj_taxes, .after = djn_taxes) %>%
  dplyr::relocate(wsj_supply, .after = djn_supply) 


print(xtable(words), type = "latex", file = "./text/tables/words.txt")
print(xtable(words), type = "html", file = "./text/tables/words.html")



###  Create some model figures #####

result_figs <- function(model){

# model fit

fig_modelfit <- plot_modelfit(model)
string <- deparse(substitute(model))
save_fig(fig_modelfit, file = paste0("./text/figures/modelfit_", string, ".eps"), width = 15, height = 15)


# latent states

p_alpha <- plot_alpha(model)
save_fig(p_alpha, file = paste0("./text/figures/alpha_", string, ".eps"), width = 15, height = 15)



# probability of words drawn from keyword topic-word distribution

p_pi <- plot_pi(model)
save_fig(p_pi, file = paste0("./text/figures/pi_", string, ".eps"))

# topic appearance likelihood in corpus

topicprop <-plot_topicprop(model, show_topic = 1:14)
save_fig(topicprop, file = paste0("./text/figures/topicprob_", string, ".eps"), width = 15, height = 15)

}

result_figs(djn_model)
result_figs(wsj_model)




### Create time series ####

dynamic_object <- function(model){

if (deparse(substitute(model)) == "djn_model"){
  load(file="./data/datatable/dj_news.rds")
  data <- cbind(dj_news, model$theta)
  

} else {
  load(file="./data/datatable/dj_news_w.rds")
  data <- cbind(dj_news_w, model$theta)
  

}

  data  <- data  %>%
    dplyr::select(-c(docdate, seq, "news-source", headline, text, subject, day, time, hour))

 
  ## create topic-sentiment index
  data <- data %>%
    dplyr::rename_with(~str_replace(., "\\d{1,2}_", ""))
  

  ## calculate the proportions without non-keyword topics
  
  data <- data %>%
    dplyr::mutate(sum = rowSums(data[, c("energy", "profits", "politics", "debt",
                                                "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                                "government_spending", "demand", "demand_shift", "supply") ])) %>%
    dplyr::mutate(across(all_of(c("energy", "profits", "politics", "debt",
                                  "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                  "government_spending", "demand", "demand_shift", "supply" ) ), ~ ./sum))
  
  return(data)

}

## save data  

djn_data_raw <- dynamic_object(djn_model)
wsj_data_raw <- dynamic_object(wsj_model)


save(djn_data_raw, file = "./data/localprojections/djn_data_raw.rds")
save(wsj_data_raw, file = "./data/localprojections/wsj_data_raw.rds")



## create mean of data for first analysis

djn_data <- djn_data_raw %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(across(all_of(c("energy", "profits", "politics", "debt",
                                   "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                   "government_spending", "demand", "demand_shift", "supply")), mean, na.rm = TRUE), .groups="drop") %>%
  dplyr::mutate(date = make_date(year, month)) %>%
  dplyr::select(-c(year, month))

wsj_data <- wsj_data_raw %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(across(all_of(c("energy", "profits", "politics", "debt",
                                   "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                   "government_spending", "demand", "demand_shift", "supply")), mean, na.rm = TRUE), .groups="drop") %>%
  dplyr::mutate(date = make_date(year, month)) %>%
  dplyr::select(-c(year, month))

save(djn_data, file = "./data/localprojections/djn_data.rds")
save(wsj_data, file = "./data/localprojections/wsj_data.rds")


load("./data/models/vars_period.rds")
plot_timetrend(djn_model, time_index_label = vars_period$Year, xlab = "Year")


## prepare data set for plots

djn_data <- djn_data %>%
  mutate(dataset = "djn") %>%
  tidyr::pivot_longer(c(1:14), names_to = "topic", values_to = "proportion")

wsj_data <- wsj_data %>%
  mutate(dataset = "wsj") %>%
  tidyr::pivot_longer(c(1:14), names_to = "topic", values_to = "proportion")

vis_data <- rbind(djn_data, wsj_data)


# Plot the time series of the different topics


custom_order <- c("government_spending", "monetary_policy", "demand_shift", "demand", 
                  "supply_chain", "energy", "labor_shortage", "supply", "pandemic", "politics",
                  "war", "debt", "taxes", "profits") 

# Reorder the 'topic' factor variable based on custom_order

djn_data$topic <- factor(djn_data$topic, levels = custom_order)
# Reorder the 'topic' factor variable based on custom_order wsj
wsj_data$topic <- factor(wsj_data$topic, levels = custom_order)



djn_data <- djn_data %>%
  mutate(names = ifelse(topic == "government_spending", "Government Spending", NA)) %>%
  mutate(names = ifelse(topic == "monetary_policy", "Monetary Policy", names)) %>%
  mutate(names = ifelse(topic == "demand", "Demand", names)) %>%
  mutate(names = ifelse(topic == "demand_shift", "Demand Shift", names)) %>%
  mutate(names = ifelse(topic == "supply_chain", "Supply Chain", names)) %>%
  mutate(names = ifelse(topic == "energy", "Energy", names)) %>%
  mutate(names = ifelse(topic == "labor_shortage", "Labor Shortage", names)) %>%
  mutate(names = ifelse(topic == "supply", "Supply", names)) %>%
  mutate(names = ifelse(topic == "pandemic", "Pandemic", names)) %>%
  mutate(names = ifelse(topic == "politics", "Politics", names)) %>%
  mutate(names = ifelse(topic == "war", "War", names)) %>%
  mutate(names = ifelse(topic == "debt", "Government Debt", names)) %>%
  mutate(names = ifelse(topic == "taxes", "Taxes", names)) %>%
  mutate(names = ifelse(topic == "profits", "Profits", names))
  

wsj_data <- wsj_data %>%
  mutate(names = ifelse(topic == "government_spending", "Government Spending", NA)) %>%
  mutate(names = ifelse(topic == "monetary_policy", "Monetary Policy", names)) %>%
  mutate(names = ifelse(topic == "demand", "Demand", names)) %>%
  mutate(names = ifelse(topic == "demand_shift", "Demand Shift", names)) %>%
  mutate(names = ifelse(topic == "supply_chain", "Supply Chain", names)) %>%
  mutate(names = ifelse(topic == "energy", "Energy", names)) %>%
  mutate(names = ifelse(topic == "labor_shortage", "Labor Shortage", names)) %>%
  mutate(names = ifelse(topic == "supply", "Supply", names)) %>%
  mutate(names = ifelse(topic == "pandemic", "Pandemic", names)) %>%
  mutate(names = ifelse(topic == "politics", "Politics", names)) %>%
  mutate(names = ifelse(topic == "war", "War", names)) %>%
  mutate(names = ifelse(topic == "debt", "Government Debt", names)) %>%
  mutate(names = ifelse(topic == "taxes", "Taxes", names)) %>%
  mutate(names = ifelse(topic == "profits", "Profits", names))


# basic stream graph
# Define the number of colors in your custom palette
num_colors <- 14

# Create a custom pastel-like palette
start_color <- rgb(94, 79, 162, maxColorValue = 255)
second_color <- rgb(92,183,170, maxColorValue = 255)
third_color <- rgb(232,246,155, maxColorValue = 255)
fourth_color <- rgb(253,183,104, maxColorValue = 255)
end_color <- rgb(158, 1, 66, maxColorValue = 255)



# Generate the custom color palette
custom_palette <- colorRampPalette(c(start_color, second_color, third_color, fourth_color, end_color))
colors <- custom_palette(num_colors)


# create plot of all topics

plot_narr <- ggplot(djn_data, aes(x = date, y = proportion, fill = topic)) +
  geom_stream(color = 1, lwd = 0.25, type = "proportional") +
  geom_stream_label(aes(label = names, fontface = "bold"), 
                    type = "proportional",
                    size = 4) +
  labs(x = "Year",
       y = "Smoothed Proportions",
       fill = "Topics") +
  scale_fill_manual(values = colors) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 15))

ggsave("./text/figures/narratives_all.eps",
       plot = plot_narr,
       device = cairo_ps,   # recommended for EPS
       width = 15,
       height = 9)


## Mean Proportions before and after pandemic outbreak #####
## Only DJN-Data
djn_data <- djn_data %>% 
  mutate(period = ifelse(date <= as.Date("2020-12-01"), "before 2021", "since 2021")) %>%
  mutate(group = ifelse(topic %in% c("government_spending", "monetary_policy",
                                   "demand", "demand_shift"), "demand", NA )) %>%
  mutate(group = ifelse(topic %in% c("supply_chain", "energy", "labor_shortage", "supply"), "supply", group)) %>%
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


custom_order <- c("Government Spending", "Monetary Policy", "Demand Shift", "Demand", 
                  "Supply Chain", "Energy", "Labor Shortage", "Supply", "Pandemic", "Politics",
                  "War", "Government Debt", "Taxes", "Profits") 
djn_change$names <- factor(djn_change$names, levels = custom_order)

plot_change <- ggplot(djn_change, aes(x = names, y = proportion*100, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = proportion*100 - ic*100, ymax = proportion*100 + ic*100),
                position = position_dodge(width = 0.7), width = 0.25, colour = "orange", size = 1.3) +
  labs(x = "Topics", y = "Mean Proportions") +
  facet_grid(cols = vars(group), scales = "free_x") +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(size = 15), 
    plot.title = element_text(size = 20), 
    plot.subtitle = element_text(size=15), 
    legend.text = element_text(size = 15), 
    legend.title = element_text(size = 15), 
    axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=0.5), 
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


postscript("./text/figures/plot_change.eps", width = 1500, height = 1000)
plot_change
dev.off()



## Compare data from djn with wsj

postscript("./text/figures/comparision.eps", width = 1500, height = 1000)


ggplot(vis_data, aes(x = date, y = proportion, color = dataset, group = interaction(topic, dataset))) +
  geom_line() +
  facet_wrap(~topic, scales = "free_y", ncol = 2) +
  labs(
    x = "Date",
    y = "Proportion"
  ) +
  theme_bw()

dev.off()

