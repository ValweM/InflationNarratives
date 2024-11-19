# This is a script to plot and analyse keyATM with LSS

## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL="en_US.UTF-8")
rm(list=ls())
gc()


## load required packages

mypackages <- c("dplyr", "keyATM", "lubridate", "stringr", "data.table",
                "ggplot2", "xtable", "tidyr", "hrbrthemes",
                "plotly", "svglite", "RColorBrewer", "ggstream", "grDevices")

lapply(mypackages, require, character.only = TRUE)

## load data


load(file = "./data/localprojections/djn_data_raw.rds")
load(file = "./data/localprojections/wsj_data_raw.rds")



load(file = "./data/localprojections/sent_dat.rds")

load(file = "./data/localprojections/sent_dat_wsj.rds")



## Combine Sentiment and Topics ######



dat <- dat %>%
  dplyr::select(doc_id, fit)

djn_data <- inner_join(djn_data_raw, dat)


djn_data[, c("energy", "profits", "politics", "debt",
         "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
         "government_spending", "demand", "demand_shift", "supply")] <-   djn_data[, c("energy", "profits", "politics", "debt",
                                                                                        "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                                                                        "government_spending", "demand", "demand_shift", "supply")] * t(djn_data$fit)



sdjn_data <- djn_data %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(across(all_of(c("energy", "profits", "politics", "debt",
                                   "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                   "government_spending", "demand", "demand_shift", "supply")), mean, na.rm = TRUE), .groups="drop") %>%
  dplyr::mutate(date = make_date(year, month)) %>%
  dplyr::select(-c(year, month))



save(sdjn_data, file = "./data/localprojections/sdjn_data.rds")


## now for wsj data

wsj_data <- inner_join(wsj_data_raw, dat)

wsj_data[, c("energy", "profits", "politics", "debt",
             "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
             "government_spending", "demand", "demand_shift", "supply")] <-   wsj_data[, c("energy", "profits", "politics", "debt",
                                                                                         "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                                                                         "government_spending", "demand", "demand_shift", "supply")] * t(wsj_data$fit)
swsj_data <- wsj_data %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(across(all_of(c("energy", "profits", "politics", "debt",
                                   "taxes", "war", "pandemic", "labor_shortage", "supply_chain", "monetary_policy",
                                   "government_spending", "demand", "demand_shift", "supply")), mean, na.rm = TRUE), .groups="drop") %>%
  dplyr::mutate(date = make_date(year, month)) %>%
  dplyr::select(-c(year, month))



save(swsj_data, file = "./data/localprojections/swsj_data.rds")



## plot the time series

sdjn_data <- sdjn_data %>%
  dplyr::mutate(dataset = "djn") %>%
  tidyr::pivot_longer(1:14, names_to = "topic", values_to = "proportion")


swsj_data <- swsj_data %>%
  dplyr::mutate(dataset = "wsj") %>%
  tidyr::pivot_longer(1:14, names_to = "topic", values_to = "proportion")



sdjn_demand <- sdjn_data %>%
  dplyr::filter(topic %in% c("demand_shift", "government_spending", 
                             "monetary_policy", "demand")) 

## Plot for Demand Narratives


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



plot_sdemand <- ggplot(sdjn_demand, aes(x = date, y = 100 * proportion, color = topic)) + 
  geom_line(linewidth = 2) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  theme(
    text = element_text(size = 20),
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
      "monetary_policy" = colors[5],
      "demand" = colors[8],
      "demand_shift" = colors[12]
    ),
    breaks = c("government_spending", "monetary_policy", "demand_shift", "demand"),
    labels = c("Govt. Spending", "Monetary Policy", "Demand Shift", "Demand (residual)")
  )



ggsave("./text/figures/plot_sdemand.eps", plot = plot_sdemand, device = cairo_ps, width = 15, height = 7)



# plot supply narratives


sdjn_supply <- sdjn_data %>%
  dplyr::filter(topic %in% c("supply_chain", "labor_shortage",
                             "energy", "supply")) 



plot_ssupply <- ggplot(sdjn_supply, aes(x = date, y = 100 * proportion, color = topic)) + 
  geom_line(linewidth = 2) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  theme(
    text = element_text(size = 20),
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
      "supply_chain" = colors[1],
      "energy" = colors[5],
      "labor_shortage" = colors[8],
      "supply" = colors[12]
    ),
    breaks = c("supply_chain", "energy", "labor_shortage", "supply"),
    labels = c("Supply Chain", "Energy", "Labor Shortage", "Supply (residual)")
  )

ggsave(filename = "./text/figures/plot_ssupply.eps", plot = plot_ssupply, device = cairo_ps, width = 15, height = 7)



# plot other narratives

sdjn_others <- sdjn_data %>%
  dplyr::filter(topic %in% c("pandemic", "politics",
                             "war", "debt", "taxes", 
                             "profits")) 

plot_sothers <- ggplot(sdjn_others, aes(x = date, y = 100 * proportion, color = topic)) + 
  geom_line(linewidth = 2) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  theme(
    text = element_text(size = 20),
    legend.position = c(0.85, 0.25)
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
      "pandemic" = colors[1],
      "politics" = colors[3],
      "war" = colors[5],
      "debt" = colors[8],
      "taxes" = colors[12],
      "profits" = colors[13]
    ),
    breaks = c("pandemic", "politics", "war", "debt", "taxes", "profits"),
    labels = c("Pandemic", "Politics", "War", "Government Debt", "Taxes", "Profits")
  )

ggsave(filename = "./text/figures/plot_sothers.eps", plot = plot_sothers, device = cairo_ps, width = 15, height = 7)



# plot comparison with wsj data
vis_data <- rbind(sdjn_data, swsj_data)

png(filename = "./text/figures/scomparision.png", width = 1800, height = 1400)

ggplot(vis_data, aes(x = date, y = proportion, color = dataset, group = interaction(topic, dataset))) +
  geom_line() +
  facet_wrap(~topic, scales = "free_y", ncol = 2) +
  labs(
    x = "Date",
    y = "Proportion"
  ) +
  theme_bw()

dev.off()

