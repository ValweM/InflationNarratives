rm(list=ls())
library("fredr")
library("dplyr")
library("xts")
library("ggplot2")
library("readxl")
library("lubridate")
library("fredr")
fredr_set_key("96626d99ddede99ac9f0874044dbab97")

cpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2015-01-01"),
  observation_end = as.Date("2023-03-01")
)

cpi <- cpi %>%
  #mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  dplyr::rename(cpi = value) %>%
  dplyr::select(cpi)

cpi_ts <- ts(cpi, frequency = 12, start = c(2015, 1))
cpi_dlog<- ts((1 + diff(log(cpi_ts)))^12-1, start = c(2015, 2), frequency = 12)
# in fact we use annualized rates for presentation


activity <- fredr(
  series_id = "USPHCI",
  observation_start = as.Date("2015-01-01"),
  observation_end = as.Date("2023-03-01")
)

activity <- activity %>%
  #mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  dplyr::rename(econ = value) %>%
  dplyr::select(econ)

econ_ts <- ts(activity, frequency = 12, start = c(2015, 1))
econ_dlog<- ts((1 + diff(log(econ_ts)))^12-1, start = c(2015, 2), frequency = 12)
# in fact we use annualized rates for presentation

expectations <- read_xlsx("./data/localprojections/infl_exp_1y_3y.xlsx")


expectation1y <- expectations %>%
  filter(date >= as.Date("2015-01-01")) %>%
  select(median_1y)

expectation3y <- expectations %>%
  filter(date >= as.Date("2015-01-01")) %>%
  select(median_3y)
  
  
 
expect1y_ts <- ts(expectation1y, frequency = 12, start = c(2015, 1))
expect3y_ts <- ts(expectation3y, frequency = 12, start = c(2015, 1))

#ts.intersect necessary due to lag (loss of one data point)
ts_all <- ts.intersect(cpi_dlog,econ_dlog,expect1y_ts, expect3y_ts)
ts_all_x <- as.xts(ts_all)

test <- data.frame(date=index(ts_all_x), coredata(ts_all_x))

# Create a custom pastel-like palette
start_color <- rgb(94, 79, 162, maxColorValue = 255)
second_color <- rgb(92,183,170, maxColorValue = 255)
third_color <- rgb(232,246,155, maxColorValue = 255)
fourth_color <- rgb(253,183,104, maxColorValue = 255)
end_color <- rgb(158, 1, 66, maxColorValue = 255)


# Generate the custom color palette
custom_palette <- colorRampPalette(c(start_color, second_color, third_color, fourth_color, end_color))
num_colors <- 4
colors <- custom_palette(num_colors)


colors <- c("Inflation" = colors[1], "Activity" = colors[2], "Expectation 1y" = colors[3],
            "Expectation 3y" = colors[4])

scale = 3

ggsave("./text/figures/all_data.eps", device = cairo_ps, width = 15, height = 6)
ggplot(test, aes(x=date,)) +
  geom_line(aes(y=cpi_dlog*100, color = "Inflation"), linewidth =2)+
  geom_line(aes(y=econ_dlog*100/scale, color = "Activity"), linewidth =2)+
  geom_line(aes(y=expect1y_ts, color = "Expectation 1y"), linewidth =2)+
  geom_line(aes(y=expect3y_ts, color = "Expectation 3y"), linewidth =2)+
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Activity (%)")) +
  labs(x = "Time",
       y = "(%)",
       color = "Legend") + 
  theme_bw() +
  scale_color_manual(values = colors) +
  theme(
    text = element_text(size = 20),
    legend.position = c(0.9, 0.15)
  )

dev.off()
