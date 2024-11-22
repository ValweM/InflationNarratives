## This is a script to run diagnostic tests and trend-cycle filter
## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL = "en_US.UTF-8")
rm(list = ls())
gc()


## load required packages

mypackages <- c("dplyr", "vars", "tseries", "forecast",
                "lubridate", "ggplot2", "tidyr", "keyATM", "readxl", "psych",
                "car", "ggpubr", "gridExtra", "bHP")

lapply(mypackages, require, character.only = TRUE)

source("./code/functions.R")


### Short Term Expectations

exp <- read_xlsx("./data/localprojections/infl_exp_1y_3y.xlsx") # Source: https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY-SCE-Data.xlsx?sc_lang=en

exp <- exp %>%
  mutate(date = ym(date))


# create plot

colors <- c("Median (1 year ahead)" = "blue", "Median Low (1 year ahead)" = "red",
            "Median Mid (1 year ahead)" = "green",  "Median High (1 year ahead)" = "yellow")

# save as eps file
postscript("./text/figures/infl_expect.eps")
ggplot(exp, aes(x=date,)) +
  geom_line(aes(y=median_1y, color = "Median (1 year ahead)"), linewidth =1.1)+
  #geom_line(aes(y=median_3y, color = "Median (3 years ahead)"), linewidth =1.1)+
  geom_line(aes(y=loweduc_1y, color = "Median Low (1 year ahead)"), linewidth =1.1)+
  geom_line(aes(y=mideduc_1y, color = "Median Mid (1 year ahead)"), linewidth =1.1)+
  geom_line(aes(y=higheduc_1y, color = "Median High (1 year ahead)"), linewidth =1.1)+
  labs(x = "Time",
       y = "(%)",
       color = "Legend") +
  scale_color_manual(values = colors)+
  theme_light()

dev.off()


# rename columns and set time window 

exp <- exp %>%
  as.data.frame() %>%
  mutate(date = as.Date(date)) %>%
  dplyr::rename(exp1y = median_1y) %>%
  dplyr::rename(exp3y = median_3y) %>%
  dplyr::rename(exp1y_loweduc = loweduc_1y) %>%
  dplyr::rename(exp1y_mideduc = mideduc_1y) %>%
  dplyr::rename(exp1y_higheduc = higheduc_1y) %>%
  dplyr::rename(exp3y_loweduc = loweduc_3y) %>%
  dplyr::rename(exp3y_mideduc = mideduc_3y) %>%
  dplyr::rename(exp3y_higheduc = higheduc_3y) %>%
  dplyr::rename(exp1y_lowinc = lowinc_1y) %>%
  dplyr::rename(exp1y_midinc = midinc_1y) %>%
  dplyr::rename(exp1y_highinc = highinc_1y) %>%
  dplyr::rename(exp3y_lowinc = lowinc_3y) %>%
  dplyr::rename(exp3y_midinc = midinc_3y) %>%
  dplyr::rename(exp3y_highinc = highinc_3y) %>%
  dplyr::rename(exp1y_lowage = lowage_1y) %>%
  dplyr::rename(exp1y_midage = midage_1y) %>%
  dplyr::rename(exp1y_highage = highage_1y) %>%
  dplyr::rename(exp3y_lowage = lowage_3y) %>%
  dplyr::rename(exp3y_midage = midage_3y) %>%
  dplyr::rename(exp3y_highage = highage_3y) %>%
  dplyr::rename(exp1y_lownumeracy = lownumeracy_1y) %>%
  dplyr::rename(exp1y_highnumeracy = highnumeracy_1y) %>%
  dplyr::rename(exp3y_lownumeracy = lownumeracy_3y) %>%
  dplyr::rename(exp3y_highnumeracy = highnumeracy_3y) %>%
  dplyr::rename(exp1y_west = west_1y) %>%
  dplyr::rename(exp1y_midwest = midwest_1y) %>%
  dplyr::rename(exp1y_south = south_1y) %>%
  dplyr::rename(exp1y_northeast = northeast_1y) %>%
  dplyr::rename(exp3y_west = west_3y) %>%
  dplyr::rename(exp3y_midwest = midwest_3y) %>%
  dplyr::rename(exp3y_south = south_3y) %>%
  dplyr::rename(exp3y_northeast = northeast_3y) %>%
  filter(date <= as.Date("2023-01-01") & date >= ("2018-01-01")) %>%
  dplyr::select(-c(date))


# Create Time Series

exp1y_ts <- ts(exp$exp1y, frequency = 12, start = c(2018, 01))
exp3y_ts <- ts(exp$exp3y, frequency = 12, start = c(2018, 01))
exp1y_loweduc_ts <- ts(exp$exp1y_loweduc, frequency = 12, start = c(2018, 01))
exp1y_mideduc_ts <- ts(exp$exp1y_mideduc, frequency = 12, start = c(2018, 01))
exp1y_higheduc_ts <- ts(exp$exp1y_higheduc, frequency = 12, start = c(2018, 01))
exp3y_loweduc_ts <- ts(exp$exp3y_loweduc, frequency = 12, start = c(2018, 01))
exp3y_mideduc_ts <- ts(exp$exp3y_mideduc, frequency = 12, start = c(2018, 01))
exp3y_higheduc_ts <- ts(exp$exp3y_higheduc, frequency = 12, start = c(2018, 01))
exp1y_lowinc_ts <- ts(exp$exp1y_lowinc, frequency = 12, start = c(2018, 01))
exp1y_midinc_ts <- ts(exp$exp1y_midinc, frequency = 12, start = c(2018, 01))
exp1y_highinc_ts <- ts(exp$exp1y_highinc, frequency = 12, start = c(2018, 01))
exp3y_lowinc_ts <- ts(exp$exp3y_lowinc, frequency = 12, start = c(2018, 01))
exp3y_midinc_ts <- ts(exp$exp3y_midinc, frequency = 12, start = c(2018, 01))
exp3y_highinc_ts <- ts(exp$exp3y_highinc, frequency = 12, start = c(2018, 01))
exp1y_lowage_ts <- ts(exp$exp1y_lowage, frequency = 12, start = c(2018, 01))
exp1y_midage_ts <- ts(exp$exp1y_midage, frequency = 12, start = c(2018, 01))
exp1y_highage_ts <- ts(exp$exp1y_highage, frequency = 12, start = c(2018, 01))
exp3y_lowage_ts <- ts(exp$exp3y_lowage, frequency = 12, start = c(2018, 01))
exp3y_midage_ts <- ts(exp$exp3y_midage, frequency = 12, start = c(2018, 01))
exp3y_highage_ts <- ts(exp$exp3y_highage, frequency = 12, start = c(2018, 01))
exp1y_lownumeracy_ts <- ts(exp$exp1y_lownumeracy, frequency = 12, start = c(2018, 01))
exp3y_lownumeracy_ts <- ts(exp$exp3y_lownumeracy, frequency = 12, start = c(2018, 01))
exp1y_highnumeracy_ts <- ts(exp$exp1y_highnumeracy, frequency = 12, start = c(2018, 01))
exp3y_highnumeracy_ts <- ts(exp$exp3y_highnumeracy, frequency = 12, start = c(2018, 01))
exp3y_west_ts <- ts(exp$exp3y_west, frequency = 12, start = c(2018, 01))
exp3y_south_ts <- ts(exp$exp3y_south, frequency = 12, start = c(2018, 01))
exp3y_northeast_ts <- ts(exp$exp3y_northeast, frequency = 12, start = c(2018, 01))
exp3y_midwest_ts <- ts(exp$exp3y_midwest, frequency = 12, start = c(2018, 01))
exp1y_west_ts <- ts(exp$exp1y_west, frequency = 12, start = c(2018, 01))
exp1y_south_ts <- ts(exp$exp1y_south, frequency = 12, start = c(2018, 01))
exp1y_northeast_ts <- ts(exp$exp1y_northeast, frequency = 12, start = c(2018, 01))
exp1y_midwest_ts <- ts(exp$exp1y_midwest, frequency = 12, start = c(2018, 01))


## Check for outliers

tsoutliers(exp1y_ts)
tsoutliers(exp3y_ts)


# Test for stationarity https://jtr13.github.io/cc21fall2/urca-unit-root-test-and-cointegration-test.html

ers.exp1 <- ur.ers(exp1y_ts, type="P-test", model="const", lag.max = 6)
summary(ers.exp1) # non stationary


ers.exp3 <- ur.ers(exp3y_ts, type="P-test", model="const", lag.max = 6)
summary(ers.exp3) # non stationary

describe(exp1y_ts)
describe(exp3y_ts)



## Continue with CPI Inflation
##Consumer Price Index: Total All items for the United States
inf <- read.csv("./data/localprojections/CPIAUCSL.csv")

inf <- inf %>%
  mutate(DATE = as.Date(DATE)) %>%
  arrange(DATE) %>%
  filter(DATE <= as.Date("2023-01-01") & DATE >= as.Date("2017-12-01")) %>%
  dplyr::rename(cpi_growth = CPIAUCSL) %>%
  dplyr::select(cpi_growth)


# create time series

inf_ts <- ts(inf, frequency = 12, start = c(2017, 12))


# seasonally adjusted at annual rates

inf_dlog<- ts((1 + diff(log(inf_ts)))^12-1, start = c(2018, 1), frequency = 12)


# Outliers

tsoutliers(inf_dlog)


# Stationarity

ers.inf <- ur.ers(inf_dlog, type="P-test", model="const", lag.max = 8)
summary(ers.inf) # stationary

describe(inf_dlog)



## coincident economic acitivity https://kevinkotze.github.io/ts-5-tut/

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


# Outliers

tsoutliers(econ_dlog)


# Dummy to control outliers

dummy <- integer(61)
dummy[c(27, 28, 29, 30, 32, 34)] <- 1
dummy


# Stationarity

ers.econ <- ur.ers(econ_dlog, type="P-test", model="const", lag.max = 8)
summary(ers.econ) # stationary




## load file from previous script

load(file = "./data/localprojections/sdjn_data.rds")
str(sdjn_data)


load(file = "./data/localprojections/swsj_data.rds")
str(sdjn_data)


### Create Data Frame with Level Data ###########

djnlevel <- data.frame(economic_activity = as.matrix(econ_dlog),
                       inflation_expectations_1y = as.matrix(exp1y_ts),
                       inflation_expectations_3y = as.matrix(exp3y_ts),
                       exp_loweduc_1y = as.matrix(exp1y_loweduc_ts),
                       exp_mideduc_1y = as.matrix(exp1y_mideduc_ts),
                       exp_higheduc_1y = as.matrix(exp1y_higheduc_ts),
                       exp_loweduc_3y = as.matrix(exp3y_loweduc_ts),
                       exp_mideduc_3y = as.matrix(exp3y_mideduc_ts),
                       exp_higheduc_3y = as.matrix(exp3y_higheduc_ts),
                       exp_lowinc_1y = as.matrix(exp1y_lowinc_ts),
                       exp_midinc_1y = as.matrix(exp1y_midinc_ts),
                       exp_highinc_1y = as.matrix(exp1y_highinc_ts),
                       exp_lowinc_3y = as.matrix(exp3y_lowinc_ts),
                       exp_midinc_3y = as.matrix(exp3y_midinc_ts),
                       exp_highinc_3y = as.matrix(exp3y_highinc_ts),
                       exp_lowage_1y = as.matrix(exp1y_lowage_ts),
                       exp_midage_1y = as.matrix(exp1y_midage_ts),
                       exp_highage_1y = as.matrix(exp1y_highage_ts),
                       exp_lowage_3y = as.matrix(exp3y_lowage_ts),
                       exp_midage_3y = as.matrix(exp3y_midage_ts),
                       exp_highage_3y = as.matrix(exp3y_highage_ts),
                       exp_lownumeracy_1y = as.matrix(exp1y_lownumeracy_ts),
                       exp_lownumeracy_3y = as.matrix(exp3y_lownumeracy_ts),
                       exp_highnumeracy_1y = as.matrix(exp1y_highnumeracy_ts),
                       exp_highnumeracy_3y = as.matrix(exp3y_lownumeracy_ts),
                       exp_west_1y = as.matrix(exp1y_west_ts),
                       exp_west_3y = as.matrix(exp3y_west_ts),
                       exp_midwest_1y = as.matrix(exp1y_midwest_ts),
                       exp_midwest_3y = as.matrix(exp3y_midwest_ts),
                       exp_south_1y = as.matrix(exp1y_south_ts),
                       exp_south_3y = as.matrix(exp3y_south_ts),
                       exp_northeast_1y = as.matrix(exp1y_northeast_ts),
                       exp_northeast_3y = as.matrix(exp3y_northeast_ts),
                       inflation = as.matrix(inf_dlog),
                       government_spending = as.matrix(sdjn_data$government_spending),
                       monetary_policy = as.matrix(sdjn_data$monetary_policy),
                       demand = as.matrix(sdjn_data$demand),
                       demand_shift = as.matrix(sdjn_data$demand_shift),
                       politics = as.matrix(sdjn_data$politics),
                       debt = as.matrix(sdjn_data$debt),
                       taxes = as.matrix(sdjn_data$taxes),
                       profits = as.matrix(sdjn_data$profits),
                       war = as.matrix(sdjn_data$war),
                       labor_shortage = as.matrix(sdjn_data$labor_shortage),
                       supply_chain = as.matrix(sdjn_data$supply_chain),
                       supply = as.matrix(sdjn_data$supply),
                       energy = as.matrix(sdjn_data$energy),
                       pandemic = as.matrix(sdjn_data$pandemic),
                       dummy = as.matrix(dummy)) %>%
  dplyr::rename(economic_activity = econ_ac) %>%
  dplyr::rename(inflation = cpi_growth)



# Define the topic names and their corresponding table names
topic_names <- c("government_spending", "monetary_policy", "demand", "demand_shift",
                 "supply_chain", "energy", "supply", "labor_shortage", "pandemic", "politics",
                 "war", "debt", "taxes", "profits")
topic_table <- c("Government Spending", "Monetary Policy", "Demand", "Demand Shift",
                 "Supply Chain", "Energy", "Supply", "Labor Shortage", "Pandemic", "Politics",
                 "War", "Debt", "Taxes", "Profits")

# Perform stationary tests
tests <- list(
  government_spending = ur.ers(djnlevel$government_spending, type="P-test", model="const", lag.max = 6),
  monetary_policy = ur.ers(djnlevel$monetary_policy, type="P-test", model="const", lag.max = 6),
  demand = ur.ers(djnlevel$demand, type="P-test", model="const", lag.max = 6),
  demand_shift = ur.ers(djnlevel$demand_shift, type="P-test", model="const", lag.max = 6),
  supply_chain = ur.ers(djnlevel$supply_chain, type="P-test", model="const", lag.max = 6),
  energy = ur.ers(djnlevel$energy, type="P-test", model="const", lag.max = 6),
  labor_shortage = ur.ers(djnlevel$labor_shortage, type="P-test", model="const", lag.max = 6),
  supply = ur.ers(djnlevel$supply, type="P-test", model="const", lag.max = 6),
  pandemic = ur.ers(djnlevel$pandemic, type="P-test", model="const", lag.max = 6),
  politics = ur.ers(djnlevel$politics, type="P-test", model="const", lag.max = 6),
  war = ur.ers(djnlevel$war, type="P-test", model="const", lag.max = 6),
  debt = ur.ers(djnlevel$debt, type="P-test", model="const", lag.max = 6),
  taxes = ur.ers(djnlevel$taxes, type="P-test", model="const", lag.max = 6),
  profits = ur.ers(djnlevel$profits, type="P-test", model="const", lag.max = 6)
)



# Extract test statistics and critical values
results <- do.call(rbind, lapply(names(tests), function(name) {
  test <- tests[[name]]
  data.frame(
    Variable = topic_table[which(topic_names == name)],
    Statistic = round(test@teststat,2),
    Critical_Values_1pct = round(test@cval[1],2),
    Critical_Values_5pct = round(test@cval[2],2),
    Critical_Values_10pct = round(test@cval[3],2)
  )
}))

# Generate LaTeX table as sideways table
generate_latex_table <- function(data, caption) {
  latex_table <- paste0("\\begin{sidewaystable}[H]\n\\centering\n\\caption{", caption, "}\\label{table:ers}\n\n\\begin{tabular}{lcccc}\n\\toprule\n\\textbf{Variable} & \\textbf{Statistic} & \\textbf{Critical Value 1\\%} & \\textbf{Critical Value 5\\%} & \\textbf{Critical Value 10\\%} \\\\\n\\midrule\n")
  
  for (i in 1:nrow(data)) {
    latex_table <- paste0(latex_table, data$Variable[i], " & ", data$Statistic[i], " & ", data$Critical_Values_1pct[i], " & ", data$Critical_Values_5pct[i], " & ", data$Critical_Values_10pct[i], " \\\\\n")
  }
  
  latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{sidewaystable}")
  
  return(latex_table)
}


# LaTeX table with a custom caption
custom_caption <- "Elliott, Rothenberg and Stock unit root test results"
latex_table <- generate_latex_table(results, custom_caption)
write(latex_table, file = "./text/tables/ers_results.tex")



## boosted HP-Filter 

bHP_list <- lapply(djnlevel[1:48], BoostedHP, lambda = 129600, Max_Iter = 200, stopping = "adf") #129 600 #104035

djnbHP <- data.frame(index = seq(1:61))
for (i in 1:48){
  djnbHP <- cbind(djnbHP, bHP_list[[i]]$cycle)
}

djnbHP <- bind_cols(djnbHP, djnlevel$dummy[1:61],) %>%
  dplyr::select(-index)

colnames(djnbHP) <- colnames(djnlevel)


## Differences 

diff(djnlevel$economic_activity)
diff_list <- lapply(djnlevel[1:48], diff)

djndiff <- data.frame(index = seq(1:60)) #pandemic, war, debt and government spending
for (i in 1:48){
  djndiff <- cbind(djndiff, diff_list[[i]])
}

djndiff <- bind_cols(djndiff, djnlevel$dummy[2:61],) %>%
  dplyr::select(-index)

colnames(djndiff) <- colnames(djnlevel)

djndiff$economic_activity <- djnlevel$economic_activity[2:61]
djndiff$inflation <- djnlevel$inflation[2:61]


## Save Data

save(djnlevel, file = "./data/localprojections/djnlevel.rds")
save(djnbHP, file = "./data/localprojections/djnbHP.rds")
save(djndiff, file = "./data/localprojections/djndiff.rds")

