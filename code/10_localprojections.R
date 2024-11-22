# This is a script to estimate local projections
## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL = "en_US.UTF-8")
rm(list = ls())
gc()


## load required packages

mypackages <- c("dplyr", "vars", "tseries",
 "forecast", "lpirfs", "lubridate",
 "ggplot2",  "hrbrthemes",
 "tidyr", "keyATM", "tfplot", "mFilter", "readxl",
 "car")

lapply(mypackages, require, character.only = TRUE)

source("./code/functions.R")

library(ggpubr)
library(gridExtra)

load(file = "./data/localprojections/sdjn_data.rds")
load(file = "./data/localprojections/djnlevel.rds")
load(file = "./data/localprojections/djndiff.rds")
load(file = "./data/localprojections/djnbHP.rds")



## function to estimate local projections and to save figures and output

localprojections <- function(topic, data, type, group) {
  
  if (type == "baseline" ){
    
    sink(file = paste0("./text/output/lp/", type, "/", data, "/",
                       topic, "/djn_output_", topic, ".txt"))
    
    print(paste(c(topic, "djn level estimation")))
    
    data_source <- switch(data,
                          "level" = djnlevel,
                          "diff" = djndiff,
                          "hp" = djnhp, 
                          "bHP" = djnbHP)
    
    data_djn <- data_source[c(topic,
                           "inflation_expectations_1y", "inflation_expectations_3y",
                           "inflation", "economic_activity", "dummy")]
    
  } else if (type == "income"){
    
    sink(file = paste0("./text/output/lp/", type, "/", group, "/", data, "/",
                       topic, "/djn_output_", topic, ".txt"))
    
    print(paste(c(topic, "djn level estimation")))
    
    data_source <- switch(data,
                          "level" = djnlevel,
                          "diff" = djndiff,
                          "hp" = djnhp)
    
    data_djn <- data_source[c(topic, "exp_lowinc_1y",
                           "exp_lowinc_3y",
                           "inflation", "economic_activity", "dummy")]
    
    data_djn <- data_djn %>%
      dplyr::rename(inflation_expectations_1y = exp_lowinc_1y, 
                    inflation_expectations_3y = exp_lowinc_3y)
    
  
  }
  
  # select optimal number of lags
  tsdata_djn <- ts(data_djn, start = c(2018, 2), freq = 12)
  month <- seasonaldummy(tsdata_djn[, topic])
  data_djn <- cbind(data_djn, month)

  # estimate local projections
  results_lin <- lp_lin(endog_data = data_djn[1:5],
                        lags_endog_lin = NaN,
                        lags_criterion = "BIC",
                        max_lags = 4, 
                        contemp_data = c(data_djn["dummy"], data_djn[, 7:16]) ,
                        trend          = 0,
                        shock_type     = 1,
                        confint        = 1.65,
                        hor            = 12)
  
  
  print(results_lin)
  plots <- myplot(results_lin)
  plot(results_lin)

  # Using ggsave to save plots as EPS files
  if (group == "NONE") {
    ggsave(plots[[1]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", topic, "on", topic, ".eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[2]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/expectations1y_on", topic, ".eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[3]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/expectations3yon", topic, "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[4]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "inflation", "on", topic, ".eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[5]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "econac", "on", topic, "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[6]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", topic, "onexpectations1y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[7]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/expectations1yonexpectations1y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[8]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "expectations3yonexpectations1y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[9]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "inflationonexpectations1y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[10]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/econacon", "inflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[11]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", topic, "onexpectations3y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[12]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "expectations1yonexpectations3y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[13]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "expectations3yonexpectations3y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[14]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "inflationyonexpectations3y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[15]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "econaconexpectations3y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[16]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", topic, "oninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[17]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "expectations1yoninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[18]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "expectations3yoninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[19]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "inflationoninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[20]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "econaconinflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[21]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", topic, "oneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[22]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "expectations1yoneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[23]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "expectations3yoneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[24]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "inflationoneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[25]], file = paste0("./text/output/lp/", type, "/", data, "/", topic, "/", "econaconeconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    
  } else {
    
    ggsave(plots[[1]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", topic, "on", topic, ".eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[2]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/expectations1y_on", topic, ".eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[3]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/expectations3yon", topic, "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[4]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "inflation", "on", topic, ".eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[5]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "econac", "on", topic, "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[6]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", topic, "onexpectations1y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[7]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/expectations1yonexpectations1y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[8]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "expectations3yonexpectations1y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[9]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "inflationonexpectations1y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[10]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/econacon", "inflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[11]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", topic, "onexpectations3y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[12]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "expectations1yonexpectations3y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[13]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "expectations3yonexpectations3y_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[14]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "inflationyonexpectations3y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[15]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "econaconexpectations3y", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[16]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", topic, "oninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[17]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "expectations1yoninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[18]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "expectations3yoninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[19]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "inflationoninflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[20]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "econaconinflation", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[21]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", topic, "oneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[22]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "expectations1yoneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[23]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "expectations3yoneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[24]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "inflationoneconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
    ggsave(plots[[25]], file = paste0("./text/output/lp/", type, "/", group, "/", data, "/", topic, "/", "econaconeconac", "_djn.eps"), width = 6, height = 5, device = cairo_ps)
  }
  
  
  
  sink(file = NULL)
  
  
}


topic_names <- colnames(sdjn_data[1:14])

for (i in topic_names){
  localprojections(i, "level", "baseline", "NONE")
}


for (i in topic_names){
  localprojections(i, "diff", "baseline", "NONE")
}


for (i in topic_names){
  localprojections(i, "bHP", "baseline","NONE")
}

