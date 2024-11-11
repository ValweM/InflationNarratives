## pre-settings

Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL = "en_US.UTF-8")
rm(list = ls())
gc()


## load required packages
## Procedure as in bruceR granger_causality (vargranger in stata)

mypackages <- c("dplyr", "vars", "tseries",
                "forecast", "lubridate",
                "ggplot2", "tidyr", "readxl", "car",
                "sandwich", "dynlm", "bruceR", "xtable")

lapply(mypackages, require, character.only = TRUE)

# Load data sets
load(file = "./data/localprojections/djnlevel.rds")
load(file = "./data/localprojections/djnbHP.rds")
load(file = "./data/localprojections/djnhp.rds")
load(file = "./data/localprojections/djndiff.rds")

granger_cause <- function(topic, datatype, modeltype, var, group, reverse = FALSE){
  if (modeltype == "djn" & var == "base") {
    data_source <- switch(datatype,
                          "level" = djnlevel,
                          "bHP" = djnbHP,
                          "hp" = djnhp, 
                          "diff" = djndiff)
    output_dir <- paste0("./text/output/granger/baseline/", datatype, "/")
    
  } else {
    data_source <- switch(datatype,
                          "level" = djnlevel,
                          "bHP" = djnbHP,
                          "hp" = djnhp,
                          "diff" = djndiff)
      output_dir <- paste0("./text/output/granger/", var, "/", group, "/", datatype, "/")
      
    
  } 
  
  sink(file = paste0(output_dir, "output_", topic, "_", modeltype, ".txt"))
  
  if (var == "base") {
    data <- data_source[, c("economic_activity", "inflation_expectations_1y", 
                            "inflation_expectations_3y", "inflation", topic, "dummy")]
    
  } else if (var == "income" & group == "low") {
    data <- data_source[, c("economic_activity", "exp_lowinc_1y",
                            "exp_lowinc_3y", "inflation", topic, "dummy")]
    data <- dplyr::rename(data,
                          inflation_expectations_1y = exp_lowinc_1y,
                          inflation_expectations_3y = exp_lowinc_3y)
    
  } else if (var == "income" & group == "mid") {
    data <- data_source[, c("economic_activity", "exp_midinc_1y", 
                            "exp_midinc_3y", "inflation", topic, "dummy")]
    data <- dplyr::rename(data,
                          inflation_expectations_1y = exp_midinc_1y,
                          inflation_expectations_3y = exp_midinc_3y)
    
  } else if (var == "income" & group == "high") {
    data <- data_source[, c("economic_activity", "exp_highinc_1y",
                            "exp_highinc_3y", "inflation", topic, "dummy")]
    data <- dplyr::rename(data,
                          inflation_expectations_1y = exp_highinc_1y,
                          inflation_expectations_3y = exp_highinc_3y)
  } else if (var == "education" & group == "low") {
  data <- data_source[, c("economic_activity", "exp_loweduc_1y",
                          "exp_loweduc_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_loweduc_1y,
                        inflation_expectations_3y = exp_loweduc_3y)
  
} else if (var == "education" & group == "mid") {
  data <- data_source[, c("economic_activity", "exp_mideduc_1y", 
                          "exp_mideduc_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_mideduc_1y,
                        inflation_expectations_3y = exp_mideduc_3y)
  
} else if (var == "education" & group == "high") {
  data <- data_source[, c("economic_activity", "exp_higheduc_1y",
                          "exp_higheduc_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_higheduc_1y,
                        inflation_expectations_3y = exp_higheduc_3y)


} else if (var == "age" & group == "low") {
  data <- data_source[, c("economic_activity", "exp_lowage_1y",
                          "exp_lowage_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_lowage_1y,
                        inflation_expectations_3y = exp_lowage_3y)
  
} else if (var == "age" & group == "mid") {
  data <- data_source[, c("economic_activity", "exp_midage_1y", 
                          "exp_midage_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_midage_1y,
                        inflation_expectations_3y = exp_midage_3y)
  
} else if (var == "age" & group == "high") {
  data <- data_source[, c("economic_activity", "exp_highage_1y",
                          "exp_highage_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_highage_1y,
                        inflation_expectations_3y = exp_highage_3y)
} else if (var == "numeracy" & group == "low") {
  data <- data_source[, c("economic_activity", "exp_lownumeracy_1y",
                          "exp_lownumeracy_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_lownumeracy_1y,
                        inflation_expectations_3y = exp_lownumeracy_3y)
  
} else if (var == "numeracy" & group == "high") {
  data <- data_source[, c("economic_activity", "exp_highnumeracy_1y", 
                          "exp_highnumeracy_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_highnumeracy_1y,
                        inflation_expectations_3y = exp_highnumeracy_3y)
  
} else if (var == "region" & group == "west") {
  data <- data_source[, c("economic_activity", "exp_west_1y",
                          "exp_west_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_west_1y,
                        inflation_expectations_3y = exp_west_3y)
} else if (var == "region" & group == "midwest") {
  data <- data_source[, c("economic_activity", "exp_midwest_1y",
                          "exp_midwest_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_midwest_1y,
                        inflation_expectations_3y = exp_midwest_3y)
} else if (var == "region" & group == "south") {
  data <- data_source[, c("economic_activity", "exp_south_1y",
                          "exp_south_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_south_1y,
                        inflation_expectations_3y = exp_south_3y)
} else if (var == "region" & group == "northeast") {
  data <- data_source[, c("economic_activity", "exp_northeast_1y",
                          "exp_northeast_3y", "inflation", topic, "dummy")]
  data <- dplyr::rename(data,
                        inflation_expectations_1y = exp_northeast_1y,
                        inflation_expectations_3y = exp_northeast_3y)
}
  
  
  
  
  tsdata <- ts(data, start = c(2018, 1), freq = 12)
  

  selection <- VARselect(tsdata[, 1:5], lag.max = 6, exogen = tsdata[,"dummy"],
                         season = 12, type = "none")
  #num_lags <- table(selection$selection)
  #num_lags <- as.numeric(names(which.max(num_lags)))
  num_lags <- selection$selection[3]
  model <- VAR(y = tsdata[,1:5], exogen = tsdata[,"dummy"], p = num_lags, season = 12) # for the tests!
  
  # Inflation expectations 3y equation
  month <- seasonaldummy(tsdata[, topic])
  VAR_EQ0<- dynlm(inflation_expectations_3y ~ L(inflation_expectations_3y, 1:num_lags) +
                    L(inflation_expectations_1y, 1:num_lags) + 
                    L(inflation, 1:num_lags) + L(economic_activity, 1:num_lags) + 
                    L(tsdata[, topic], 1:num_lags) + dummy + month,
                  start = c(2018, 1), end = c(2023, 1), data = tsdata)
  names(VAR_EQ0$coefficients) <- c("Intercept", paste0("inflation_expectations_3y", ".l", 1:num_lags),
                                   paste0("inflation_expectations_1y", ".l", 1:num_lags), 
                                   paste0("inflation", ".l", 1:num_lags), 
                                   paste0("economic_activity", ".l", 1:num_lags),
                                   paste0(topic, ".l", 1:num_lags), "dummy", paste0("month", 1:ncol(month)))
  print(VAR_EQ0) 

  
  # Inflation expectations 1y equation
  VAR_EQ1<- dynlm(inflation_expectations_1y ~ L(inflation_expectations_1y, 1:num_lags) +
                    L(inflation_expectations_3y, 1:num_lags) + 
                    L(inflation, 1:num_lags) + L(economic_activity, 1:num_lags) + 
                    L(tsdata[, topic], 1:num_lags) + dummy + month,
                  start = c(2018, 1), end = c(2023, 1), data = tsdata)
  
  names(VAR_EQ1$coefficients) <- c("Intercept", paste0("inflation_expectations_1y", ".l", 1:num_lags),
                                   paste0("inflation_expectations_3y", ".l", 1:num_lags), 
                                   paste0("inflation", ".l", 1:num_lags), 
                                   paste0("economic_activity", ".l", 1:num_lags),
                                   paste0(topic, ".l", 1:num_lags), "dummy", 
                                   paste0("month", 1:ncol(month)))
  
  print(VAR_EQ1)


  # Inflation  equation
  VAR_EQ2<- dynlm(inflation ~ L(inflation, 1:num_lags) +
                    L(inflation_expectations_1y, 1:num_lags) + 
                    L(inflation_expectations_3y, 1:num_lags) + L(economic_activity, 1:num_lags) + 
                    L(tsdata[, topic], 1:num_lags) + dummy + month,
                  start = c(2018, 1), end = c(2023, 1), data = tsdata)
  
  names(VAR_EQ2$coefficients) <- c("Intercept", paste0("inflation_expectations_1y", ".l", 1:num_lags),
                                   paste0("inflation_expectations_3y", ".l", 1:num_lags), 
                                   paste0("inflation", ".l", 1:num_lags), 
                                   paste0("economic_activity", ".l", 1:num_lags),
                                   paste0(topic, ".l", 1:num_lags), "dummy",
                                   paste0("month", 1:ncol(month)))
  
  print(VAR_EQ2)
  

  # economic acticity equation
  VAR_EQ3<- dynlm(economic_activity ~ L(economic_activity, 1:num_lags) +
                    L(inflation_expectations_1y, 1:num_lags) + 
                    L(inflation_expectations_3y, 1:num_lags) + 
                    L(inflation, 1:num_lags) + 
                    L(tsdata[, topic], 1:num_lags) + dummy + month,
                  start = c(2018, 1), end = c(2023, 1), data = tsdata)
  
  names(VAR_EQ3$coefficients) <- c("Intercept", paste0("economic_acticity", ".l", 1:num_lags),
                                   paste0("inflation_expectations_1y", ".l", 1:num_lags),
                                   paste0("inflation_expectations_3y", ".l", 1:num_lags), 
                                   paste0("inflation", ".l", 1:num_lags), 
                                   paste0(topic, ".l", 1:num_lags), "dummy",
                                   paste0("month", 1:ncol(month)))
  
  print(VAR_EQ3)
  

  # topic equation
  VAR_EQ4<- dynlm(tsdata[, topic] ~ L(tsdata[, topic], 1:num_lags) +
                    L(economic_activity, 1:num_lags) +
                    L(inflation_expectations_1y, 1:num_lags) + 
                    L(inflation_expectations_3y, 1:num_lags) + 
                    L(inflation, 1:num_lags) + 
                    dummy + month,
                  start = c(2018, 1), end = c(2023, 1), data = tsdata)
  
  names(VAR_EQ4$coefficients) <- c("Intercept", paste0(topic, ".l", 1:num_lags),
                                   paste0("economic_activity", ".l", 1:num_lags),
                                   paste0("inflation_expectations_1y", ".l", 1:num_lags),
                                   paste0("inflation_expectations_3y", ".l", 1:num_lags), 
                                   paste0("inflation", ".l", 1:num_lags), 
                                   "dummy",paste0("month", 1:ncol(month)))
  print(VAR_EQ4)
  
  
  print("diagnostics")
  
  
  print("heteroskedasticity")
  # Heteroskedasticity
  bv.arch <- arch.test(model, lags.multi = num_lags, multivariate.only = TRUE)
  print(bv.arch) # H0: no heteroskedasticity
  
  print("serial correlation")
  # serial correlation
  serial <- serial.test(model, type = "BG", lags.pt = 6)
  print(serial) # H0:no serial correlation
  
  
  print("inflation expectations_3y")
  # inflation expectations
  
  g0 <- linearHypothesis(VAR_EQ0, 
                         hypothesis.matrix = c(paste0(topic, ".l", 1:num_lags)),
                         vcov = vcovHAC, test = "F")
  p0 <- g0[2, "Pr(>F)"]
  print(g0)
  

  print("inflation expectations_1y")
  # inflation expectations
  g1 <- linearHypothesis(VAR_EQ1, 
                         hypothesis.matrix = c(paste0(topic, ".l", 1:num_lags)),
                         vcov. = vcovHAC, test = "F")
  
  p1 <- g1[2, "Pr(>F)"]
  print(g1)
  
  
  
  print("inflation")
  # inflation
  g2 <- linearHypothesis(VAR_EQ2, 
                         hypothesis.matrix = c(paste0(topic, ".l", 1:num_lags)),
                         vcov. = vcovHAC, test = "F")
  p2 <- g2[2, "Pr(>F)"]
  print(g2)
  
  
  print("economic_activity")
  # economic activity
  g3 <- linearHypothesis(VAR_EQ3, 
                         hypothesis.matrix = c(paste0(topic, ".l", 1:num_lags)),
                         vcov. = vcovHAC, test = "F")
  p3 <- g3[2, "Pr(>F)"]
  print(g3)

  print("feedback effects")
  
 
  
  
  print("topic expectations 3y")
  # expectations_3y = 0 
  g5 <- linearHypothesis(VAR_EQ4, 
                         hypothesis.matrix = c(paste0("inflation_expectations_3y", ".l", 1:num_lags)),
                         vcov. = vcovHAC, test = "F")
  p5 <- g5[2, "Pr(>F)"]
  print(g5)
  
  
  print("topic expectations_1y")
  # expectations_1y = 0
  g6<- linearHypothesis(VAR_EQ4, 
                        hypothesis.matrix = c(paste0("inflation_expectations_1y", ".l", 1:num_lags)),
                        vcov. = vcovHAC, test = "F")
  p6 <- g6[2, "Pr(>F)"]
  print(g6)
  
  
  print("topic inflation")
  
  # inflation = 0
  g7 <- linearHypothesis(VAR_EQ4, 
                         hypothesis.matrix = c(paste0("inflation", ".l", 1:num_lags)),
                         vcov. = vcovHAC, test = "F")
  p7 <- g7[2, "Pr(>F)"]
  print(g7)
 
  
  print("topic activitiy")
  # economic_activity = 0 
  g8 <- linearHypothesis(VAR_EQ4, 
                         hypothesis.matrix = c(paste0("economic_activity", ".l", 1:num_lags)),
                         vcov. = vcovHAC, test = "F")
  p8 <- g8[2, "Pr(>F)"]
  print(g8)
  
  sink(file = NULL)
  p_values <- c(p0, p1, p2, p3)
  p_values_feedback <- c(p5, p6, p7, p8)
  
  if (reverse == TRUE){
    return(p_values_feedback)
  } else {
  return(p_values)
  }
  
}


topic_names <- c("government_spending", "monetary_policy", "pent_up_demand", "demand_shift",
                 "supply_chain", "energy", "labor_shortage", "pandemic", "politics",
                 "war", "debt", "taxes", "profits")
topic_table <- c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift",
                 "Supply Chain", "Energy", "Labor Shortage", "Pandemic", "Politics",
                 "War", "Debt", "Taxes", "Profits")



## bHP ##

# Robustness - bHP/Boosted HP
# Format p-values with asterisks
format_p_value <- function(p) {
  if (p < 0.01) {
    return("$<$0.01 ***")
  } else if (p < 0.05) {
    return(sprintf("%.2f **", p))
  } else if (p < 0.1) {
    return(sprintf("%.2f *", p))
  } else {
    return(sprintf("%.2f", p))
  }
}


# Create a tibble with the required structure and formatted p-values
base_bHP <- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Expectations" = character(),
  "3-Year Expectations" = character(),
  "CPI Inflation" = character(),
  "Economic Activity" = character()
)




# Iterate across topic names 
for (i in seq_along(topic_names)) {
  p_value <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "base")
  
  base_bHP <- add_row(base_bHP,
                       Narrative = topic_table[i],
                       Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                         ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                       "1-Year Expectations" = format_p_value(p_value[2]),
                       "3-Year Expectations" = format_p_value(p_value[1]),
                      "CPI Inflation" = format_p_value(p_value[3]),
                      "Economic Activity" = format_p_value(p_value[4])
  )
}




# Function to generate LaTeX table with custom formatting and caption
generate_latex_table <- function(data, caption) {
  categories <- unique(data$Category)
  latex_table <- paste0("\\begin{table}[ht]\n\\centering\n\\caption{", caption, "}\\label{table:granger}\n\n\\begin{tabular}{lcc}\n\\toprule\n\\textbf{Narratives} & \\textbf{One-Year Expectations} & \\textbf{Three-Year Expectations} \\\\\n& (Pr($>$F)) & (Pr($>$F)) \\\\\n\\midrule\n")
  
  for (cat in categories) {
    latex_table <- paste0(latex_table, "\\multicolumn{3}{l}{\\textbf{", cat, "}} \\\\\n\\midrule\n")
    subset <- data[data$Category == cat, ]
    for (i in 1:nrow(subset)) {
      latex_table <- paste0(latex_table, subset$Narrative[i], " & ", subset$`1-Year Expectations`[i], " & ", subset$`3-Year Expectations`[i], " \\\\\n")
    }
    latex_table <- paste0(latex_table, "\\midrule\n")
  }
  
  latex_table <- sub("\\midrule\n\\midrule\n$", "\\midrule\n", latex_table)  # Remove the last midrule
  latex_table <- paste0(latex_table, "\\bottomrule\n\\textit{Note:}  & \\multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\\n\\bottomrule\n\\end{tabular}\n\\end{table}")
  
  return(latex_table)
}

# Example of generating and saving the LaTeX table with a custom caption
custom_caption <- "Granger causality analysis (boosted HP-Filter)"
latex_table_bHP <- generate_latex_table(base_bHP, custom_caption)
write(latex_table_bHP, file = "./text/output/granger/baseline/bHP/granger_bHP_base.tex")


## bHP Feedback


base_bHP_feedback<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Expectations" = character(),
  "3-Year Expectations" = character(),
  "CPI Inflation" = format_p_value(p_value[3]),
  "Economic Activity" = format_p_value(p_value[4])
)


# Iterate across topic names
for (i in seq_along(topic_names)) {
  p_value <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "base", reverse = TRUE)
  
  base_bHP_feedback <- add_row(base_bHP_feedback,
                       Narrative = topic_table[i],
                       Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                         ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                       "1-Year Expectations" = format_p_value(p_value[2]),
                       "3-Year Expectations" = format_p_value(p_value[1]),
                       "CPI Inflation" = format_p_value(p_value[3]),
                       "Economic Activity" = format_p_value(p_value[4])
  )
}



# 26 warnings regarding "dummy" name

custom_caption <- "Reverse granger causality analysis (boosted HP-Filter)"
latex_table_bHP_feedback <- generate_latex_table(base_bHP_feedback, custom_caption)
write(latex_table_bHP_feedback, file = "./text/output/granger/baseline/bHP/granger_bHP_base_feedback.tex")





## Estimation Level

base_level<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Expectations" = character(),
  "3-Year Expectations" = character()
)


# Iterate across topics
for (i in seq_along(topic_names)) {
  p_value <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "base")
  
  base_level<- add_row(base_level,
                    Narrative = topic_table[i],
                    Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                      ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                    "1-Year Expectations" = format_p_value(p_value[2]),
                    "3-Year Expectations" = format_p_value(p_value[1])
  )
}


# 26 warnings regarding "dummy" name

custom_caption <- "Granger causality analysis (level)"
latex_table_level <- generate_latex_table(base_level, custom_caption)
write(latex_table_level, file = "./text/output/granger/baseline/level/granger_level_base.tex")



## Estimation Level Feedback

base_level_feedback<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Expectations" = character(),
  "3-Year Expectations" = character()
)


# Iterate across topic names
for (i in seq_along(topic_names)) {
  p_value <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "base", reverse = TRUE)
  
  base_level_feedback <- add_row(base_level_feedback,
                              Narrative = topic_table[i],
                              Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                                ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                              "1-Year Expectations" = format_p_value(p_value[2]),
                              "3-Year Expectations" = format_p_value(p_value[1])
  )
}


# 26 warnings regarding "dummy" name

custom_caption <- "Granger causality analysis (level)"
latex_table_level_feedback <- generate_latex_table(base_level_feedback, custom_caption)
write(latex_table_level_feedback, file = "./text/output/granger/baseline/level/granger_level_base_feedback.tex")




## with differences



base_diff<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Expectations" = character(),
  "3-Year Expectations" = character()
)


# Iterate across all topic names
for (i in seq_along(topic_names)) {
  p_value <- granger_cause(topic_names[i], datatype = "diff", modeltype = "djn", var = "base")
  
  base_diff<- add_row(base_diff,
                    Narrative = topic_table[i],
                    Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                      ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                    "1-Year Expectations" = format_p_value(p_value[2]),
                    "3-Year Expectations" = format_p_value(p_value[1])
  )
}



# 26 warnings regarding "dummy" name



custom_caption <- "Granger causality analysis (differences)"
latex_table_diff <- generate_latex_table(base_diff, custom_caption)
write(latex_table_diff, file = "./text/output/granger/baseline/diff/granger_diff_base.tex")










## INCOME HETEROGENEITY


# Function to generate LaTeX table with custom formatting and caption (income heterogeneity)
generate_latex_table_income <- function(data, caption) {
  categories <- unique(data$Category)
  latex_table <- paste0("\\begin{sidewaystable}[ht]\n\\centering\n\\caption{", caption, "}\\label{table:granger}\n\n\\begin{tabular}{lcccccc}\n \\toprule\n\\textbf{Narratives} & \\textbf{1-Year Low Income} & \\textbf{3-Year Low Income} & \\textbf{1-Year Mid Income} & \\textbf{3-Year Mid Income} & \\textbf{1-Year High Income} & \\textbf{3-Year High Income} \\\\\n\\midrule\n")
  
  for (cat in categories) {
    latex_table <- paste0(latex_table, "\\multicolumn{7}{l}{\\textbf{", cat, "}} \\\\\n\\midrule\n")
    subset <- data[data$Category == cat, ]
    for (i in 1:nrow(subset)) {
      latex_table <- paste0(latex_table, subset$Narrative[i], " & ", subset$`1-Year Low Income`[i], " & ", subset$`3-Year Low Income`[i], " & ", subset$`1-Year Mid Income`[i], " & ", subset$`3-Year Mid Income`[i], " & ", subset$`1-Year High Income`[i], " & ", subset$`3-Year High Income`[i], " \\\\\n")
    }
    latex_table <- paste0(latex_table, "\\midrule\n")
  }
  
  latex_table <- sub("\\midrule\n\\midrule\n$", "\\midrule\n", latex_table)  # Remove the last midrule
  latex_table <- paste0(latex_table, "\\bottomrule\n\\textit{Note:}  & \\multicolumn{6}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\\n\\bottomrule\n\\end{tabular}\n\\end{sidewaystable}")
  
  return(latex_table)
}







# income heterogeneity

# Initialize the tibble for "level" data type
income_level <- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Income" = character(),
  "3-Year Low Income" = character(),
  "1-Year Mid Income" = character(),
  "3-Year Mid Income" = character(),
  "1-Year High Income" = character(),
  "3-Year High Income" =  character()
)

for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "income", group = "low")
  
  # Get p-values for the 'mid' group
  p_value_mid <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "income", group = "mid")
  
  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "income", group = "high")
  
  # Bind all together into the main tibble
  
  
  income_level<- add_row(income_level,
                    Narrative = topic_table[i],
                    Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                      ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                    "1-Year Low Income" = format_p_value(p_value_low[2]),
                    "3-Year Low Income" = format_p_value(p_value_low[1]),
                    "1-Year Mid Income" = format_p_value(p_value_mid[2]),
                    "3-Year Mid Income" = format_p_value(p_value_mid[1]),
                    "1-Year High Income" = format_p_value(p_value_high[2]),
                    "3-Year High Income" = format_p_value(p_value_high[1]))
  
}


# 50 dummy warnings

custom_caption <- "Income: Granger causality analysis (level)"
latex_table_income_level <- generate_latex_table_income(income_level, custom_caption)
write(latex_table_income_level, file = "./text/output/granger/income/granger_income_level.tex")










# Initialize the tibble for "bHP" data type
income_bHP <- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Income" = character(),
  "3-Year Low Income" = character(),
  "1-Year Mid Income" = character(),
  "3-Year Mid Income" = character(),
  "1-Year High Income" = character(),
  "3-Year High Income" =  character()
)

for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "income", group = "low")
  
  # Get p-values for the 'mid' group
  p_value_mid <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "income", group = "mid")
  
  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "income", group = "high")
  
  income_bHP<- add_row(income_bHP,
                         Narrative = topic_table[i],
                         Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                           ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                         "1-Year Low Income" = format_p_value(p_value_low[2]),
                         "3-Year Low Income" = format_p_value(p_value_low[1]),
                         "1-Year Mid Income" = format_p_value(p_value_mid[2]),
                         "3-Year Mid Income" = format_p_value(p_value_mid[1]),
                         "1-Year High Income" = format_p_value(p_value_high[2]),
                         "3-Year High Income" = format_p_value(p_value_high[1]))
  
}

# 50 dummy warnings

custom_caption <- "Income: Granger causality analysis (bHP-Filter)"
latex_table_income_bHP <- generate_latex_table_income(income_bHP, custom_caption)
write(latex_table_income_bHP, file = "./text/output/granger/income/granger_income_bHP.tex")








# EDUCATION 

# Function to generate LaTeX table with custom formatting and caption (income heterogeneity)
generate_latex_table_educ <- function(data, caption) {
  categories <- unique(data$Category)
  latex_table <- paste0("\\begin{sidewaystable}[ht]\n\\centering\n\\caption{", caption, "}\\label{table:granger}\n\n\\begin{tabular}{lcccccc}\n\\toprule\n\\textbf{Narratives} & \\textbf{1-Year Low Education} & \\textbf{3-Year Low Education} & \\textbf{1-Year Mid Education} & \\textbf{3-Year Mid Education} & \\textbf{1-Year High Education} & \\textbf{3-Year High Education} \\\\\n\\midrule\n")
  
  for (cat in categories) {
    latex_table <- paste0(latex_table, "\\multicolumn{7}{l}{\\textbf{", cat, "}} \\\\\n\\midrule\n")
    subset <- data[data$Category == cat, ]
    for (i in 1:nrow(subset)) {
      latex_table <- paste0(latex_table, subset$Narrative[i], " & ", subset$`1-Year Low Education`[i], " & ", subset$`3-Year Low Education`[i], " & ", subset$`1-Year Mid Education`[i], " & ", subset$`3-Year Mid Education`[i], " & ", subset$`1-Year High Education`[i], " & ", subset$`3-Year High Education`[i], " \\\\\n")
    }
    latex_table <- paste0(latex_table, "\\midrule\n")
  }
  
  latex_table <- sub("\\midrule\n\\midrule\n$", "\\midrule\n", latex_table)  # Remove the last midrule
  latex_table <- paste0(latex_table, "\\bottomrule\n\\textit{Note:}  & \\multicolumn{6}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\\n\\bottomrule\n\\end{tabular}\n\\end{sidewaystable}")
  
  return(latex_table)
}



# Initialize the tibble for "level" data type
education_level <- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Education" = character(),
  "3-Year Low Education" = character(),
  "1-Year Mid Education" = character(),
  "3-Year Mid Education" = character(),
  "1-Year High Education" = character(),
  "3-Year High Education" =  character()
)

for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "education", group = "low")
  
  # Get p-values for the 'mid' group
  p_value_mid <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "education", group = "mid")
  
  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "education", group = "high")
  
  # Bind all together into the main tibble
  education_level<- add_row(education_level,
                      Narrative = topic_table[i],
                      Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                        ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                      "1-Year Low Education" = format_p_value(p_value_low[2]),
                      "3-Year Low Education" = format_p_value(p_value_low[1]),
                      "1-Year Mid Education" = format_p_value(p_value_mid[2]),
                      "3-Year Mid Education" = format_p_value(p_value_mid[1]),
                      "1-Year High Education" = format_p_value(p_value_high[2]),
                      "3-Year High Education" = format_p_value(p_value_high[1]))
}


custom_caption <- "Education: Granger causality analysis (level)"
latex_table_education_level <- generate_latex_table_educ(education_level, custom_caption)
write(latex_table_education_level, file = "./text/output/granger/education/granger_education_level.tex")







# Initialize the tibble for "bHP" data type
education_bHP <- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Education" = character(),
  "3-Year Low Education" = character(),
  "1-Year Mid Education" = character(),
  "3-Year Mid Education" = character(),
  "1-Year High Education" = character(),
  "3-Year High Education" =  character()
)


for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "education", group = "low")
  
  # Get p-values for the 'mid' group
  p_value_mid <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "education", group = "mid")
  
  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "education", group = "high")
  
  # Bind all together into the main tibble
  education_bHP<- add_row(education_bHP,
                            Narrative = topic_table[i],
                            Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                              ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                            "1-Year Low Education" = format_p_value(p_value_low[2]),
                            "3-Year Low Education" = format_p_value(p_value_low[1]),
                            "1-Year Mid Education" = format_p_value(p_value_mid[2]),
                            "3-Year Mid Education" = format_p_value(p_value_mid[1]),
                            "1-Year High Education" = format_p_value(p_value_high[2]),
                            "3-Year High Education" = format_p_value(p_value_high[1]))
}

custom_caption <- "Education: Granger causality analysis (bHP-Filter)"
latex_table_education_bHP <- generate_latex_table_educ(education_bHP, custom_caption)
write(latex_table_education_bHP, file = "./text/output/granger/education/granger_education_bHP.tex")






## AGE #############


# Function to generate LaTeX table with custom formatting and caption (age heterogeneity)
generate_latex_table_age <- function(data, caption) {
  categories <- unique(data$Category)
  latex_table <- paste0("\\begin{sidewaystable}[ht]\n\\centering\n\\caption{", caption, "}\\label{table:granger}\n\n\\begin{tabular}{lcccccc}\n\\toprule\n\\textbf{Narratives} & \\textbf{1-Year Low Age} & \\textbf{3-Year Low Age} & \\textbf{1-Year Mid Age} & \\textbf{3-Year Mid Age} & \\textbf{1-Year High Age} & \\textbf{3-Year High Age} \\\\\n\\midrule\n")
  
  for (cat in categories) {
    latex_table <- paste0(latex_table, "\\multicolumn{7}{l}{\\textbf{", cat, "}} \\\\\n\\midrule\n")
    subset <- data[data$Category == cat, ]
    for (i in 1:nrow(subset)) {
      latex_table <- paste0(latex_table, subset$Narrative[i], " & ", subset$`1-Year Low Age`[i], " & ", subset$`3-Year Low Age`[i], " & ", subset$`1-Year Mid Age`[i], " & ", subset$`3-Year Mid Age`[i], " & ", subset$`1-Year High Age`[i], " & ", subset$`3-Year High Age`[i], " \\\\\n")
    }
    latex_table <- paste0(latex_table, "\\midrule\n")
  }
  
  latex_table <- sub("\\midrule\n\\midrule\n$", "\\midrule\n", latex_table)  # Remove the last midrule
  latex_table <- paste0(latex_table, "\\bottomrule\n\\textit{Note:}  & \\multicolumn{6}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\\n\\bottomrule\n\\end{tabular}\n\\end{sidewaystable}")
  
  return(latex_table)
}





# Initialize the tibble for "level" data type
age_level<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Age" = character(),
  "3-Year Low Age" = character(),
  "1-Year Mid Age" = character(),
  "3-Year Mid Age" = character(),
  "1-Year High Age" = character(),
  "3-Year High Age" =  character()
)


for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "age", group = "low")
  
  # Get p-values for the 'mid' group
  p_value_mid <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "age", group = "mid")
  
  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "age", group = "high")
  
  # Bind all together into the main tibble
  age_level<- add_row(age_level,
                         Narrative = topic_table[i],
                         Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                           ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                         "1-Year Low Age" = format_p_value(p_value_low[2]),
                         "3-Year Low Age" = format_p_value(p_value_low[1]),
                         "1-Year Mid Age" = format_p_value(p_value_mid[2]),
                         "3-Year Mid Age" = format_p_value(p_value_mid[1]),
                         "1-Year High Age" = format_p_value(p_value_high[2]),
                         "3-Year High Age" = format_p_value(p_value_high[1]))
}

custom_caption <- "Age: Granger causality analysis (level)"
latex_table_age_level <- generate_latex_table_age(age_level, custom_caption)
write(latex_table_age_level, file = "./text/output/granger/age/granger_age_level.tex")



# Initialize the tibble for "bHP" data type
age_bHP<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Age" = character(),
  "3-Year Low Age" = character(),
  "1-Year Mid Age" = character(),
  "3-Year Mid Age" = character(),
  "1-Year High Age" = character(),
  "3-Year High Age" =  character()
)


for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "age", group = "low")
  
  # Get p-values for the 'mid' group
  p_value_mid <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "age", group = "mid")
  
  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "age", group = "high")
  
  # Bind all together into the main tibble
  age_bHP<- add_row(age_bHP,
                      Narrative = topic_table[i],
                      Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                        ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                      "1-Year Low Age" = format_p_value(p_value_low[2]),
                      "3-Year Low Age" = format_p_value(p_value_low[1]),
                      "1-Year Mid Age" = format_p_value(p_value_mid[2]),
                      "3-Year Mid Age" = format_p_value(p_value_mid[1]),
                      "1-Year High Age" = format_p_value(p_value_high[2]),
                      "3-Year High Age" = format_p_value(p_value_high[1]))
}

custom_caption <- "Age: Granger causality analysis (bHP-Filter)"
latex_table_age_bHP <- generate_latex_table_age(age_bHP, custom_caption)
write(latex_table_age_bHP, file = "./text/output/granger/age/granger_age_bHP.tex")





# numeracy heterogeneity

# Function to generate LaTeX table with custom formatting and caption (numeracy heterogeneity)
generate_latex_table_numeracy <- function(data, caption) {
  categories <- unique(data$Category)
  latex_table <- paste0("\\begin{sidewaystable}[ht]\n\\centering\n\\caption{", caption, "}\\label{table:granger}\n\n\\begin{tabular}{lcccc}\n\\toprule\n\\textbf{Narratives} & \\textbf{1-Year Low Numeracy} & \\textbf{3-Year Low Numeracy} & \\textbf{1-Year High Numeracy} & \\textbf{3-Year High Numeracy} \\\\\n\\midrule\n")
  
  for (cat in categories) {
    latex_table <- paste0(latex_table, "\\multicolumn{5}{l}{\\textbf{", cat, "}} \\\\\n\\midrule\n")
    subset <- data[data$Category == cat, ]
    for (i in 1:nrow(subset)) {
      latex_table <- paste0(latex_table, subset$Narrative[i], " & ", subset$`1-Year Low Numeracy`[i], " & ", subset$`3-Year Low Numeracy`[i], " & ", subset$`1-Year High Numeracy`[i], " & ", subset$`3-Year High Numeracy`[i], " \\\\\n")
    }
    latex_table <- paste0(latex_table, "\\midrule\n")
  }
  
  latex_table <- sub("\\midrule\n\\midrule\n$", "\\midrule\n", latex_table)  # Remove the last midrule
  latex_table <- paste0(latex_table, "\\bottomrule\n\\textit{Note:}  & \\multicolumn{4}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\\n\\bottomrule\n\\end{tabular}\n\\end{sidewaystable}")
  
  return(latex_table)
}


# Initialize the tibble for "hp" data type
numeracy_level<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Numeracy" = character(),
  "3-Year Low Numeracy" = character(),
  "1-Year High Numeracy" = character(),
  "3-Year High Numeracy" = character()
)



for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "numeracy", group = "low")
  

  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "level", modeltype = "djn", var = "numeracy", group = "high")
  
  # Bind all together into the main tibble
  numeracy_level<- add_row(numeracy_level,
                      Narrative = topic_table[i],
                      Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                        ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                      "1-Year Low Numeracy" = format_p_value(p_value_low[2]),
                      "3-Year Low Numeracy" = format_p_value(p_value_low[1]),
                      "1-Year High Numeracy" = format_p_value(p_value_high[2]),
                      "3-Year High Numeracy" = format_p_value(p_value_high[1]))
}

custom_caption <- "Numeracy: Granger causality analysis (level)"
latex_table_numeracy_level<- generate_latex_table_numeracy(numeracy_level, custom_caption)
write(latex_table_numeracy_level, file = "./text/output/granger/numeracy/granger_numeracy_level.tex")



# Initialize the tibble for "bHP" data type
numeracy_bHP<- tibble(
  Narrative = character(),
  Category = character(),
  "1-Year Low Numeracy" = character(),
  "3-Year Low Numeracy" = character(),
  "1-Year High Numeracy" = character(),
  "3-Year High Numeracy" = character()
)


for (i in seq_along(topic_names)) {
  # Get p-values for the 'low' group
  p_value_low <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "numeracy", group = "low")

  
  # Get p-values for the 'high' group
  p_value_high <- granger_cause(topic_names[i], datatype = "bHP", modeltype = "djn", var = "numeracy", group = "high")
  
  # Bind all together into the main tibble
  numeracy_bHP<- add_row(numeracy_bHP,
                           Narrative = topic_table[i],
                           Category = ifelse(topic_table[i] %in% c("Government Spending", "Monetary Policy", "Pent-up Demand", "Demand Shift"), "Demand",
                                             ifelse(topic_table[i] %in% c("Supply Chain", "Energy", "Labor Shortage"), "Supply", "Miscellaneous")),
                           "1-Year Low Numeracy" = format_p_value(p_value_low[2]),
                           "3-Year Low Numeracy" = format_p_value(p_value_low[1]),
                           "1-Year High Numeracy" = format_p_value(p_value_high[2]),
                           "3-Year High Numeracy" = format_p_value(p_value_high[1]))
}

custom_caption <- "Numeracy: Granger causality analysis (bHP-Filter)"
latex_table_numeracy_bHP<- generate_latex_table_numeracy(numeracy_bHP, custom_caption)
write(latex_table_numeracy_bHP, file = "./text/output/granger/numeracy/granger_numeracy_bHP.tex")






