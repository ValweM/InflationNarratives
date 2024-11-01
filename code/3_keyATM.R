## Create Topics
Sys.setenv(LANG = "en")
Sys.setenv(LC_ALL="en_US.UTF-8")
rm(list=ls())
gc()



mypackages <- c("dplyr", "quanteda", "keyATM", "quanteda.corpora", "lubridate", 
                "stringr", "tidytext", "data.table", "quanteda.textstats", 
                "quanteda.textplots", "textdata", "spacyr", "text2vec")

lapply(mypackages, require, character.only = TRUE)
#setwd("/media/maxweinig/My Passport/2023_MediaReportsOnInflationNarratives/code")

getwd()


load(file="../data/corpus/tokens_w.rds")
load(file ="../data/dfm/dfm_w.rds")
DFM_w <- DFM


load(file="../data/corpus/tokens.rds")
load(file ="../data/dfm/dfm.rds")



## Define keywords for topics



energy <- c("crude", "gas", "gasoline", "oil", "crude", "fuel")
war <- c("russia", "war", "ukraine", "invasion", "moscow", "putin", "military")
pandemic <- c("pandemic", "covid-19", "virus", "coronavirus", "infection", "outbreak", "case")
labor_shortage <- c("worker", "employment", "labor", "wage", "workforce", "labour", "job", "strike", "union", "hire")
supply_chain <- c("supply", "supplier", "chain", "producer", "bottleneck", "disruption")
monetary_policy <- c("fed", "quantitative", "easing", "loose", "monetary", "interest")
government_spending <-c("infrastructure", "agreement", "biden", "negotiation", "spending", "deficit", "bipartisan", "package")
pent_up_demand <- c("demand", "surge", "activity", "recovery", "pandemic")
demand_shift <- c("consumer", "good", "durable", "shift", "vehicle", "trend", "service", "change")
profits <- c("margin", "corporate", "profitability", "profit", "growth")
politics <- c("party", "republican", "trump", "congress", "senate", "president", "biden", "democrats", "government")
debt <- c("debt", "public", "national", "federal", "deficit", "borrowing", "government", "balance")
taxes <- c("tax", "raise", "reform", "legislation", "overhaul", "reduction")


#energy <- c("crude_oil", "gas_price", "natural_gas", "fuel_gas", "energy_price")
#war <- c("russian_invasion", "russia_invasion", "ukraine_war", "war_ukraine")
#pandemic <- c("new_coronavirus", "covid-19_virus", "coronavirus_infection", "coronavirus_pandemic", "coronavirus_case")
#labor_shortage <- c("unemployment_rate", "labor_market", "labor_shortage", "labour_market", "tight_labor", "job_market")
#supply_chain <- c("chip_shortage", "supply_constraint", "chain_bottleneck", "supply_chain", "chain_disruption", "chip_maker")
#monetary_policy <- c("monetary_policy", "quantitative_easing", "loose_monetary", "policy_easing", "policy_meeting", "rate_cut")
#government_spending <-c("government_spending", "fiscal_stimulus", "fiscal_policy", "inflation_reduction", "reduction_act", "expansionary_fiscal")
#pent_up_demand <- c("strong_consumer", "economic_recovery", "demand_recovery", "consumer_spending", "economic_growth")
#demand_shift <- c("demand_shift", "consumption_trend", "spending_shift", "consumption_trend", "consumer_shift", "shift sharply")
#profits <- c("profit_margin", "gross_profit", "wide_profit", "profit_growth", "revenue_growth")
#politics <- c("political_party", "republican_party", "president_trump", "president_biden", "policy_failure")
#debt <- c("public_debt", "government_debt", "sovereign_debt", "fiscal_deficit", "debt_crisis")
#taxes <- c("tax_raise", "tax_reform", "tax_legislation", "tax_increase", "tax_rate")

## define list of keywords

keywords <- list(
  energy = energy,
  war = war,
  pandemic = pandemic,
  labor_shortage = labor_shortage,
  supply_chain = supply_chain,
  monetary_policy = monetary_policy,
  government_spending = government_spending,
  pent_up_demand = pent_up_demand,
  demand_shift = demand_shift,
  profits = profits,
  politics = politics,
  debt = debt,
  taxes = taxes
)


 
## word embeddings https://quanteda.io/articles/pkgdown/replication/text2vec.html
dfm_vocab <- featnames(dfm_trim(DFM, min_count = 10))
dfm_toks <- tokens_select(key_corpus, dfm_vocab, padding = TRUE)
fcm <- fcm(dfm_toks, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)

glove <- GlobalVectors$new(rank = 30, x_max = 10)
wv_main <- glove$fit_transform(fcm, n_iter = 10,
                               convergence_tol = 0.01, n_threads = 8)

dim(wv_main)

wv_context <- glove$components
dim(wv_context)

word_vectors <- wv_main + t(wv_context)

word_analogy <-   word_vectors["margin", , drop = FALSE] +
  word_vectors["profit", , drop = FALSE] 

cos_sim = sim2(x = word_vectors, 
               y = word_analogy, 
               method = "cosine", 
               norm = "l2")


head(sort(cos_sim[,1], decreasing = TRUE), 70)


## find related words https://tutorials.quanteda.io/advanced-operations/target-word-collocations/
## Random sample of corpus to calcuated keyness of keywords faster
set.seed(4780)

pattern <- government_spending

toks_inside <- tokens_keep(key_corpus, pattern = pattern, window = 10)
toks_inside <- tokens_remove(key_corpus, pattern = pattern) # remove the keywords
toks_outside <- tokens_remove(key_corpus, pattern = pattern, window = 10)


gc()

dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)

rm(toks_inside)
rm(toks_outside)
gc()

## PMI instead of chi2?

tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     measure = "lr",
                                     target = seq_len(ndoc(dfmat_inside)))
rm(dfmat_outside)
rm(dfmat_inside)
gc()



tstat_key_inside
rm(tstat_key_inside)
gc()




keyATM_docs <- keyATM_read(DFM)
keyATM_docs_w <- keyATM_read(DFM_w)
save(keyATM_docs, file ="../data/models/keyATM_docs.rds")
save(keyATM_docs_w, file ="../data/models/keyATM_docs_w.rds")

load(file ="../data/models/keyATM_docs.rds")

## Proportion of keywords in documents (Proportion is defined as a number of times
#a keyword occurs in the corpus divided by the total length of documents.
#This measures the frequency of the keyword in the corpus. )

key_viz <- visualize_keywords(docs = keyATM_docs, keywords = keywords)
key_viz


key_viz_w <- visualize_keywords(docs = keyATM_docs_w, keywords = keywords)
key_viz_w

save_fig(key_viz, file = "../text/figures/key_viz.png")
save_fig(key_viz_w, file = "../text/figures/key_viz_w.png")

### dynamic keyATM ####

vars <- docvars(DFM)
vars_period <- vars %>%
  mutate(Period = dense_rank(date)) %>%
  mutate(Year = year(date)) %>%
  mutate(Month = month(date)) %>%
  mutate(date = date) %>%
  dplyr::select(Year, date, Period)

vars_w <- docvars(DFM_w)
vars_period_w <- vars_w %>%
  mutate(Period = dense_rank(date)) %>%
  mutate(Year = year(date)) %>%
  mutate(Month = month(date)) %>%
  mutate(date = date) %>%
  dplyr::select(Year, date, Period)

save(vars_period, file = "../data/models/vars_period.rds")

save(vars_period_w, file = "../data/models/vars_period_w.rds")

#load("../data/models/vars_period_s.rds")
#load("../data/models/vars_period_w.rds")


dynamic <- keyATM(docs          = keyATM_docs,                         
                  no_keyword_topics = 50,                                   
                  keywords          = keywords,                      
                  model             = "dynamic",
                  
                  model_settings    = list(time_index = vars_period$Period, ### Number of days
                                           num_states = 4),              ## 4 Periods in observational period
                  options           = list(seed = 265720, verbose = TRUE,iterations = 5000))

save(dynamic, file = "../data/models/dynamic_s_7.rds")

dynamic <- keyATM(docs          = keyATM_docs_w,                         
                  no_keyword_topics = 50,                                   
                  keywords          = keywords,                      
                  model             = "dynamic",
                  
                  model_settings    = list(time_index = vars_period_w$Period, ### Number of days
                                           num_states = 4),              ## 4 Periods in observational period
                  options           = list(seed = 265720, verbose = TRUE,iterations = 1000))


save(dynamic, file = "../data/models/dynamic_w2.rds")


