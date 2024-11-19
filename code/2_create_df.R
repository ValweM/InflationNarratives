
# reading nml files of DJ corpus
Sys.setenv(LC_ALL="en_US.UTF-8")
rm(list=ls())
gc()


mypackages <- c("tibble", "dplyr", "stringr", "foreach", "doParallel", "data.table", "XML", "rvest")
lapply(mypackages, require, character.only = TRUE)


options(stringsAsFactors = F)

getwd()

source("./functions.R")



namesoflines <- list.files(path="../data/rds", pattern=NULL, all.files=FALSE,
                           full.names=FALSE)

tail(namesoflines, 20)


for (i in 94:1) {
  
 ## 5months per round: 470 files / 94 = 5
  
  parallel::detectCores()
  n.cores <- 30
  
  my.cluster <- parallel::makeCluster(
    n.cores, 
    type = "PSOCK"
  )
  
  doParallel::registerDoParallel(cl = my.cluster)
  foreach::getDoParRegistered()
  foreach::getDoParWorkers() 
  
 devide <- 470/94
 devide

 files_per_round <- namesoflines[(((i-1)*devide)+1):(i*devide)]
  files_per_round

  ## Foreach loop to create dfs
  
  dj_list <- list()
  
foreach(j =  1:length(files_per_round, .packages = c("stringr", "dplyr", "XML", "tibble", "data.table"))) %dopar% { ## needs approx. 5h per file
    dj_list[j]<- get_DJ(files_per_round[j])
  }
  
  dj_data <-  dj_list %>%
    bind_rows() %>%
    as.data.table()
  
  save(dj_data, file = paste0("../data/datatable/dj_data/data_" , i, ".rds"))
  
  rm(dj_data)
  gc()
  
  ## End Parallel Backend
  
  parallel::stopCluster(cl = my.cluster)
  
  
  }


parallel::stopCluster(cl = my.cluster)
.rs.restartR()
