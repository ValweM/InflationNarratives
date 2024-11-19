
# create final dataframe by binding rows of rds-files
Sys.setenv(LC_ALL="en_US.UTF-8")
rm(list=ls())


mypackages <- c("tibble", "dplyr", "stringr", "foreach", "doParallel", "data.table", "XML", "rvest")
lapply(mypackages, require, character.only = TRUE)



options(stringsAsFactors = F)

getwd()


### Read in the rds files #####

files <- list.files(path="../data/datatable/dj_data/", pattern=NULL, all.files=FALSE,
                    full.names=FALSE)

files <- files[27:10]


list_Newswire <- list()

for (i in 1:length(files)) {

path <- paste0( "../data/datatable/dj_data/", files[i])
load(file = path)
list_Newswire[[i]] <- dj_data

rm(dj_data)
gc()

}



dj_news <-  list_Newswire%>%
  bind_rows() %>%
  as.data.table()

rm(list_Newswire)
gc()



save(dj_news, file ="../data/datatable/dj_news_raw.rds")


