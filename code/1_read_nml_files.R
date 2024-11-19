## Overview over Code

## (1) Read in nml files as xml files
## (2) Create smaller RData files out of xml files 
## (3) Parse and create dfs



# reading nml files of DJ corpus
Sys.setenv(LANG = "en")
rm(list=ls())
gc()


## define packages
mypackages <- c("tibble", "dplyr", "stringr", "foreach", "doParallel", "nml", 
  "XML", "rvest")

## load required packages
lapply(mypackages, require, character.only = TRUE)  
# devtools::install_github("jsta/nml")


## set global optiosn
options(stringsAsFactors = F)

getwd()


## load function file
source("./functions.R")

## Read in the nml files

files <- list.files(path="../data/data", pattern=NULL, all.files=FALSE,
           full.names=FALSE)

## Check for duplicates
which(duplicated(files)==TRUE)


## 
for (i in 198:length(files)) {
  read_DJ(files[i])
}


## Vector of names of lines

namesoflines <- list.files(path="../data/dfs", pattern=NULL, all.files=FALSE,
                    full.names=FALSE)

namesoflines

## Create smaller Files ####


for (i in 1:length(namesoflines)) {
  devide_files(namesoflines[i])
  
}



