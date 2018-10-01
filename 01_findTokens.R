rm(list=ls())
library(tidyverse)
library(plotly)

########## this script is to find the top size tokens and copy them to the 01_data folder  #####


dataFolder = "Ethereum token graphs/"

fileList <- list.files(dataFolder,pattern = "^network",full.names = TRUE)
fileList[sapply(fileList, file.size)>8000000]


tmp = tibble(token = str_remove(fileList,dataFolder), size = file.size(fileList)/1e6 )
tmp

tmp %>% arrange(desc(size)) %>% pull(token) %>% head(50) %>% str_remove(.,"network") %>% str_remove(.,"TX.txt")


topList = tmp %>% arrange(desc(size)) %>% pull(token) %>% head(30)

outFolder = "01_data/"

