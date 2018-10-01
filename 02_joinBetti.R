rm(list=ls())
library(tidyverse)
library(plotly)
library(depthTools)


###### move sepearte joined betti files to one folder ############
##  better to calucate more tokens and review their final betti result and delete some tokens result ###
#### for example, to pick 30 tokens, calculate like 35 tokens. and review its final betti. 
#### Because some transaction distribution is not consistent and stable  

inputFolder = "02_output/"
outputFolder = "03_joinBetti/"

tokenList <- list.files(inputFolder)

for (f in tokenList) {
  bettiLoc = paste0(inputFolder,f,"/06_joinBetti/")
  bList = list.files(bettiLoc)
  
  for (b in bList) {
    # skip Betti 3 files, which are all zeros
    if (str_detect(b,"B3")) {
      next()
    }
    file.copy(paste0(bettiLoc,b),outputFolder)
  }
  
}


####### rolling depth per token #############
rm(list=ls())

folder = "03_joinBetti/"
fileList = list.files(folder)

rollDepthFun <- function(f,rollSize=7){
  message(f)
  message(Sys.time())
  df = read_csv(paste0(folder,f),col_types = cols(Time=col_character(),.default = col_integer()))
  data = df[,-1]
  dataN = data/apply(data,1,function(x) max(1, max(x))) # compute normalized betti numbers
  rollDepth = NULL
  
  for (i in rollSize:nrow(dataN)) {
    betti_roll = dataN[(i-(rollSize-1)):i,]  # previous rows
    depth_betti_cur=as.vector(MBD(betti_roll,plotting = F,main = f,
                                  ylab=paste0('Betti-'),xlab='Filtration threshold')$MBD)
    rollDepth[i] = depth_betti_cur[length(depth_betti_cur)] # only take the last depth value
  }
  res <- tibble(
    day = str_sub(df$Time,-8),
    rollDepth = rollDepth,
    type = str_sub(df$Time,0,-9)
  )
  return(res)
}

rollDepthList <- lapply(fileList,rollDepthFun,rollSize=7)

write_rds(bind_rows(rollDepthList),"rollDepth7.rds")


rd = read_rds("rollDepth7.rds")


################## format to standard #############
rm(list=ls())

rollDF = read_rds("rollDepth7.rds")

rollDF = rollDF %>% separate(type,into = c("betti","token"),sep = 2)

rollDF = rollDF %>% type_convert(col_types = cols(day = col_date("%Y%m%d"))) %>% rename(Date = day)

# specify to roll depth 7
rollDF = rollDF %>% rename(rollDepth7 = rollDepth)
rollDF

### depth change ######

source("00_simpleFunctions.R")

rollDF = rollDF %>% group_by(token,betti) %>% mutate(rdChange = simpleReturn(rollDepth7) )

## Check 
rollDF[,"token"] %>% unique()
tmp = rollDF %>% filter(token == "civic",betti =="B1") %>% tail()
tmp

### output

write_rds(rollDF,"rollDepth7_tks.rds")

