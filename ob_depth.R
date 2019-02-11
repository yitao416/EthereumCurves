
library(tidyverse)
library(depthTools)

depthAnalysis = R6::R6Class("depthAnalysis", public = list(
  
  joinFolder = NULL,
  initialize = function(){},
  collectAllBettiFiles = function(ifolder,ofolder){
    #ifolder: the parent folder of all tokens
    
    dir.create(ofolder,showWarnings = FALSE)
    self$joinFolder = ofolder
    
    tokenList <- list.files(ifolder)
    
    for (f in tokenList) {
      bettiLoc = paste0(ifolder,f,"/06_joinBetti/")
      bList = list.files(bettiLoc)
      
      for (b in bList) {
        # skip Betti 3 files, which are all zeros
        if (str_detect(b,"B3")) {
          next()
        }
        file.copy(paste0(bettiLoc,b),ofolder)
      }
      
    }
    invisible(self)
  },
  
  rollDepthPerFile = function(f,rollSize=7){
    message(f)
    message(Sys.time())
    df = read_csv(paste0(self$joinFolder,f),col_types = cols(Time=col_character(),.default = col_integer()))
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
    write_rds(res,paste0("00_Rdata/","rd_",f,".rds"))
    return(res)
  },
  
  rollDepth = function(resFile="rollDepth7.rds"){
    fileList = list.files(self$joinFolder)
    res = map_dfr(fileList,self$rollDepthPerFile)
    write_rds(res,paste0("00_Rdata/",resFile))
    invisible(self)
  }
  
))