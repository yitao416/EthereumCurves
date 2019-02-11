
rm(list = ls())

source("ob_RFmodel.R")
# output result is rf_xxxx.rds

library(parallel)
numCores <- detectCores()
numCores

ifolder = "00_Rdata/"
fmls = c(M1="~ vertexNum + edgeNum + clusterCoef + openNorm",
         M2="~ vertexNum + edgeNum + clusterCoef + openNorm + B0",
         M3="~ vertexNum + edgeNum + clusterCoef + openNorm + B0 + B1",
         M4="~ vertexNum + edgeNum + clusterCoef + openNorm + B0 + B1 + B2")

files = list.files(ifolder,"df.*25")
# files = list.files(ifolder,"df.*20")
tks = files %>% str_extract("(?<=_).*(?=\\.)")

mclapply(1:length(files),function(i){
  tkpred = RFmodel$new(tks[i],paste0(ifolder,files[i]))
  # tkpred$df
  # tkpred$showVarNames()
  # tkpred$flags
  # tkpred$dftrain
  # tkpred$dftest
  
  # tkpred$RFpred(fmls)
  # tkpred$outputRDS(ifolder) 
  
  # just run one time
  tkpred$RFpred(fmls,repNum=1)
  tkpred$outputRDS("01_Rdata/")
  tkpred$RF_singleRun(fmls,"flag2","01_Rdata/")
  
  message(tks[i],"\t completed.")
}, mc.cores = numCores)


