rm(list=ls())
library(tidyverse)
library(plotly)



tkdf = read_csv("R7B0Neg15_msclDetail.csv")
tkdf

tks = tkdf %>% pull(token) %>% unique()


msctb2 <- function(tk,modelName="predBetti", df){
  trueV = df[df$token==tk,]$trueValue
  predV = df[df$token==tk,] %>% pull(modelName)
  
  tb = klaR::errormatrix(trueV,predV)
  tb
  TP = tb[2,2] 
  TN = tb[1,1]
  FP = tb[1,2]
  FN = tb[2,1]
  
  TPR = TP/(TP+FN)
  FPR = FP/(FP+TN)
  PPV = TP/(TP+FP)
  
  res = tibble(token = tk,
               !!paste0("TPR_",modelName):=TPR,
               !!paste0("FPR_",modelName):=FPR
               # !!paste0("PPV_",modelName):=PPV
  )
  return(res)
}

tmp = map2(tks,"predBetti",msctb2,df = tkdf) %>% bind_rows()
tmp2 =  map2(tks,"predBettiEdge",msctb2,df = tkdf) %>% bind_rows()
tmp3 =  map2(tks,"predEdge",msctb2,df = tkdf) %>% bind_rows()

resDF2 = reduce(list(tmp,tmp2,tmp3),left_join,by="token")
resDF2
resDF3 = resDF2 %>% dplyr::select(token,contains("TPR"),contains("FPR"))

## add token ranks

tkrank = read_rds("tokenRank.rds")
tkrank
## add trails for tokens

tktrial = tkdf %>% group_by(token) %>% summarise(trials = n())

resDF4 = resDF3 %>% left_join(tkrank,by="token") %>% left_join(tktrial,by="token")


write_csv(resDF4,"R7B0Neg15_tksRatio.csv")


### test


