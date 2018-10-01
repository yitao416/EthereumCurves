rm(list=ls())
library(tidyverse)
library(gridExtra)
library(grid)
library(reshape)
library(data.table)
library(pROC)
library(plotly)
library(klaR)


df = read_rds("rollDepthPriceEdge.rds")
df

#  narrow down days

# datedf = df[df$betti=="B0",] %>% group_by(Date) %>% summarise(n=n()) %>% ungroup()
# 
# a <- list(
#   x = "2017-07-01",
#   y = datedf[datedf$Date =="2017-07-01",]$n,
#   text = "The starting point of pred model",
#   xref = "x",
#   yref = "y",
#   showarrow = TRUE,
#   arrowhead = 7,
#   ax = 20,
#   ay = -40
# )
# 
# p = plot_ly(datedf,x=~Date,y=~n) %>% 
#   layout(title="valid token count on each day,",annotations = a)
# 
# p
# htmlwidgets::saveWidget(p,"tokenPriceOverview.html")

# the starting date is "2017-07-01"

df = df %>% filter(Date >= "2017-07-01")
df
## find the flaged days abs
# 
# R7B0Neg15tb = df %>% filter(betti=="B0",rdChange< -0.15)
# R7B0Neg15tb2 = R7B0Neg15tb %>% group_by(Date) %>% summarise(count = n()) %>% filter(count >= 9)

# R7B0Neg15tb2 = R7B0Neg15tb %>% group_by(Date) %>% summarise(count = n())

## find the flaged days relative
thres = 1/4
datedf = df[df$betti=="B0",] %>% group_by(Date) %>% summarise(alltoken=n()) %>% ungroup()
datedf = datedf %>% mutate(mintks = alltoken*thres)

R7B0Neg15tb = df %>% filter(betti=="B0",rdChange< -0.15)

R7B0Neg15tb2 = R7B0Neg15tb %>% group_by(Date) %>% summarise(abn_count = n())
R7B0Neg15tb2 = R7B0Neg15tb2 %>% left_join(datedf,by="Date")


R7B0Neg15 = R7B0Neg15tb2 %>% filter(abn_count>=mintks) %>% pull(Date)
message("The percentage of abnormal betti days:")
R7B0Neg15 %>% length() / (df %>% pull(Date) %>% unique() %>% length())


## pred model process
source("Functions_081218.R")
typeName <- "R7B0Neg15"

tmp = predModel(Dates.x=R7B0Neg15,typeName,depthRes=df)

##  token performance

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
