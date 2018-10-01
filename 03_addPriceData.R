rm(list=ls())
library(tidyverse)
library(plotly)


#### first combine two sets of token depths #########

rollDF1 = read_rds("rollDepth7_tks.rds")
rollDF2 = read_rds("rollDepth7_tks_2.rds")

rollDF = bind_rows(rollDF1,rollDF2)

## remove beautychain1, rep, because it has not price data

rollDF = rollDF %>% filter(token != "beautychain1")

rollDF = rollDF %>% filter(token != "rep")

tks = rollDF %>% pull(token) %>% unique()
tks
tks %>% length()

#### find all price files and move them to 03_tokenPrice folder #######

outFolder = "03_tokenPrice/"
inFolder = "Priced/"


# search sub folder first 

flist = list.files(paste0(inFolder,"crawler/"))

for (f in flist) {
  tkCur = f %>% str_to_lower()
  if (tkCur %in% tks) {
    file.copy(paste0(paste0(inFolder,"crawler/"),f),paste0(outFolder,f))
  }
}


###### search txt files

# find the remaining tks
tksChecked = list.files(outFolder) %>% str_to_lower()
tksUnchecked = setdiff(tks,tksChecked)


flist = list.files(inFolder,".txt")

for (f in flist) {
  tkCur = f%>% str_remove(".txt") %>% str_to_lower()
  if (tkCur %in% tksUnchecked) {
    file.copy(paste0(inFolder,f),paste0(outFolder,f))
    tksUnchecked = setdiff(tksUnchecked,tkCur)
  }
}

## note that: ployai is ploy-ai in the price folder


####### combine all price files ##############

inFolder = "03_tokenPrice/"
flist = list.files(inFolder)

f = flist[3]
f

tkName = f %>% str_remove(".txt") %>% str_to_lower()

tkPrice = read_tsv(paste0(inFolder,f),col_types = cols(Date = col_date(format = "%m/%d/%Y"))) %>% arrange(Date)


priceFun <- function(f,inFolder =  "03_tokenPrice/"){
  tkName = f %>% str_remove(".txt") %>% str_to_lower()
  tkPrice = read_tsv(paste0(inFolder,f),col_types = cols(Date = col_date(format = "%m/%d/%Y"))) %>% arrange(Date)
  res = tkPrice %>% select(Date,Open) %>% mutate(token=tkName)
  return(res)
}

tmp = priceFun(f)
tmp = map(flist,priceFun)
tkPrice = bind_rows(tmp)
tkPrice %>% pull(token) %>% unique() %>% length()

source("00_simpleFunctions.R")

tkPrice2 = tkPrice %>% group_by(token) %>% mutate(priceReturn = simpleReturn(Open)) %>% ungroup()
tkPrice2
tkPrice2 %>% filter(token == "request")


#######join with depth ##########


### single case test
rd_tmp = rollDF %>% filter(token=="civic")
tkp_tmp = tkPrice2 %>% filter(token =="civic")

res_tmp = inner_join(rd_tmp,tkp_tmp,by=c("Date","token"))
res_tmp
res_tmp %>% filter(betti =="B1")


### combine with inner join, ensure to have both depth and price


depthPrice = inner_join(rollDF,tkPrice2,by=c("Date","token"))
depthPrice %>% filter(token=="civic") %>% tail()

## reorder columns

res = depthPrice[,c(1,2,3,5,4,6,7)]
res
write_rds(res,"rollDepthPrice.rds")


######## add edges count

tks = res %>% pull(token) %>% unique()
tks %>% length()

gmdata = read_tsv("graphMetrics.txt")

gmdata %>% pull(token) %>% unique() %>% length()

gm = gmdata %>% filter(token %in% tks)
gm %>% pull(token) %>% unique() 

gm = gm %>% mutate(Date = as.Date(paste0(year,"-",day),"%Y-%j")) %>% select(-(year:day))
gm1 = gm %>% select(token,edge,Date)
gm1

res2 = left_join(res,gm1)
res2 %>% filter(is.na(edge))
res2 %>% filter(Date == "2017-05-23")


write_rds(res2,"rollDepthPriceEdge.rds")

## token rank
gm2 = gm1 %>% group_by(token) %>% summarise(avgEdge = sum(edge,na.rm = T)/n()) %>% ungroup()
gm3 = gm2 %>% mutate(rank = rank(-avgEdge))
gm3

## save the token rank
write_rds(gm3,"tokenRank.rds")

