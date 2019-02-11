# This file is for:
#

rm(list=ls())

source("ob_cointeg.R")
library(tidyverse)

dataFolder = "00_Rdata/"
prefix = "partCoin_"

tokenFiles = list.files(dataFolder,pattern = paste0(prefix,".*"))
tokenLoc = paste0(dataFolder,tokenFiles)
tokenLoc

b0 = read_rds(tokenLoc[1])
b1 = read_rds(tokenLoc[2])
b2 = read_rds(tokenLoc[3])
price = read_rds(tokenLoc[4])

b2$coinTest_p1 %>% sum
b2$coinTest_p2 %>% sum

# table(price$coinTest_p2)

priceCoin =  price %>% filter(coinTest_p1==TRUE,coinTest_p2==TRUE) %>% 
  filter(x!="openNorm")

priceCoin2 = price %>% filter(coinTest_p1==TRUE) %>% 
  filter(x!="openNorm")



tk_plus = price %>% filter(x=="openNorm_plus",y=="openNorm_plus",coinTest_p2==TRUE)
tk_minus = price %>% filter(x=="openNorm_minus",y=="openNorm_minus",coinTest_p2==TRUE)

tkcoin = bind_rows(tk_plus,tk_minus)
tkcoin = tkcoin %>% arrange(x_tk,y_tk)

shockscoin = tkcoin %>% group_by(x_tk,y_tk) %>% summarise(n=n()) %>% arrange(-n) %>% filter(n==2) %>% ungroup()



library(igraph)

priceCoin


gdf = graph.data.frame(priceCoin %>% select(x_tk,y_tk),directed = FALSE)
ecount(gdf)
plot(gdf,vertex.size=0,vertex.label.font=1,vertex.label.cex = 1.5)


gdf = graph.data.frame(priceCoin2 %>% select(x_tk,y_tk),directed = FALSE)
plot(igraph::simplify(gdf),vertex.size=0,vertex.label.font=1,vertex.label.cex = 1.5)


b0 %>% filter(coinTest_p1==TRUE)
b1 %>% filter(coinTest_p1==TRUE)
b2 %>% filter(coinTest_p1==TRUE)


sug_b0 = b0 %>% filter(coinTest_p1==TRUE)
sug_b1 = b1 %>% filter(coinTest_p1==TRUE)
sug_b2 = b2 %>% filter(coinTest_p1==TRUE)

sug = bind_rows(sug_b0,sug_b1,sug_b2)
sug = sug %>% arrange(x_tk,y_tk)
sug %>% distinct(x_tk,x,y_tk,y)

sug_tk = sug %>% group_by(x_tk,y_tk) %>% summarise(n=n()) %>% ungroup()

bench = price %>% filter(coinTest_p2==TRUE)
bench_tk = bench %>% group_by(x_tk,y_tk) %>% summarise(n=n()) %>% ungroup()

sug_true = inner_join(sug_tk,bench_tk,by=c("x_tk","y_tk"))

gdf = graph.data.frame(sug_true %>% select(x_tk,y_tk),directed = FALSE)
ecount(gdf)

plot(gdf,vertex.size=0,vertex.label.font=1,vertex.label.cex = 1.4,layout=layout_as_star(gdf))


gdf = graph.data.frame(bind_rows(priceCoin,sug_true) %>% select(x_tk,y_tk),directed = FALSE)
ecount(gdf)
vcount(gdf)
plot(gdf,vertex.size=0,vertex.label.font=1,vertex.label.cex = 1.4)

plot(gdf,vertex.size=0,vertex.label.font=1,vertex.label.cex = 1.4,layout=layout_as_star(gdf))



tk
sug

price$coinTest_p2 %>% sum



