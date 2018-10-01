rm(list=ls())
library(tidyverse)

#### add three variable to our orignal data set: 
#### 1. Whether next three days will have price change
#### 2. week day
#### 3. normalized price (open)

df = read_rds("rollDepthPriceEdge.rds") %>% ungroup()
df

df = df %>% select(-rdChange)  ## drop the rolldepth change column
df = df %>% mutate(betti = paste0(betti,"_rd7"))

#### spread betti rolldepth to sperate columns ###########

df2 = df %>% spread(key= betti,value = rollDepth7)
df2

#### flag days funs : return a vector #########

flagDaysFun2 <- function(priceReturn, ndays = 3,threshod=0.15) {
  # input the vector of the priceReturn and flag today based on next ndays
  # ndays = 3  obsever price change in next ndays
  # pr: priceReturn
  prABS = abs(priceReturn)
  l = length(priceReturn)
  flagDays = vector(length = l) # declear an empty 0 vector with same length
  for (i in 1:(l-1)) {
    maxChange = max(priceReturn[(i+1):(i+ndays)], na.rm = TRUE)
    flagDays[i] = ifelse(maxChange>=threshod,TRUE,FALSE)
  }
  return(flagDays)
}


df3 = df2
## flag 1 to 7 advance, dynamic assign col name

for (x in 1:7) {
  df3 = df3 %>% group_by(token) %>% mutate(!!(paste0("flag",x)) := flagDaysFun2(priceReturn,ndays = x))
}

df3


##### add 3. normalize open ########

df4 = df3 %>% group_by(token)%>% mutate(openNorm = Open/max(Open,na.rm = T)) %>% ungroup()

df4 %>% tail(20)

df4 = df4 %>% select(Date:Open,openNorm,everything())
df4


#### convert flag to factor and with specific levels #####
#### ready for the prediciton and confusion matrix ###

df5 = df4 %>% mutate_at(vars(flag1:flag7),factor,levels=c("TRUE","FALSE"))
df5

levels(df5$flag1)

write_rds(df5,"pred_data02.rds")

