rm(list=ls())
library(tidyverse)
library(gridExtra)
library(grid)
library(reshape)
library(data.table)
library(pROC)
library(plotly)

errormatrix <- klaR::errormatrix


df = read_rds("rollDepthPriceEdge.rds")
# the starting date is "2017-07-01"

df = df %>% filter(Date >= "2017-07-01")
df


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

R7B0Neg15train = R7B0Neg15[R7B0Neg15<"2018-02-01"]
dftrain = df[df$Date<"2018-02-01",]


depth = dftrain
Date.x = R7B0Neg15train[1]
pred = "rollDepth7"

change = 0.15; nahead = 3; thresh = 0.5

Dates.y = Date.x + seq(1:nahead)#select 3 dates ahead of the date with abnormal behavior
df.x <- subset(depth, Date == Date.x)
df.x <- df.x[,c("Date",  "token", "priceReturn", pred, "betti","edge","Open")] ### add new col

tokens <- unique(df.x$token)

#data for same tokens a day before
df.y <- subset(subset(depth, Date %in% Dates.y))
df.y <- df.y[,c("Date",  "token", "priceReturn", pred, "betti","edge","Open")] ### add new col
df.y <- subset(df.y, token %in% tokens)

#recode price change 
price <- subset(df.y, betti == "B0")
price.bin <- data.frame(y=logical(), token=numeric(), Date.y=as.Date(character()))
for(i in 1:length(tokens)){
  y <- price[price$token == tokens[i], c("priceReturn", "token", "Date")]
  ind <- which(abs(y$priceReturn) > change)
  if(length(ind) > 0){y <- y[ind[1], ]
  y$priceReturn <- 1} #take 1st date with price return > change
  else{y <- y[1,]
  y$priceReturn <- 0}
  price.bin <- rbind(price.bin, y)
}
#merge with df.x
df <- merge(df.x, price.bin, by = "token")
df <- df[complete.cases(df),]
df


if(sum(df$priceReturn.y) > 0){
  data <- data.frame(y = subset(df, betti == "B0")[,which(names(df) == "priceReturn.y")], 
                     B0 = subset(df, betti == "B0")[,which(names(df) == pred)],
                     B1 = subset(df, betti == "B1")[,which(names(df) == pred)],
                     B2 = subset(df, betti == "B2")[,which(names(df) == pred)],
                     edge = subset(df,betti == "B0")[,which(names(df) == "edge")],
                     openPrice = subset(df,betti == "B0")[,which(names(df) == "Open")],
                     priceReturn.x = subset(df, betti == "B0")[,which(names(df) == "priceReturn.x")],
                     Date.x = subset(df, betti == "B0")[,which(names(df) == "Date.x")],
                     Date.y = subset(df, betti == "B0")[,which(names(df) == "Date.y")],
                     token =  subset(df, betti == "B0")[,which(names(df) == "token")])
  fit <- glm(y ~ B0 + B1 + B2, data = data, family=binomial)
  # print("fit over")
  fit2 <- glm(y~ B0+ B1 + B2 +edge,data = data, family=binomial)
  # print("fit2 over")
  fit3 <- glm(y~ edge,data = data, family=binomial)
  
  fit4 <- glm(y~ openPrice,data = data, family=binomial)
  
  ncases <- c(sum(data$y), length(data$y))
}
