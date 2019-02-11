# This file is for:
# visulize result

rm(list=ls())
ifolder = "01_Rdata/" # for paper
source("ob_RFres.R")

files = list.files(ifolder,"rf.*25")
tks = files %>% str_extract("(?<=_).*(?=\\.)")


res = RFres$new(ifolder,"rf_.*25")
res$files
res$ifolder
res$tks

# res$showAllMeasure()
res$setMeasure(c("Accuracy","Sensitivity","Specificity"))
res$avgAllToken()
# res$outputPlots("06_Summary/")
res$outputPlots("07_paperPlots/")

# available measures
# [1] "Accuracy"             "Kappa"                "AccuracyLower"        "AccuracyUpper"        "AccuracyNull"        
# [6] "AccuracyPValue"       "McnemarPValue"        "Sensitivity"          "Specificity"          "Pos Pred Value"      
# [11] "Neg Pred Value"       "Precision"            "Recall"               "F1"                   "Prevalence"          
# [16] "Detection Rate"       "Detection Prevalence" "Balanced Accuracy"   


# analysis the details


rm(list=ls())
ifolder = "01_Rdata/" # for paper


files = list.files(ifolder,"rf.*25")
tks = files %>% str_extract("(?<=_).*(?=\\.)")

tkrank = read_rds("tokenRank.rds")
tkrank

tks
files


para_measure = "Accuracy"
para_horizon = "flag2"

df = map_dfr(paste0(ifolder,files), function(f){
  read_rds(f) %>% filter(measure == para_measure,horizon==para_horizon)
})

df_tidy = df %>% mutate(token = str_remove(tkname,"_abs25")) %>% select(token,value,modelType)
df_tidy = df_tidy %>% spread(key=modelType,value=value)
df_res = df_tidy %>% left_join(tkrank) %>% arrange(rank) 

df_res

write.csv(df_res,paste0(ifolder,"accTable_",para_horizon,".csv"))


### WEEKLY ACC

rm(list=ls())
ifolder = "01_Rdata/" # for paper
para_flag = "flag2"

files = list.files(ifolder,paste0("predDF.*25.*",para_flag))
tks = files %>% str_extract("(?<=_).*(?=\\.)")

df = read_rds(paste0(ifolder,files[1]))
df
df %>% select(tk,day,para_flag,M1:M4)

df = map_dfr(paste0(ifolder,files),function(f){
  read_rds(f) %>% select(tk,day,para_flag,M4)
})
df = df %>% filter(day>"2017-10-20")
df2 = df %>% mutate(weekNum = lubridate::week(day))



measureFun2 <- function(pred,ref,para = "Accuracy"){
  
  cf = caret::confusionMatrix(pred,ref,positive = "TRUE")
  cfmatrix = rbind(as.matrix(cf,what="overall"),
                   as.matrix(cf,what="classes"))
  value = cfmatrix[para,] %>% as.numeric()
  return(rep(value,length(pred)))
}
library(lubridate)

df_wk = df2 %>% group_by(tk,weekNum) %>% mutate(acc = measureFun2(M4,flag2)) %>% ungroup()
df_wk = df_wk %>% mutate(newDay = as.Date(paste(year(day),weekNum-1,1,sep="-"),"%Y-%U-%u"))

df_wk2 = df_wk %>% group_by(tk) %>% distinct(newDay,.keep_all=TRUE) %>% ungroup()
mw2 = df_wk2 %>% group_by(newDay) %>% summarise(avg = mean(acc,na.rm = TRUE),std = sd(acc,na.rm = TRUE))
df_wk2 %>% group_by(newDay) %>% summarise(n=n())

pricedf = read_rds("pred_data01.rds")

pricedf2 = pricedf %>% group_by(Date) %>% summarise(priceAvg = mean(openNorm,na.rm = T),std = sd(openNorm,na.rm = T))
pricedf3 = pricedf2 %>% filter(Date>"2017-10-20")


gwek = ggplot() + 
  geom_ribbon(data = mw2, aes(x=newDay,ymin = avg - 0.5*std, ymax = avg + 0.5*std, alpha=0.3), fill = "lightblue2") + 
  geom_ribbon(data = pricedf3, aes(x=Date,ymin = priceAvg - 0.5*std, ymax = priceAvg + 0.5*std, alpha=0.3), fill = "orange") +
  geom_line(data = mw2,aes(x=newDay,y=avg),size=0.8,color="lightblue4")  +
  geom_line(data = pricedf3,aes(x=Date,y=priceAvg),size=0.8,color="orange3") +
  scale_x_date(name="Date",date_breaks = "5 weeks",date_labels = "%m/%d/%y") + ylab("accuracy") +
  theme_minimal() +  theme(legend.position="none",text = element_text(size=22)) +
  annotate('text',x=as.Date("2017-11-10"),y=0.4,label="Average token price",size=6) +
  annotate('segment',x=as.Date("2017-11-11"),y=0.35,xend = as.Date("2017-12-01"),yend = 0.2,arrow=arrow()) + 
  annotate('text',x=as.Date("2018-03-26"),y=0.65,label="Weekly average accuracy",size=6) +
  annotate('segment',x=as.Date("2018-02-26"),y=0.67,xend = as.Date("2018-02-10"),yend = 0.85,arrow=arrow())

gwek

ggsave("07_paperPlots/8weeklyAccAbs25.png",gwek,width = 12,height = 7)



## horizon plot

rm(list=ls())
library(tidyverse)
pricedf = read_rds("pred_data01.rds")
df = pricedf %>% mutate(abn = ifelse(abs(priceReturn)>=0.25,TRUE,FALSE))

dfplot = df %>% group_by(Date) %>% summarise(tkcount = sum(abn,na.rm = TRUE)) %>% ungroup()

g = ggplot(dfplot %>% filter(Date>"2016-11-21"),aes(x=Date,y=tkcount)) +
  geom_bar(stat = 'identity',color='lightsteelblue4', fill='lightsteelblue4') +
  scale_x_date(name="Date",date_breaks = "3 months",date_labels = "%b %y") +
  ylab("Anomalous Token Count") +
  theme_minimal() +theme(text = element_text(size=22))

ggsave("07_paperPlots/10dateAnomaly.png",g,width = 10,height = 6)

## token price horizon 

rm(list=ls())
library(tidyverse)
ifolder = "01_Rdata/" # for paper
para_flag = "flag2"

files = list.files(ifolder,paste0("predDF.*25.*",para_flag))
tks = files %>% str_extract("(?<=_).*(?=\\.)")

df = map_dfr(paste0(ifolder,files),function(f){
  read_rds(f) %>% select(tk,day,para_flag,M4)
})
df = df %>% filter(day>"2017-10-20")
df
df = df %>% mutate_if(is.factor,as.logical)
df
df %>% filter(M4==TRUE)
df %>% filter(day=="2018-01-29")

df_abnormalPlot = df %>% group_by(day) %>% summarise(abnormalNum=sum(flag2,na.rm = TRUE)) %>% ungroup()
dfpred_correctplot = df %>% group_by(day) %>% summarise(abnormalNum=sum(M4,na.rm = TRUE)) %>% ungroup()


ggplot(df_abnormalPlot,aes(x=day,y=abnormalNum)) + geom_line()

tmp = ggplot(data = df_abnormalPlot,aes(x=day,y=abnormalNum)) + 
  geom_bar(stat = 'identity',color='dodgerblue1', fill='dodgerblue1') + 
  geom_point(data = dfpred_correctplot,
             aes(x=day,y=abnormalNum),size=4,shape=8,alpha=0.7, colour="firebrick1") + 
  scale_x_date(name="Date",date_breaks = "1 months",date_labels = "%b %y") +
  ylab("Token Count") + scale_y_continuous(breaks=seq(0,10,2)) +
  theme_minimal() +theme(text = element_text(size=22))

tmp

ggsave("07_paperPlots/10Horizon_flag2_M4.png",tmp,width = 12,height = 7)

df
df %>% filter(flag2==TRUE) # total 226 true abnormla
df %>% filter(flag2==TRUE,M4==TRUE) # predicted 86
df %>% filter(M1==TRUE)
df %>% filter(M4==TRUE)

### arima average acc

files = list.files("00_Rdata/","lagPref_")
files

df = read_rds(paste0("00_Rdata/",files[[1]]))
df = map_dfr(files,function(f){
  read_rds(paste0("00_Rdata/",f)) %>% filter(measure =="Accuracy")
})
df
res = df %>% group_by(flag,diff,modelType) %>% summarise(avg=mean(value,na.rm = TRUE))



## ROC plots


rm(list=ls())
ifolder = "01_Rdata/" # for paper
library(tidyverse)

files = list.files(ifolder,"rf.*25")
tks = files %>% str_extract("(?<=_).*(?=\\.)")

df = read_rds(paste0(ifolder,files[[1]]))


df$measure %>% unique()

df = map_dfr(files,function(f){
  read_rds(paste0(ifolder,f)) %>% filter(measure%in% c("Sensitivity","Specificity"))
})

df = df %>% spread(measure,value = value)

df = df %>% mutate(FNR = 1- Specificity)

dfplot = df %>% group_by(horizon,modelType) %>% 
  summarise(avgSen = mean(Sensitivity,na.rm = TRUE),avgFNR = mean(FNR,na.rm = TRUE))

dfline = tibble(x1 = 0,y1=0,x2=0.4,y2=0.4)

g = ggplot() + geom_point(data = dfplot,aes(x=avgFNR,y=avgSen,color=horizon,shape=modelType),size=2) + 
  geom_segment(data=dfline,aes(x=x1,y=y1,xend=x2,yend=y2),linetype="dashed") +
  labs(x="False positive rate",y="True positive rate") +
  theme_minimal() +
  theme(text = element_text(size=15))

ggsave("plot_ROC.png",g,width = 8,height = 6)



# per token 


rm(list=ls())
ifolder = "01_Rdata/" # for paper
library(tidyverse)

files = list.files(ifolder,"rf.*25")
tks = files %>% str_extract("(?<=_).*(?=\\.)")

findex = files %>% str_detect("bnb")

df = read_rds(paste0(ifolder,files[findex]))


df = df %>% filter(measure%in% c("Sensitivity","Specificity"))

df = df %>% spread(measure,value = value)
df = df %>% mutate(FNR = 1- Specificity)
df = df %>% mutate(model=modelType)
df = df %>% mutate(horizon = str_replace(horizon,"flag","h="))

tk = df$tkname %>% unique()
dfline = tibble(x1 = 0,y1=0,x2=0.6,y2=0.6)
g = ggplot() + geom_line(data = df,aes(x=FNR,y=Sensitivity,color=model)) +
  geom_segment(data=dfline,aes(x=x1,y=y1,xend=x2,yend=y2),linetype="dashed") +
  geom_point(data = df,aes(x=FNR,y=Sensitivity,shape=horizon),size=3) + 
  scale_shape_manual(values=1:7) + 
  labs(x="False positive rate",y="True positive rate") +
  theme_minimal() +
  theme(text = element_text(size=22))
g

ggsave("07_paperPlots/9ROC_bnb_abs25.png",g,width = 8,height = 6)

walk(files,function(f){
  df = read_rds(paste0(ifolder,f))
  df = df %>% filter(measure%in% c("Sensitivity","Specificity"))
  
  df = df %>% spread(measure,value = value)
  df = df %>% mutate(FNR = 1- Specificity)
  df = df %>% mutate(model=modelType)
  df = df %>% mutate(horizon = str_replace(horizon,"flag","h="))
  
  tk = df$tkname %>% unique()
  dfline = tibble(x1 = 0,y1=0,x2=0.8,y2=0.8)
  g = ggplot() + geom_line(data = df,aes(x=FNR,y=Sensitivity,color=model)) +
    geom_segment(data=dfline,aes(x=x1,y=y1,xend=x2,yend=y2),linetype="dashed") +
    geom_point(data = df,aes(x=FNR,y=Sensitivity,shape=horizon),size=3) + 
    scale_shape_manual(values=1:7) + 
    labs(x="False positive rate",y="True positive rate") +
    theme_minimal() +
    theme(text = element_text(size=22))
  
  ggsave(paste0("07_paperPlots/","ROC_",tk,".png"),g,width = 8,height = 6)
})


# pick tronix

fid = files %>% str_detect("tronix")

df = read_rds(paste0(ifolder,files[fid]))
df


df = df %>% filter(measure%in% c("Sensitivity","Specificity"))

df = df %>% spread(measure,value = value)
df = df %>% mutate(FNR = 1- Specificity)
df


tk = df$tkname %>% unique()
dfline = tibble(x1 = 0,y1=0,x2=0.8,y2=0.8)
ggplot() + geom_line(data = df,aes(x=FNR,y=Sensitivity,linetype=modelType)) 



ggplot() + geom_line(data = df,aes(x=FNR,y=Sensitivity,color=modelType)) +
  geom_segment(data=dfline,aes(x=x1,y=y1,xend=x2,yend=y2),linetype="dashed") +
  labs(title =tk ,x="False positive rate",y="True positive rate") +
  xlim(c(0,0.8)) + ylim(0,0.8)+
  theme_minimal() +
  theme(text = element_text(size=15))


g = ggplot() + geom_point(data = df,aes(x=FNR,y=Sensitivity,color=modelType)) +
  geom_segment(data=dfline,aes(x=x1,y=y1,xend=x2,yend=y2),linetype="dashed") +
  labs(title =tk ,x="False positive rate",y="True positive rate") +
  theme_minimal() +
  theme(text = element_text(size=15))
g



# tronix ; storj; bnb


fid = files %>% str_detect("tronix|storj|bnb")

df = map_dfr(files[fid],function(f){
  read_rds(paste0(ifolder,f)) %>% filter(measure%in% c("Sensitivity","Specificity"),modelType=="M4")
})

df = df %>% spread(measure,value = value)
df = df %>% mutate(FNR = 1- Specificity)
df = df %>% mutate(token = str_remove(tkname,"_abs25"))
df = df %>% mutate(horizon = str_replace(horizon,"flag","h="))
dfline = tibble(x1 = 0,y1=0,x2=0.8,y2=0.8)

g = ggplot() + geom_line(data = df,aes(x=FNR,y=Sensitivity,color=token)) +
  geom_segment(data=dfline,aes(x=x1,y=y1,xend=x2,yend=y2),linetype="dashed") +
  geom_point(data = df,aes(x=FNR,y=Sensitivity,shape=horizon),size=3) + 
  scale_shape_manual(values=1:7) + 
  labs(x="False positive rate",y="True positive rate") +
  coord_cartesian(xlim = c(0,0.7),ylim=c(0,0.7))+
  scale_x_continuous(breaks = seq(0,0.7,0.1))+
  scale_y_continuous(breaks = seq(0,0.7,0.1))+
  theme_minimal() +
  theme(text = element_text(size=22))

ggsave("07_paperPlots/ROC_3tokens.png",g,width = 8,height = 6)

g = ggplot() + geom_point(data = df,aes(x=FNR,y=Sensitivity,color=modelType)) +
  geom_segment(data=dfline,aes(x=x1,y=y1,xend=x2,yend=y2),linetype="dashed") +
  labs(title =tk ,x="False positive rate",y="True positive rate") +
  theme_minimal() +
  theme(text = element_text(size=15))
g




# Improvement Accuracy / Sensitivity /  Specificity /Precision

rm(list=ls())
ifolder = "01_Rdata/" # for paper
library(tidyverse)

files = list.files(ifolder,"rf.*25")
tks = files %>% str_extract("(?<=_).*(?=\\.)")


measureStr = "Precision"  # replace here

df = map_dfr(files,function(f){
  read_rds(paste0(ifolder,f)) %>% filter(measure == measureStr)
})

dfs = df %>% group_by(horizon,modelType) %>% summarise(avg = mean(value,na.rm = TRUE)) %>% ungroup()

dfs = dfs %>% spread(modelType,avg)
dfs
#  formula: 1 - M1/M4

gain = function(x,ben){
  return(1-ben/x)
}

dfplot = map_dfc(paste0("M",2:4),function(x){
  res_tb = tibble(gain(dfs %>% pull(x),
                       dfs$M1))
  names(res_tb) = paste0(x,"_gain")
  res_tb
})
dfplot = bind_cols(dfs %>% select(horizon),dfplot)
dfplot = dfplot %>% gather(M2_gain:M4_gain,key="type",value='value')
dfplot2 = dfplot %>% mutate(horizon=str_remove(horizon,"flag"),type=str_remove(type,"_gain"))

g = ggplot(data = dfplot2,aes(x=horizon,y=value,fill = type)) + geom_bar(colour="black", position="dodge",stat = "identity") + 
  scale_fill_brewer() + xlab("Prediction horizon (unit: day)") + ylab(paste("Gain in:",measureStr)) + 
  theme_minimal() + theme(text = element_text(size=22),legend.position="bottom")
g
ggsave(paste0("07_paperPlots/7_",measureStr,".png"),g,width = 8,height = 6)

