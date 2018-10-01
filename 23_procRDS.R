rm(list=ls())
library(tidyverse)

ifolder = "01_RF/"
ofolder = "02_RF_summary/"

files = list.files(ifolder,"_test.rds")

dfRaw = lapply(files, function(x){
  tmp = read_rds(paste0(ifolder,x))
}) %>% bind_rows() %>% as.tibble()


dfRaw

levels(dfRaw$flag1eoB0B1)

df = dfRaw

flags = paste0('flag',1:7)


########  functions ###########
measureFun <- function(pred,ref,para = "Accuracy"){
  cf = caret::confusionMatrix(pred,ref,positive = "TRUE")
  cfmatrix = rbind(as.matrix(cf,what="overall"),
                   as.matrix(cf,what="classes"))
  value = cfmatrix[para,] %>% as.numeric()
  return(value)
}


######### boxplot overview #######

accTokens = lapply(flags, function(flagCur){
  dfflag = df %>% select(Date:B2_rd7,matches(flagCur))
  
  dfnames = dfflag %>% names()
  pdnames = dfnames[dfnames %>% str_detect("eo")]
  
  tmp = lapply(pdnames,function(v){
    group_var <- "token"   # group by this variable
    summ <- paste0('measureFun(', v,',',flagCur, ')')  # construct summary method, e.g. mean(mpg)
    summ_name <- paste0( v,'_acc')  # construct summary variable name, e.g. mean_mpg
    
    df_summ <- dfflag %>%
      group_by_(.dots = group_var) %>%
      summarise_(.dots = setNames(summ, summ_name))
  }) %>% bind_cols()
  
  resflag = tmp %>% select(token,contains("flag"))
  
  dfplot = resflag %>% gather(key="type",value = "value",-token)
  
  gflag = ggplot(dfplot,aes(x=type,y=value)) + geom_boxplot() +
    geom_jitter(position = position_jitter(0.5),aes(colour = type),show.legend = F) +
    ggtitle(flagCur)
  
  # ggsave(paste0(ofolder,flagCur,'boxplot.png'),gflag)
  return(resflag)
})


tkrank = read_rds("tokenRank.rds")

accTokens2 = lapply(accTokens, function(x){
  left_join(x,tkrank)
}
)

writexl::write_xlsx(accTokens2,"accTokens.xlsx")

########### weekly acc ########


measureFun2 <- function(pred,ref,para = "Accuracy"){
  
  cf = caret::confusionMatrix(pred,ref,positive = "TRUE")
  cfmatrix = rbind(as.matrix(cf,what="overall"),
                   as.matrix(cf,what="classes"))
  value = cfmatrix[para,] %>% as.numeric()
  return(rep(value,length(pred)))
}


######### cal weekly acc ##########

#### add week number col
#### convert back to Date 
#### (to ensure the week start day is same for all tokens, count correctly the # token at each week)
df2 = df %>% mutate(weekNum = strftime(Date,format = "%Y-%V"),DateR = as.Date(paste(weekNum,1),format="%Y-%W %u"))
df2 %>% select(Date,token,weekNum,DateR)

flagCur

accWeek = lapply(flags, function(flagCur){
  
  dfflag = df2 %>% select(Date:token,matches(flagCur),weekNum,DateR)
  
  dfnames = dfflag %>% names()
  pdnames = dfnames[dfnames %>% str_detect("eo")]
  
  tmp = lapply(pdnames,function(v){
    
    muu <- paste0('measureFun2(', v,',',flagCur, ')')  # construct summary method, e.g. mean(mpg)
    var_name <- paste0( v,'_acc')  # construct summary variable name, e.g. mean_mpg
    
    df_muu <- dfflag %>%
      group_by(token,weekNum) %>% mutate_(.dots = setNames(muu,var_name)) %>% ungroup()
    return(df_muu %>% select(contains('Date'),token,weekNum,contains("acc")))
  })
  
  mweek = Reduce(left_join,tmp)
  
  m2 = mweek %>% group_by(token) %>% distinct(weekNum,.keep_all = T) %>% ungroup()
  m2
  
  mw = m2 %>% group_by(DateR) %>% summarise_at(.vars = vars(contains('_acc')),.funs = funs(avg = mean(.)))
  
  mw2 = mw %>% gather(key = "type",value = "value",-DateR)
  
  gwk= ggplot(mw2,aes(x= DateR, y = value, group = type, color = type)) + 
    geom_line(position=position_dodge(width=0.5)) + geom_point() +
    scale_x_date(name="Date",date_breaks = "2 weeks")  +
    ggtitle(flagCur) +
    theme(legend.position="bottom",legend.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1))
  
  # gwk
  
  ggsave(paste0(ofolder,flagCur,"_AvgAcc.png"),gwk,width = 12,height = 10)
  
})



########### cross count the overlay TRUE ######

df3 = df %>% mutate_if(is.factor,as.logical)

flagCur

crossCount = lapply(flags, function(flagCur){
  
  dfflag = df3 %>% select(Date:token,matches(flagCur))
  
  dfnames = dfflag %>% names()
  pdnames = dfnames[dfnames %>% str_detect("flag")]
  
  dfflag
  
  pdlen = length(pdnames)
  
  tbflag = matrix(nrow = pdlen,ncol = pdlen,dimnames = list(pdnames,pdnames))
  tbflag
  
  for (i in 1:pdlen) {
    for (j in 1:pdlen) {
      tbflag[i,j] = sum(dfflag[,pdnames[i]] & dfflag[,pdnames[j]])
    }
  }
  
  return(tbflag)
})


tbfile = paste0(ofolder,"crossTable.csv")
cat("",file=tbfile,append=F)

lapply(crossCount, function(mflag){
  
  write.table(mflag,file = tbfile,append = T,sep = ',')
  cat("\n\n",file=tbfile,append=TRUE)
})
