rm(list=ls())
library(tidyverse)
library(plotly)


ifolder = "11_topNodes/"
ofolder = "11_topNodes/"
f = list.files(ifolder,"rds")


dailyDF = read_rds(paste0(ifolder,f))

tokenList = dailyDF %>% pull(token) %>% unique()


tk = tokenList[1]


res = map(tokenList,function(tk){
  dailyDF2 = dailyDF[dailyDF$token == tk,]
  
  dailyDF3 = dailyDF2 %>% select(time,top100_perc:pct50_perc)
  plotDF = dailyDF3 %>% gather(key = "type",value = percentage,top100_perc:pct50_perc)
  
  p1= plot_ly(plotDF,x=~time,y=~percentage,color = ~type, type = 'scatter', mode = 'markers+lines') %>% 
    layout(title = paste("Edge coverage (%) per diff selections --",tk))
  
  p2= plot_ly(dailyDF2 %>% arrange(time),x=~time,y=~totalEdges,name = "# edge",type = 'scatter', mode = 'markers+lines') %>% 
    add_trace(y=~totalNodes,name="# nodes") %>% 
    add_lines(y=1000,name="Ref. Line",line=list(dash="dash"))
  
  
  p <- subplot(p1,p2,nrows = 2, shareX = T,titleY=T,heights = c(0.6,0.4))
  
  plotFile = paste0("EdgeCoverage_",tk,".html")
  htmlwidgets::saveWidget(p,plotFile)
  file.rename(plotFile,paste0(ofolder,plotFile))
  
})


