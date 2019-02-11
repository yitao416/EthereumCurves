
rm(list = ls())
source("ob_tokents.R")

ifolder = "00_Rdata/"
ofolder = "00_Plots/"
prefix = "df_"
keyword = "abs25"

tknames = list.files(ifolder,pattern = paste0(prefix,".*",keyword))
tknames = tknames %>% str_extract(paste0("(?<=",prefix,")",".*(?=_",keyword,")"))
tknames = tknames %>% setdiff("polyai")

walk(tknames,function(tokenName){
  
  tryCatch(
    {
      tk = tokents$new(ifolder,prefix,tokenName,keyword)
      
      ### Time Series model 
      tk$laggedARIMA(ofolder = ofolder,prefix = "lagged_",lag_max = 7,dataFolder = ifolder)
      tk$laggedARIMA_performance(outputFolder = ifolder,prefix = "lagPref_")
      tk$lagged_measurePlots(m="Accuracy",outputFolder = ofolder,prefix = "lagM_")
      ### EG test
      # tk$EG_test(outputFolder = ifolder, prefix = "coin_")
      # tk$EG_plots(outputFolder = ofolder,prefix = "coin_")
    },
    error = function(e) message(tokenName),
    finally = message("Done.")
  )
  
})



#### for single token testing ###
# 
# setup()
# 

# ofolder = "06_Summary/"
source("ob_tokents.R")
tokenName = "bat" # bat or storj
tk = tokents$new(ifolder,prefix,tokenName,keyword)
# 
tk$laggedARIMA(ofolder = "07_paperPlots/",prefix = "lagged_",lag_max = 4,plotting = TRUE,dataFolder = ifolder)

# 
# tmp = tk$tmp
# train = tmp[[4]]
# 
# tmp[[3]]
# 
# tmppd = tmp[[1]]
# tmppd$upper
# 
# tmpdf = tmppd$mean
# tmpdf %>% as.vector()
# 
# tmp[[3]]
# 
# tmppd$upper[,2]
# 
# ggdf2 = tibble(day=tmp[[3]]$day,
#               openNorm = tmpdf %>% as.vector(),
#               CI95upper = tmppd$upper[,2] %>% as.vector(),
#               CI95lower = tmppd$lower[,2] %>% as.vector())
# 
# ggdf = bind_rows(train %>% select(day,openNorm),ggdf2[,1:2])
# 
# ggplot(tk$df,aes(x=day,y=openNorm)) + geom_line() + 
#   geom_line(data = ggdf2,aes(x=day,y=openNorm),colour="blue") + 
#   geom_ribbon(data=ggdf2,aes(ymin=CI95lower,ymax=CI95upper),fill="steelblue1",alpha=0.4) +
#   scale_x_date(date_breaks = "1 month",date_labels="%m/%d/%y") + 
#   theme_minimal()
# 
# ggplot() + 
#   geom_ribbon(data=ggdf2,aes(x=day,ymin=CI95lower,ymax=CI95upper,fill="CI_95"),
#               alpha=0.4) +
#   geom_line(data = ggdf2,aes(x=day,y=openNorm,color='predicted')) +
#   geom_line(data = tk$df,aes(x=day,y=openNorm)) +
#   scale_fill_manual("",values="steelblue1") +
#   scale_color_manual("",values = c("predicted"="blue")) +
#   scale_x_date(date_breaks = "2 months",date_labels="%m/%d/%y") + 
#   theme_minimal() +
#   labs(title = "Price only",y="price",x='Date') +
#   theme(legend.position = c(0.2,0.9),
#         legend.box = "horizontal",
#         legend.box.background = element_rect(fill="white",colour="white"))

# tk$laggedARIMA_performance(outputFolder = ifolder,prefex = "lagPref_")
# tk$lagged_measurePlots(m="Accuracy",outputFolder = ofolder,prefix = "lagM_")

# tk$EG_test(outputFolder = ifolder)
# tk$EG_plots(outputFolder = ofolder,prefix = "coin_")
