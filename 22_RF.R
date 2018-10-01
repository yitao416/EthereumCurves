rm(list=ls())
library(randomForest)


Ofolder = "01_RF/"
dfRaw = readRDS("pred_data02.rds")
head(dfRaw)
levels(dfRaw$flag6)
tokens = unique(dfRaw$token)



flags = paste0('flag',1:7)

fmls = c("~ edge + openNorm",
         "~ edge + openNorm + B0_rd7",
         "~ edge + openNorm + B0_rd7 + B1_rd7",
         "~ edge + openNorm + B0_rd7 + B1_rd7 + B2_rd7")

######### data section ########

tkname = "omisego"

df = dfRaw[dfRaw$token==tkname,]

dateRange = range(df$Date)
spDate = dateRange[1] + (dateRange[2]-dateRange[1])/3*2

dfTrainTK = df[df$Date<spDate,]
dfTestTK = df[!df$Date<spDate,]

###########  functions #########
#### ntree 501, mtry as default
#### return a named col that can be added to dftest later


rfFun6 <- function(fmlstr,dfTrain,dfTest,tk,para_ntrees=501){
  set.seed(321)
  dfRF = randomForest(as.formula(fmlstr),dfTrain, ntree=para_ntrees,importance = TRUE, na.action = na.omit)
  
  # plot(dfRF)
  plotname = gsub("\\~|\\+|\\s|_rd7","",fmlstr)
  plotname = gsub("edgeopenNorm",'eo',plotname)
  
  png(paste0(Ofolder,tk,"_ntr",para_ntrees,'_var_',plotname,".png"))
  varImpPlot(dfRF,sort = TRUE,main = paste(tk,"nt:",para_ntrees," ",fmlstr))
  dev.off()
  
  png(paste0(Ofolder,tk,"_ntr",para_ntrees,'_rf_',plotname,".png"))
  plot(dfRF,main = paste(tk,"nt:",para_ntrees," ",fmlstr))
  dev.off()
  
  pd = predict(dfRF,dfTest)
  pd = as.data.frame(pd)
  names(pd) = plotname
  
  # dfTest = cbind(dfTest,pd)

  ### in this case here, the dftest will be saved as rds file rather than return 
  # saveRDS(dfTest,paste0(Ofolder,tk,"_ntr",para_ntrees,"_",plotname,'.rds'))
  return(pd)
}


tksFun <- function(tkname){
  
  df = dfRaw[dfRaw$token==tkname,]
  dateRange = range(df$Date)
  spDate = dateRange[1] + (dateRange[2]-dateRange[1])/3*2
  
  dfTrainTK = df[df$Date<spDate,]
  dfTestTK = df[!df$Date<spDate,]

  
  for (y in flags) {
    predList = lapply(fmls, function(fml){
      rfFun6(paste(y,fml),dfTrain = dfTrainTK,dfTest = dfTestTK,tk=tkname)
    })
    
    dfTestTK = cbind(dfTestTK,Reduce(cbind,predList))
  }
  
  saveRDS(dfTestTK,paste0(Ofolder,tkname,"_test.rds"))
}

tmp = lapply(tokens, tksFun)
