
rm(list=ls())

source("ob_tokenData.R")

ifolder = "00_Rdata/"

tks = list.files(ifolder,"graphFeature") %>% str_remove("graphFeature_") %>% str_remove(".rds")

# rep is reputation
tks = setdiff(tks,"rep")

walk(tks,function(tkname){
  
  tk = tokenData$new(tkname)
  
  if (tk$havePriceFile("01_tokenPrice/")) {
    tk$addGraphFeature(ifolder)
    tk$addRollDepth(ifolder)
    tk$addPrice("01_tokenPrice/")
    tk$procPrice()
    tk$flagPeriod()
    tk$mergeData()
    tk$outputRDS(ifolder)
  }else{
    message(tk$tkname," has no price file.")
  }
  
})

# threshod 0.2

walk(tks,function(tkname){
  
  tk = tokenData$new(tkname,returnThreshod = 0.2)
  
  if (tk$havePriceFile("01_tokenPrice/")) {
    tk$addGraphFeature(ifolder)
    tk$addRollDepth(ifolder)
    tk$addPrice("01_tokenPrice/")
    tk$procPrice()
    tk$flagPeriod()
    tk$mergeData()
    tk$outputRDS(ifolder)
  }else{
    message(tk$tkname," has no price file.")
  }
  
})

