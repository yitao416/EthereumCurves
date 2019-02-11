
# This file is for:
#

rm(list=ls())
source("ob_cointeg.R")

dataFolder = "00_Rdata/"
prefix = "df_"
keyword = "abs25"

tokenFiles = list.files(dataFolder,pattern = paste0(prefix,".*",keyword))

badtk = tokenFiles %>% str_detect("yocoin|reputation")

tokenFiles = tokenFiles[!badtk]
tokenLoc = paste0(dataFolder,tokenFiles)

# compVar = "openNorm"
compVar = "B2"
message(compVar)

l = length(tokenLoc)

self = cointeg$new(tokenLoc[1],tokenLoc[2])

self$partialCoinTest(tk1_var = compVar,tk2_var = compVar)
tmp = self$partialTest
tmp


res = map_dfr(1:(l-1),function(i){
  res_cur = map_dfr(seq(from=(i+1),to=l),function(j){
    
    self = cointeg$new(tokenLoc[i],tokenLoc[j])
    tryCatch(
      self$partialCoinTest(tk1_var = compVar,tk2_var = compVar),
      error = function(e) {
        message(tokenLoc[i])
        message(tokenLoc[j])
      }
    )
    
    self$partialTest
  })
})

# input a tibble(single col) return a pair of cum plus and minus

write_rds(res,paste0("00_Rdata/partCoin_",compVar,".rds"))

# 
# testPos = res %>% filter(coinTest==TRUE)
# write_excel_csv(testPos,"testPos.csv")
# testPos %>% group_by(x_tk,y_tk) %>% summarise(n=n()) %>% arrange(-n)

