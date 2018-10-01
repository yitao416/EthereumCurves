
source("00_objects2.R")


ifolder = "11_topNodes/"
ofolder = "11_topNodes/"
fList = list.files(ifolder,"network")

tokenNames = fList %>% str_remove("network") %>% str_remove("TX.txt")

res = map2(fList,tokenNames,function(f,tokenName,dataFolder = ifolder){
  
  network <- EtherNetwork$new(modelName = tokenName,fileLoc = paste0(dataFolder,f),timeFormat = "%Y%m%d")
  network$dropExtreme(percentage = 1/500) #drop the extreme value: remove top and bottom
  network$normalValue() #for value2 col: distance
  
  df = network$networkDF
  
  daysList = df$time %>% unique() %>% as.character()
  
  dailyDF = map(daysList,function(cur,.df=df){
    dfcur = df[df$time == cur,]
    totalNodes = union(unique(dfcur$to),unique(dfcur$from)) %>% length()
    top100 = getTopList(dfcur,100)
    top150 = getTopList(dfcur,150) 
    pct50 = getTopList(dfcur,0.5*totalNodes)
    pct10 = getTopList(dfcur,0.1*totalNodes)
    pct20 = getTopList(dfcur,0.2*totalNodes)
    
    dfcur2 = dfcur %>% mutate(top100 = checkInList(to,from,top100),
                              top150 = checkInList(to,from,top150),
                              pct10 = checkInList(to,from,pct10),
                              pct20 = checkInList(to,from,pct20),
                              pct50 = checkInList(to,from,pct50)
    )
    
    dfcur3 = dfcur2 %>% summarise_at(vars(top100:pct50),sum)
    dfcurRes = dfcur3 %>% add_column(time=as.Date(cur,"%Y%m%d"),totalEdges=nrow(dfcur2),totalNodes = totalNodes,.before= "top100")
    return(dfcurRes)
  })
  
  dailyDF = dailyDF %>% bind_rows()
  
  dailyDF2 = dailyDF %>% mutate_at(vars(top100:pct50),funs(perc = ./totalEdges))
  dailyDF3 = dailyDF2 %>% add_column(token = tokenName,.before = "totalEdges") %>% arrange(time)
  return(dailyDF3)
})



res2 = res %>% bind_rows()
res2

write_rds(res2,paste0(ofolder,"edgeCoverage.rds"))


