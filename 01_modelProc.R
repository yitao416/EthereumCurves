source("00_objects.R")


dataFolder = "01_data/"
outputFolder = "02_output/"

fileList <- list.files(dataFolder,pattern = "txt$")

f = fileList[1]
f = "networkrequestTX.txt"

networkName = f %>% str_remove(.,"network") %>% str_remove(.,"TX.txt")
# 
# t = 1500001911
# t %>% anydate() %>% format(.,"%Y%m%d")

#input and filter

network <- EtherNetwork$new(modelName = networkName,fileLoc = paste0(dataFolder,f),timeFormat = "%Y%m%d")
t <- network$networkDF
table(t$time)


network$dropExtreme(percentage = 1/500) #drop the extreme value: remove top and bottom
network$normalValue() #for value2 col: distance

# bulid folders

loc = paste0(outputFolder,networkName)
dir.create(loc,showWarnings = FALSE)

orgGraphFolder = paste0(loc,"/00_orgGraph/")

graphFolder = paste0(loc,"/01_graphs/")
distFolder =paste0(loc,"/02_distMatrix/")
GUDHIFolder = paste0(loc,"/03_GUDHI/")
bettiFolder = paste0(loc,"/04_betti/")
bettiPicFolder = paste0(loc,"/05_bettiPlots/")
joinBettiFolder = paste0(loc,"/06_joinBetti/")

dir.create(orgGraphFolder,showWarnings = FALSE)
dir.create(graphFolder,showWarnings = FALSE)
dir.create(distFolder,showWarnings = FALSE)
dir.create(GUDHIFolder,showWarnings = FALSE)
dir.create(bettiFolder,showWarnings = FALSE)
dir.create(bettiPicFolder,showWarnings = FALSE)
dir.create(joinBettiFolder,showWarnings = FALSE)

# processing
network$record(paste0(loc,"/dataSummary.txt"))


network$procOrignal(orgGraphFolder,distFolder = distFolder)

network$proc(graphFolder,distFolder,toprank = 150)
network$GUDHIRun(distFolder,GUDHIFolder)
network$convertBetti(GUDHIFolder,bettiFolder)
network$plotBetti(bettiFolder,bettiPicFolder)
network$joinBetti(bettiFolder,joinBettiFolder)



t <- tibble(
  a=1,
  b=2
)
t
write_csv(t,'text.txt')

for (f in fileList) {
  
  networkName = gsub(".txt","",f)
  
  #input and filter
  
  network <- EtherNetwork$new(modelName = networkName,fileLoc = paste0(dataFolder,f),timeFormat = "%Y%W")
  network$networkDF
  network$dropExtreme(percentage = 1/500) #drop the extreme value: remove top and bottom
  network$normalValue() #for value2 col: distance
  
  # bulid folders
  
  loc = paste0(outputFolder,networkName)
  dir.create(loc,showWarnings = FALSE)
  
  graphFolder = paste0(loc,"/01_graphs/")
  distFolder =paste0(loc,"/02_distMatrix/")
  GUDHIFolder = paste0(loc,"/03_GUDHI/")
  bettiFolder = paste0(loc,"/04_betti/")
  bettiPicFolder = paste0(loc,"/05_bettiPlots/")
  joinBettiFolder = paste0(loc,"/06_joinBetti/")
  dir.create(graphFolder,showWarnings = FALSE)
  dir.create(distFolder,showWarnings = FALSE)
  dir.create(GUDHIFolder,showWarnings = FALSE)
  dir.create(bettiFolder,showWarnings = FALSE)
  dir.create(bettiPicFolder,showWarnings = FALSE)
  dir.create(joinBettiFolder,showWarnings = FALSE)
  
  # processing
  network$record(paste0(loc,"/dataSummary.txt"))
  network$proc(graphFolder,distFolder)
  network$GUDHIRun(distFolder,GUDHIFolder)
  network$convertBetti(GUDHIFolder,bettiFolder)
  network$plotBetti(bettiFolder,bettiPicFolder)
  network$joinBetti(bettiFolder,joinBettiFolder)
}


