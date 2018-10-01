source("00_objects.R")

dataFolder = "01_data/"
outputFolder = "02_output/"

fileList <- list.files(dataFolder,pattern = "txt$")

for (f in fileList) {
  
  networkName = f %>% str_remove(.,"network") %>% str_remove(.,"TX.txt")
  
  #input and filter
  
  network <- EtherNetwork$new(modelName = networkName,fileLoc = paste0(dataFolder,f),timeFormat = "%Y%m%d")
  t <- network$networkDF
  # table(t$time)
  
  network$dropExtreme(percentage = 1/500) #drop the extreme value: remove top and bottom
  network$normalValue() #for value2 col: distance
  
  # bulid folders
  
  loc = paste0(outputFolder,networkName)
  dir.create(loc,showWarnings = FALSE)
  
  orgGraphFolder = paste0(loc,"/00_orgGraph/")
  orgDistFolder = paste0(loc,"/00_orgDist/")
  graphFolder = paste0(loc,"/01_graphs/")
  distFolder =paste0(loc,"/02_distMatrix/")
  GUDHIFolder = paste0(loc,"/03_GUDHI/")
  bettiFolder = paste0(loc,"/04_betti/")
  bettiPicFolder = paste0(loc,"/05_bettiPlots/")
  joinBettiFolder = paste0(loc,"/06_joinBetti/")
  
  dir.create(orgGraphFolder,showWarnings = FALSE)
  dir.create(orgDistFolder,showWarnings = FALSE)
  dir.create(graphFolder,showWarnings = FALSE)
  dir.create(distFolder,showWarnings = FALSE)
  dir.create(GUDHIFolder,showWarnings = FALSE)
  dir.create(bettiFolder,showWarnings = FALSE)
  dir.create(bettiPicFolder,showWarnings = FALSE)
  dir.create(joinBettiFolder,showWarnings = FALSE)
  
  # processing
  network$record(paste0(loc,"/dataSummary.txt"))
  
  network$procOrignal(orgGraphFolder,orgDistFolder)
  network$proc(graphFolder,distFolder,toprank = 150)
  network$GUDHIRun(distFolder,GUDHIFolder)
  network$convertBetti(GUDHIFolder,bettiFolder)
  network$plotBetti(bettiFolder,bettiPicFolder)
  network$joinBetti(bettiFolder,joinBettiFolder)
  
}



##### specific one  ###

dataFolder = "01_speical/"
outputFolder = "02_SpecialOutput/"
  
f = "networkdecentralandTX.txt"

networkName = f %>% str_remove(.,"network") %>% str_remove(.,"TX.txt")

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
orgDistFolder = paste0(loc,"/00_orgDist/")
graphFolder = paste0(loc,"/01_graphs/")
distFolder =paste0(loc,"/02_distMatrix/")
GUDHIFolder = paste0(loc,"/03_GUDHI/")
bettiFolder = paste0(loc,"/04_betti/")
bettiPicFolder = paste0(loc,"/05_bettiPlots/")
joinBettiFolder = paste0(loc,"/06_joinBetti/")

dir.create(orgGraphFolder,showWarnings = FALSE)
dir.create(orgDistFolder,showWarnings = FALSE)
dir.create(graphFolder,showWarnings = FALSE)
dir.create(distFolder,showWarnings = FALSE)
dir.create(GUDHIFolder,showWarnings = FALSE)
dir.create(bettiFolder,showWarnings = FALSE)
dir.create(bettiPicFolder,showWarnings = FALSE)
dir.create(joinBettiFolder,showWarnings = FALSE)

# processing
network$record(paste0(loc,"/dataSummary.txt"))

network$procOrignal(orgGraphFolder,orgDistFolder)
network$proc(graphFolder,distFolder,toprank = 150)
network$GUDHIRun(distFolder,GUDHIFolder)
network$convertBetti(GUDHIFolder,bettiFolder)
network$plotBetti(bettiFolder,bettiPicFolder)
network$joinBetti(bettiFolder,joinBettiFolder)


##### specific folder  ########

dataFolder = "01_data2/"
outputFolder = "02_output2/"


fileList <- list.files(dataFolder,pattern = "txt$")

for (f in fileList) {
  
  networkName = f %>% str_remove(.,"network") %>% str_remove(.,"TX.txt")
  
  #input and filter
  
  network <- EtherNetwork$new(modelName = networkName,fileLoc = paste0(dataFolder,f),timeFormat = "%Y%m%d")
  t <- network$networkDF
  # table(t$time)
  
  network$dropExtreme(percentage = 1/500) #drop the extreme value: remove top and bottom
  network$normalValue() #for value2 col: distance
  
  # bulid folders
  
  loc = paste0(outputFolder,networkName)
  dir.create(loc,showWarnings = FALSE)
  
  orgGraphFolder = paste0(loc,"/00_orgGraph/")
  orgDistFolder = paste0(loc,"/00_orgDist/")
  graphFolder = paste0(loc,"/01_graphs/")
  distFolder =paste0(loc,"/02_distMatrix/")
  GUDHIFolder = paste0(loc,"/03_GUDHI/")
  bettiFolder = paste0(loc,"/04_betti/")
  bettiPicFolder = paste0(loc,"/05_bettiPlots/")
  joinBettiFolder = paste0(loc,"/06_joinBetti/")
  
  dir.create(orgGraphFolder,showWarnings = FALSE)
  dir.create(orgDistFolder,showWarnings = FALSE)
  dir.create(graphFolder,showWarnings = FALSE)
  dir.create(distFolder,showWarnings = FALSE)
  dir.create(GUDHIFolder,showWarnings = FALSE)
  dir.create(bettiFolder,showWarnings = FALSE)
  dir.create(bettiPicFolder,showWarnings = FALSE)
  dir.create(joinBettiFolder,showWarnings = FALSE)
  
  # processing
  network$record(paste0(loc,"/dataSummary.txt"))
  
  network$procOrignal(orgGraphFolder,orgDistFolder)
  network$proc(graphFolder,distFolder,toprank = 150)
  network$GUDHIRun(distFolder,GUDHIFolder)
  network$convertBetti(GUDHIFolder,bettiFolder)
  network$plotBetti(bettiFolder,bettiPicFolder)
  network$joinBetti(bettiFolder,joinBettiFolder)
  
}
