
source("ob_EtherNetwork.R")

dataFolder = "01_data/"
outputFolder = "02_output/"
dir.create(outputFolder,showWarnings = FALSE)

orgGraphFolder = "00_orgGraph/"
orgDistFolder = "00_orgDist/"
graphFolder = "01_graphs/"
distFolder ="02_distMatrix/"
GUDHIFolder = "03_GUDHI/"
bettiFolder = "04_betti/"
bettiEqFolder = "04_bettiEq/"
bettiPicFolder = "05_bettiPlots/"
joinBettiFolder = "06_joinBetti/"

fileList <- list.files(dataFolder,pattern = "txt$")

for (f in fileList[7:33]) {

  networkName = f %>% str_remove(.,"network") %>% str_remove(.,"TX.txt")
  
  #input and filter
  network <- EtherNetwork$new(modelName = networkName,fileLoc = paste0(dataFolder,f),
                              timeFormat = "%Y%m%d",outFolder = outputFolder)
  # t <- network$networkDF
  # table(t$time)
  
  network$dropExtreme() #drop the extreme value: remove top and bottom
  network$normalValue() #for value2 col: distance
  
  network$record()
  
  # network$procOrignal(orgGraphFolder,orgDistFolder)
  network$proc(graphFolder,distFolder,toprank = 150)
  network$GUDHIRun(distFolder,GUDHIFolder)
  network$convertBetti(GUDHIFolder,bettiFolder)
  # network$plotBetti(bettiFolder,bettiPicFolder)
  
  network$reformatBetti(bettiFolder,bettiEqFolder)
  network$joinBetti(bettiEqFolder,joinBettiFolder)
}
