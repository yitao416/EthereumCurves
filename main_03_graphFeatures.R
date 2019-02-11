
rm(list=ls())

source("ob_gh.R")

rdsFolder = "00_Rdata/"
ifolder = "01_data/"
files = list.files(ifolder)
# f = paste0(ifolder,files)[1]

totalRes = map_dfr( paste0(ifolder,files), function(f){
    tkgh = gh$new(f)
    message(tkgh$tkname)
    res = tkgh$featureTb()
    write_rds(res,paste0(rdsFolder,"graphFeature_",tkgh$tkname,".rds"))
    return(res)
})

write_rds(totalRes,paste0(rdsFolder,"gF.rds"))


