# This file is for:
# visulize result

rm(list=ls())
ifolder = "00_Rdata/"
ifolder = "01_Rdata/" # for paper
source("ob_RFres.R")

files = list.files(ifolder,"rf.*25")
tks = files %>% str_extract("(?<=_).*(?=\\.)")


res = RFres$new(ifolder,"rf_.*25")
res$files
res$ifolder
res$tks

# res$showAllMeasure()
res$setMeasure(c("Accuracy","Sensitivity","Specificity"))
res$avgAllToken()
# res$outputPlots("06_Summary/")
res$outputPlots("01_Rdata/")

# available measures
# [1] "Accuracy"             "Kappa"                "AccuracyLower"        "AccuracyUpper"        "AccuracyNull"        
# [6] "AccuracyPValue"       "McnemarPValue"        "Sensitivity"          "Specificity"          "Pos Pred Value"      
# [11] "Neg Pred Value"       "Precision"            "Recall"               "F1"                   "Prevalence"          
# [16] "Detection Rate"       "Detection Prevalence" "Balanced Accuracy"   