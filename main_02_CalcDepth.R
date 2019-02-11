
rm(list=ls())
source("ob_depth.R")

dp = depthAnalysis$new()
dp$collectAllBettiFiles("02_output/","03_jointedBetti/")
dp$rollDepth()
