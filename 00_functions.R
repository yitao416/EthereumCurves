rm(list=ls())
library(igraph)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(data.table)
library(Matrix)
library(plyr)
library(gdata)
library(TDA)
library(plyr)
library(stringr)
library(dplyr)
library(anytime)
library(tidyr)
library(tidyverse)
library(R6)

coreDist <- function(edgeList,edgeWeight,plotBefore=F,plotAfter=F,extraTitle=""){
  
  g2 <- graph.data.frame(edgeList,directed = F)
  
  E(g2)$weight <- edgeWeight
  g2 <- igraph::simplify(g2,remove.multiple = T,edge.attr.comb = list(weight="mean","ignore"))
  
  if (plotBefore == T) {
    plot(g2,vertex.size=1,vertex.label=NA,main = paste(extraTitle,"Original vertices #:",vcount(g2)))
  }
  print(paste("Before vertices Number: ", vcount(g2)))
  # delete degree 1
  g3 <- g2
  g3.deg <- degree(g3) == 1
  while(any(g3.deg == T)){
    g3 <- delete.vertices(g3,V(g3)[g3.deg])
    g3.deg <- degree(g3) == 1
  }
  #delete degree 0
  g3 <- delete.vertices(g3,V(g3)[degree(g3) == 0])
  
  if (plotAfter == T) {
    if (ecount(g3) >0) {
      plot(g3,vertex.size=1,vertex.label=NA, main = paste(extraTitle,"Trimed vertices #:",vcount(g3)))
    }
  }
  
  print(paste("After vertices Number: ", vcount(g3)))
  print("Degree distribution")
  print(table(degree(g3)))

  DD = distances(g3, v = V(g3), to = V(g3), mode = c("all"), weights = NULL, algorithm = c("automatic"))
  Ind <- DD ==Inf
  DD[Ind] <- vcount(g3)
  DD <- round(DD,digits = 5)
}


graphInfo <- function(edgeList,edgeWeight,plotBefore=F,plotAfter=F,extraTitle=""){
  
  g2 <- graph.data.frame(edgeList,directed = F)
  
  E(g2)$weight <- edgeWeight
  g2 <- igraph::simplify(g2,remove.multiple = T,edge.attr.comb = list(weight="mean","ignore"))

  if (plotBefore == T) {
    plot(g2,vertex.size=1,vertex.label=NA,main = paste(extraTitle,"Original vertices #:",vcount(g2)))
  }
  print(paste("Before vertices Number: ", vcount(g2)))
  # delete degree 1
  g3 <- g2
  g3.deg <- degree(g3) == 1
  while(any(g3.deg == T)){
    g3 <- delete.vertices(g3,V(g3)[g3.deg])
    g3.deg <- degree(g3) == 1
  }
  #delete degree 0
  g3 <- delete.vertices(g3,V(g3)[degree(g3) == 0])
  
  if (plotAfter == T) {
    if (ecount(g3) >0) {
      plot(g3,vertex.size=1,vertex.label=NA, main = paste(extraTitle,"Trimed vertices #:",vcount(g3)))
    }
  }
  
  print(paste("After vertices Number: ", vcount(g3)))
  print("Degree distribution")
  print(table(degree(g3)))
  
  # DD = distances(g3, v = V(g3), to = V(g3), mode = c("all"), weights = NULL, algorithm = c("automatic"))
  # Ind <- DD ==Inf
  # DD[Ind] <- vcount(g3)
  # DD <- round(DD,digits = 5)
  info <- tibble(
    orgEdges = ecount(g2),
    orgNodes = vcount(g2),
    orgAvgDegree = sum(degree(g2))/vcount(g2),
    trimedEdges = ecount(g3),
    trimedNodes = vcount(g3),
    trimedAvgDegree = sum(degree(g3))/vcount(g3)
  )
  return(info)
}




selectTop <- function(dat,topsize=100){
  frq.to <- sort(table(dat$to),decreasing = T)[1:topsize]
  frq.from <- sort(table(dat$from),decreasing = T)[1:topsize]
  toplist <- union(names(frq.to),names(frq.from)) %>% unique()
  dat <- subset(dat,(to %in% toplist) & (from %in% toplist))
}

# normalize value
distnorm <- function(x){
  d = 1+(x-A)*(b-a)/(B-A)
  return(1/d)
}

# distnorm <- function(x){
#   d = 1+(log10(x)-log10(A))*(b-a)/(log10(B)-log10(A))
#   return(1/d)
# }

bettinorm <- function(bettiResult){
  for (i in names(bettiResult)) {
    if (i == "Time") {
      next
    }
    if (max(bettiResult[[i]]) == 0) {
      next
    }
    bettiResult[[i]] = bettiResult[[i]] / max(bettiResult[[i]])
  }
  return(bettiResult)
}


recordDistrib <- function(before,after,dataSummary,filename){
  sink(filename)
  print("Original:")
  print(before)
  print(sprintf("Valid range: [%.2e, %.2e]",A,B))
  print("Drop extreme value:")
  print(after)
  print("Value summary:")
  print(dataSummary)
  sink()
  return()
}
