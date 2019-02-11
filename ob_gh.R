
library(igraph)
library(tidyverse)

gh = R6::R6Class("gh",public = list(
  tkname = NULL,
  df = NULL,
  
  initialize = function(inputfile){
    self$tkname = str_extract(inputfile,"(?<=network).*(?=TX)")
    df = read_delim(inputfile," ",col_names = FALSE,col_types =cols(.default = col_double())) %>% 
      setNames(.,c("from","to","time","value"))
    df$time = df$time %>% anytime::anydate()
    self$df = df %>% arrange(time)
  },
  
  # calculate graph features per day
  featurePerDay = function(inputday= "2018-04-24"){
    
    tmp = self$df %>% filter(time==inputday)
    
    # create graph
    rawGraph <- graph.data.frame(tmp[,c("from","to")],directed = F)
    E(rawGraph)$weight = tmp$value 
    
    # the return result
    tibble(
      tk = self$tkname,
      day = inputday,
      vertexNum = vcount(rawGraph),
      edgeNum = ecount(rawGraph),
      clusterCoef = transitivity(rawGraph)
    )
  },
  
  # apply featurePerDay to the whole data
  featureTb = function(){
    dayVector = self$df$time %>% unique()
    
    res = map_dfr(
      dayVector,
      self$featurePerDay
    )
  }
))