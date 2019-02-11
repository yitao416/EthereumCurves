
library(tidyverse)

RFmodel = R6::R6Class("RFmodel",public = list(
  tkname = NULL,
  df = NULL,
  flags = NULL,
  fmls = NULL,
  dftrain = NULL,
  dftest = NULL,
  res = NULL,


  initialize = function(tkname,inputFile){
    self$tkname = tkname
    df = read_rds(inputFile)
    self$flags = df %>% select(contains("flag")) %>% colnames()
    # change all logical to factor for classification 
    df = df %>% mutate_if(is.logical,factor,levels=c("TRUE","FALSE"))
    df$clusterCoef = replace(df$clusterCoef,is.nan(df$clusterCoef),0)
    self$df = df
    dateRange = range(df$day)
  	spDate = dateRange[1] + diff(dateRange)/3*2
  	self$dftrain = df[df$day<spDate,]
	  self$dftest = df[!df$day<spDate,]

  },

  showVarNames = function(){
  	print(colnames(self$df))
  	invisible(self)
  },

  RFpred = function(inputfmls,repNum = 100){
    message(self$tkname)

  	# iterate from flag1 to flag7
  	res = map_dfr(self$flags,function(flagi){
  		set.seed(321)
  	  message(flagi)
  	  flag_resRuns = map_dfr(1:repNum,function(repID){
  	    # iterate all formula
  	    flag_res = map2_dfr(inputfmls,names(inputfmls),function(fmlstr,fmlname){
  	      
  	      rf = randomForest::randomForest(as.formula(paste0(flagi,fmlstr)),
  	                                      self$dftrain,importance = TRUE, na.action = na.omit)
  	      cf = caret::confusionMatrix(
  	        predict(rf,self$dftest),
  	        self$dftest %>% pull(flagi),
  	        positive = "TRUE"
  	      )
  	      cfmatrix = rbind(as.matrix(cf,what="overall"),
  	                       as.matrix(cf,what="classes"))
  	      cftb = cfmatrix %>% as.data.frame()  %>% rownames_to_column() %>% as_tibble()
  	      names(cftb) = c("measure","value")
  	      cftb$horizon = flagi
  	      cftb$modelType = fmlname
  	      return(cftb)
  	    })
  	    flag_res$id = repID
  	    return(flag_res)
  	    
  	  })
  		
  	})

  	res$tkname = self$tkname
  	self$res = res

	  invisible(self)
  },
  
  # for detail study, output the prediction
  RF_singleRun = function(inputfmls,para_flag,outputFolder){
    dir.create(outputFolder,showWarnings = FALSE)
    message(self$tkname)
    
    # same seed as RFpred
    set.seed(321)
    message(para_flag)
    
    flag_res = map2_dfc(inputfmls,names(inputfmls),function(fmlstr,fmlname){
      
      rf = randomForest::randomForest(as.formula(paste0(para_flag,fmlstr)),
                                      self$dftrain,importance = TRUE, na.action = na.omit)
      pred_res = tibble(
        pred = predict(rf,self$dftest)
      )
      
      names(pred_res) = fmlname
      
      return(pred_res)
    })
    
    
    write_rds(bind_cols(self$dftest,flag_res),
              paste0(outputFolder,"predDF_",self$tkname,"_",para_flag,".rds"))
    
    invisible(self)
  },

  outputRDS = function(outputFolder,prefix = "rf_"){
    dir.create(outputFolder,showWarnings = FALSE)
  	saveRDS(self$res,paste0(outputFolder,prefix,self$tkname,".rds"))
  	invisible(self)
  }
  
))