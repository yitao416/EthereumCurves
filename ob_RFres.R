# object for prediction result

library(tidyverse)

RFres = R6::R6Class("RFres",public = list(
  
  selectedMeasure = c("Accuracy","Neg Pred Value","Precision","Sensitivity","Specificity"),
  ifolder = NULL,
  files = NULL,
  tks = NULL,
  tmp = NULL,
  dfres = NULL,
  dfplot = NULL,
  threshod = NULL,

  initialize = function(inputFolder,keyword){
  	self$ifolder = inputFolder
  	self$files = list.files(inputFolder,keyword)
  	self$tks = self$files %>% str_extract("(?<=_).*(?=\\.)")
    self$threshod = self$tks[1] %>% str_extract("(?<=_).*")
  	
  },
  showAllMeasure = function(){
  	df = read_rds(paste0(self$ifolder,self$files[1]))
  	measure = df %>% pull(measure) %>% unique()
  	print(measure)
  	invisible(self)

  },
  setMeasure = function(measures){
  	self$selectedMeasure = measures
  },

  avgPerToken = function(tkfile = self$files[7],tkname = tks[7]){
  	df = read_rds(paste0(self$ifolder,tkfile))

  	dfavg = df %>% group_by(horizon,modelType,measure) %>% 
  			summarise(avg = mean(value, na.rm = TRUE)) %>% ungroup()
  	dfavg = dfavg %>% filter(measure %in% self$selectedMeasure)
  	dfavg$tkname = tkname
  	return(dfavg)
  },

  avgAllToken = function(){
  	self$dfres = map2_dfr(self$files,self$tks,self$avgPerToken)
    tmp = self$dfres %>% group_by(measure,horizon,modelType) %>% summarise(value = mean(avg,na.rm = TRUE)) %>% ungroup()
  	self$dfplot = tmp %>% mutate(horizon = str_remove(horizon,"flag") %>% as.factor(),modelType = as.factor(modelType))
    invisible(self)
  },

  plotPerMeasure = function(inputMeasure = "Accuracy",plotting = FALSE){
  	dfplot = self$dfplot %>% filter(measure == inputMeasure)
    yrange = range(dfplot$value)
    g = ggplot(data = dfplot,aes(x=horizon,y=value,fill = modelType)) + geom_bar(colour="black", position="dodge",stat = "identity") + 
          coord_cartesian(ylim=c(floor(yrange[1]*10)/10,ceiling(yrange[2]*10)/10)) +
          scale_fill_brewer() + xlab("Prediction horizon (unit: day)") + ylab(inputMeasure)+
          theme_minimal() + theme(text = element_text(size=22),legend.position="bottom")
    # theme(legend.position="bottom",legend.text = element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
        
    if(plotting){
      print(g)
    }
    return(g)
  },

  outputPlots = function(outputFolder,prefix = "6",plotting = FALSE){
    dir.create(outputFolder,showWarnings = FALSE)

    walk(self$selectedMeasure,function(j){
      g = self$plotPerMeasure(j,plotting)
      gfile = paste0(outputFolder,prefix,str_remove(j," "),"_",self$threshod,".png")
      ggsave(gfile,g,width=8,height = 6)
    })
    invisible(self)
  }
))











