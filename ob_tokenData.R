

library(tidyverse)

# an object that will merge roll depth, token price and graph features
# the network data as base

tokenData = R6::R6Class("tokenData",public = list(
	tkname = NULL,
	rollDepth = NULL,
	ghFeature = NULL,
	tkprice = NULL,
	price = NULL,
	threshod = NULL,
	df = NULL,

	initialize = function(inputName,returnThreshod = 0.25){
		self$tkname = inputName
		self$threshod = returnThreshod

	},

	addGraphFeature = function(inputFolder,keyword="graphFeature"){
		inputFile = list.files(inputFolder,paste0("(",keyword,").*",self$tkname))
		df = read_rds(paste0(inputFolder,inputFile))

		self$ghFeature = df

		invisible(self)

	},
	addRollDepth = function(inputFolder,keyword="rd"){
		# have 3 files B0 B1 B2
		inputFiles = list.files(inputFolder,paste0("(",keyword,").*",self$tkname))
		df = map_dfr(paste0(inputFolder,inputFiles),function(inputFile){
			read_rds(inputFile)
		})

		df = df %>% mutate(day = parse_date(day,format = "%Y%m%d"),
					      type = str_sub(type,1,2)) %>% 
				    spread(type,rollDepth)
		df$tk = self$tkname
		self$rollDepth = df %>% arrange(day)

		invisible(self)
	},
	# first check the existence of price file
	havePriceFile = function(inputFolder){
		files = list.files(inputFolder)
		inputFile = files[str_detect(files,regex(self$tkname,ignore_case=TRUE))]
		res = ifelse(length(inputFile)!=0,TRUE,FALSE)
		return(res)
	},
	addPrice = function(inputFolder){
		files = list.files(inputFolder)
		inputFile = files[str_detect(files,regex(self$tkname,ignore_case=TRUE))]
		message(inputFile)
		df = read_tsv(paste0(inputFolder,inputFile),
				col_types = cols(Date = col_date(format = "%m/%d/%Y"))) %>% arrange(Date)
  
		self$tkprice = df
		invisible(self)
	},
	procPrice = function(){
		df = self$tkprice %>% select(Date,Open)
		price = df$Open
		priceDiff = diff(price)
		priceReturn = priceDiff / price[1:length(priceDiff)]
		df = df %>% mutate(openNorm = Open/max(Open),priceReturn = c(NA,priceReturn))

		self$price = df
		invisible(self)
	},

	# flag under one horizon
	flagDaysFun = function(inputVector,ndays=3){
		# flag 1:7 days
		absReturn = abs(inputVector)
		l = length(inputVector)
		flagDays = vector(length = l)
		for (i in 1:(l-1)) {
			maxChange = max(absReturn[(i+1):(i+ndays)], na.rm=TRUE)
			# assign as factor
			flagDays[i] = ifelse(maxChange>=self$threshod,
			                     TRUE,
			                     FALSE)
		}
		return(flagDays)
	},

	# flag under all horizon
	flagPeriod = function(flagRange = 1:7){
		df = self$price
		for (x in flagRange) {
			df = df%>% mutate(!!(paste0("flag",x)):=self$flagDaysFun(priceReturn,x))
		}
		self$price = df
		invisible(self)
	},

	# merge 3 data sets, inner_join
	mergeData = function(){
		df = inner_join(self$ghFeature,self$rollDepth,by=(c("tk","day")))
		df = inner_join(df,self$price,by=c("day"="Date"))
		self$df = df
		invisible(self)
	},

	# output the final data set
	outputRDS = function(outputFolder){
		fileName = paste0(outputFolder,"df_",self$tkname,"_abs",100*self$threshod,".rds")
		saveRDS(self$df,fileName)
		invisible(self)
	}

  
))