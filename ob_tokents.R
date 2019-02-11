library(tidyverse)
library(forecast)
library(grid)

# variable lag function
lag_mutate <- function(df, mvstr,lag_max=3) {
  expr <- quo(!!rlang::sym(mvstr))
  mean_name <- paste0("mean_", quo_name(expr))
  sum_name <- paste0("sum_", quo_name(expr))
  tmp = df
  for (i in seq(lag_max)) {
    newColName = paste0(mvstr,"_lag",i)
    tmp = mutate(tmp,
                 !! newColName:= lag(!!expr,i))
  }
  return(tmp)
}


# flag days per price
flagDaysFun = function(inputVector,ndays=3, threshold=0.25){
  
  price = inputVector
  priceDiff = diff(price)
  priceReturn = priceDiff / price[1:length(priceDiff)]
  priceReturn = c(NA,priceReturn)
  
  absReturn = abs(priceReturn)
  l = length(priceReturn)
  flagDays = vector(length = l)
  for (i in 1:(l-1)) {
    maxChange = max(absReturn[(i+1):(i+ndays)], na.rm=TRUE)
    # assign as factor
    flagDays[i] = ifelse(maxChange>=threshold,
                         TRUE,
                         FALSE)
  }
  return(flagDays)
}


# object for 


tokents = R6::R6Class("tokents",public = list(
  df = NULL,
  tokenName = NULL,
  price = NULL,
  splitRatio = 2/3,
  test = NULL,
  pred = NULL,
  measure= NULL,
  egdf = NULL,
  tmp = NULL, # for debugging
  
  initialize = function(inputFolder,prefix,tokenName,keyword){
    
    tokenfile = list.files(inputFolder,paste0(prefix,tokenName,".*",keyword))

    df = read_rds(paste0(inputFolder,tokenfile))
    price = zoo::zoo(df$openNorm,df$day)

    self$df = df
    self$tokenName = tokenName
    self$price = price
  },
  
  outputARIMAplots = function(outFolder,prefix="autoARIMA_"){
    dir.create(outFolder,showWarnings = FALSE)
    
    df = self$df
    splitRatio = self$splitRatio
    train = df[1:(splitRatio*nrow(df)),]
    test = df[(splitRatio*nrow(df)+1):nrow(df),]
    
    startDate = train$day[1]
    
    price_train = ts(train$openNorm,
                     start = c(as.numeric(format(startDate,"%Y")),
                               as.numeric(format(startDate,"%j"))),
                     frequency = 365)
    
    startDate = test$day[1]
    price_test = ts(test$openNorm,
                    start = c(as.numeric(format(startDate,"%Y")),
                              as.numeric(format(startDate,"%j"))),
                    frequency = 365)
    
    fit = auto.arima(price_train,D=0)
    fp = forecast(fit,h=nrow(test))
    
    g1 = autoplot(fp) +
      autolayer(price_test,series = "price") +
      labs(subtitle = paste("No External X")) + ylab("Normalized Open Price") +
      theme(legend.position="top", 
            legend.justification="right",
            legend.direction="horizontal")
    
    features = colnames(train)
    xvars = list(M1 = features[3:5],
                 M2 = features[3:6],
                 M3 = features[3:7],
                 M4 = features[3:8])
    
    plots = list()
    for (i in seq(length(xvars))) {
      fit = auto.arima(price_train,D=0,xreg = train %>% select(one_of(xvars[[i]])) %>% as.matrix())
      fcast = forecast(fit,h = nrow(test),xreg = test %>% select(one_of(xvars[[i]])) %>% as.matrix())
      plots[[i]] = autoplot(fcast) + autolayer(price_test,series = "Acutal Price") +
        labs(subtitle = paste("Model Type:",names(xvars[i]))) + ylab("Normalized Open Price") +
        theme(legend.position="none", 
              legend.justification="right",
              legend.direction="horizontal")
    }
    lay = rbind(c(1,NA),
                c(2,3),
                c(4,5))
    
    g2 = gridExtra::arrangeGrob(
      g1,textGrob(self$tokenName,gp=gpar(fontsize=50)),
      plots[[1]],plots[[2]],
      plots[[3]],plots[[4]], nrow = 3)
    
    ggsave(paste0(outFolder,prefix,self$tokenName,".png"),g2,width = 20,height = 15)
    
    invisible(self)
  },
  
  laggedARIMA = function(ofolder,prefix="lagged_",lag_max=4,plotting=FALSE,dataFolder){
    
    df = self$df
    splitRatio = self$splitRatio
    tokenName = self$tokenName
    # lag_max = 4
    # prefix = "lagged_"
    # remove unrelated columns
    df = df %>% select(-tk,-Open,-contains("flag"))
    
    # becasue the rolling bett can be NA at first few rows, remove them
    df = df %>% filter(!is.na(B1))
    
    # create lagged value for xreg variables
    
    lagVars = c("vertexNum","edgeNum","clusterCoef","B0","B1","B2")
    
    xvars = list(M1 = lagVars[1:3],
                 M2 = lagVars[1:4],
                 M3 = lagVars[1:5],
                 M4 = lagVars[1:6])
    df_lag = df
    
    for (v in lagVars) {
      # designed function
      df_lag = lag_mutate(df_lag,v,lag_max)
    }
    # df_lag %>% names()
    # remove first few rows, since the lagged data is NA
    df_lag = df_lag[-(1:lag_max),]
    
    train = df_lag[1:(splitRatio*nrow(df_lag)),]
    test = df_lag[(splitRatio*nrow(df_lag)+1):nrow(df_lag),]
    
    startDate = train$day[1]
    
    price_train = ts(train$openNorm,
                     start = c(as.numeric(format(startDate,"%Y")),
                               as.numeric(format(startDate,"%j"))),
                     frequency = 365)
    
    startDate = test$day[1]
    price_test = ts(test$openNorm,
                    start = c(as.numeric(format(startDate,"%Y")),
                              as.numeric(format(startDate,"%j"))),
                    frequency = 365)
    
    # price only
    fit = auto.arima(price_train,D=0)
    fp = forecast(fit,h=nrow(test))
    self$tmp = list(fp,price_test,test,train)
    # 
    # g1 = autoplot(fp) +
    #   autolayer(price_test,series = "Actual price") +
    #   labs(title = paste("Pirce Only")) +
    #   ylab("Normalized Price") + xlab("Time ( % of a year)") +
    #   theme(legend.position="top", 
    #         legend.justification="right",
    #         legend.direction="horizontal")+
    #   theme_minimal()
    
    
    ggdf2 = tibble(day=test$day,
                   openNorm = fp$mean %>% as.vector(),
                   CI95upper = fp$upper[,2] %>% as.vector(),
                   CI95lower = fp$lower[,2] %>% as.vector())
    
    
    g1 = ggplot() + 
      geom_ribbon(data=ggdf2,aes(x=day,ymin=CI95lower,ymax=CI95upper,fill="CI_95"),
                  alpha=0.4) +
      geom_line(data = ggdf2,aes(x=day,y=openNorm,color='predicted')) +
      geom_line(data = df,aes(x=day,y=openNorm)) +
      scale_fill_manual("",values="steelblue1") +
      scale_color_manual("",values = c("predicted"="cornflowerblue")) +
      scale_x_date(date_breaks = "18 weeks",date_labels="%b%y") + 
      theme_minimal() +
      labs(subtitle = "Price only",y="Price",x='Date') +
      theme(legend.position = 'top',
            legend.box = "horizontal",
            legend.box.background = element_rect(fill="white",colour="white"),
            text = element_text(size=20),
            legend.text = element_text(size = 14))
    
    lagged_prediction = map(seq(lag_max),function(lag_cur){
      
      plots = list()
      pred = list()
      
      # for one lag type output one plot
      for (i in seq(length(xvars))) {
        
        xregVars = map(seq(lag_cur),function(lag_i){
          paste0(xvars[[i]],'_lag',lag_i)
        }) %>% unlist()
        
        # xregVars
        train_xreg = train %>% select(one_of(xregVars)) %>% as.matrix()
        fit = auto.arima(price_train,D=0,xreg = train_xreg)
        
        test_xreg = test %>% select(one_of(xregVars)) %>% as.matrix()
        fcast = forecast(fit,h=nrow(test),xreg = test_xreg)
        
        # predicted data
        tmp = fcast$mean %>% as.vector() %>%  tibble()
        names(tmp) = names(xvars[i])
        
        # print(tmp)
        # pred[[i]] = tmp # for one single model type
        
        # plots[[i]] = autoplot(fcast) + autolayer(price_test,series = "Actual Price") +
        #   labs(title  = paste("Model Type:",names(xvars[i]))) + 
        #   ylab("Normalized Price") + xlab("Time ( % of a year)") +
        #   theme(legend.position="none", 
        #         legend.justification="right",
        #         legend.direction="horizontal") +
        #   theme_minimal()
        
        ggdf2 = tibble(day=test$day,
                       openNorm = fcast$mean %>% as.vector(),
                       CI95upper = fcast$upper[,2] %>% as.vector(),
                       CI95lower = fcast$lower[,2] %>% as.vector())
        
       plots[[i]] = ggplot() +
         geom_ribbon(data=ggdf2,aes(x=day,ymin=CI95lower,ymax=CI95upper,fill="CI_95"),
                     alpha=0.4) +
         geom_line(data = ggdf2,aes(x=day,y=openNorm,color='predicted')) +
         geom_line(data = df,aes(x=day,y=openNorm)) +
         scale_fill_manual("",values="steelblue1") +
         scale_color_manual("",values = c("predicted"="cornflowerblue")) +
         scale_x_date(date_breaks = "18 weeks",date_labels="%b%y") + 
         theme_minimal() +
         labs(subtitle = paste("Type:",names(xvars[i])),y="",x='Date') +
         theme(legend.position = 'top',
               legend.box = "horizontal",
               legend.box.background = element_rect(fill="white",colour="white"),
               text = element_text(size=20),
               legend.text = element_text(size=14))
        
         
      }
      
      pred_tb = bind_cols(pred)
      # add the day informaiton
      
      pred_tb = bind_cols(test %>% select(day),pred_tb)
      # print(pred_tb)
      pred_tb$diff = lag_cur
      
      # plot(g2)
      if (plotting) {
        # g2 = gridExtra::arrangeGrob(
        #   textGrob(paste(tokenName," Lagged days:",lag_cur),gp=gpar(fontsize=20)),
        #   g1,
        #   plots[[1]],plots[[2]],
        #   plots[[3]],plots[[4]], nrow = 3)
        
        g2 = gridExtra::arrangeGrob(grobs = c(list(g1),plots),nrow=1)
        ggsave(paste0(ofolder,prefix,tokenName,"_lag",lag_cur,".png"),g2,width = 20,height = 4)
      }
      
      # return(pred_tb)
    })
    
    # print(lagged_prediction)
    
    self$test = test
    self$pred = bind_rows(lagged_prediction)
    write_rds(self$pred,paste0(dataFolder,"lagPred_",self$tokenName,".rds"))
    invisible(self)
    
  },
  
  laggedARIMA_performance = function(outputFolder,prefix){
    
    pred = self$pred
    test = left_join(pred %>% select(day) %>% distinct() %>% arrange(day),
                     self$df %>% select(day,matches("flag")),by="day")
    ## flag prediction days
    flag_max = 7
    modelTypes = c("M1","M2","M3","M4")
    
    diffs = pred$diff %>% unique()
    
    ## convert predicted price into binary flagged data
    pred_flag = map_dfr(diffs,function(diff_i){
      df = pred %>% filter(diff==diff_i) %>% arrange(day)
      
      # v = "M1"
      for (v in modelTypes) {
        # iterate each model type  
        tb_v = quo(!!rlang::sym(v))
        # flag from 1 to 7 days, default threshold is 0.25
        for (i in seq(flag_max)) {
          df = df %>% mutate(!!paste0(v,"_flag",i):= flagDaysFun(!!tb_v,ndays=i,threshold = 0.25))
        }
      }
      return(df)
    })
    
    test = test %>% mutate_if(is.logical,as.factor)
    pred_flag = pred_flag %>% mutate_if(is.logical,as.factor)
    
    res_measure = map_dfr(modelTypes,function(v){
      
      res_model = map_dfr(diffs,function(diff_i){
        df = pred_flag %>% filter(diff==diff_i)
        res_ModelDiff = map_dfr(seq(flag_max),function(flag_i){
          cf = caret::confusionMatrix(df %>% pull(paste0(v,"_flag",flag_i)),test %>% pull(paste0("flag",flag_i)),positive="TRUE")
          cf$overall['Accuracy']
          cf$byClass
          cfmatrix = rbind(as.matrix(cf,what="overall"),
                           as.matrix(cf,what="classes"))
          cftb = cfmatrix %>% as.data.frame() %>% rownames_to_column()  %>% as_tibble() 
          names(cftb) = c("measure",'value')
          cftb$modelType = v
          cftb$diff = diff_i
          cftb$flag = flag_i
          return(cftb)
        })
        
      })
      
    })
    write_rds(res_measure,paste0(outputFolder,prefix,self$tokenName,".rds"))
    self$measure = res_measure
    invisible(self)
  },
  
  lagged_measurePlots = function(m,outputFolder,prefix){
    
    df= self$measure
    tokenName = self$tokenName
    # m = "Accuracy"
    
    df = df %>% filter(measure==m)
    df = df %>% mutate(diff=as.factor(diff))
    
    
    plots = list()
    flags = df$flag %>% unique()
    ymin = min(df$value,na.rm = TRUE)
    ymin = floor(ymin*100)/100
    
    for (flag_i in flags) {
      df_flag = df %>% filter(flag==flag_i)
      
      
      plots[[flag_i]]=ggplot(data = df_flag,aes(x=diff,y=value,fill=modelType)) + 
        geom_bar(position="dodge",stat = "identity") +
        coord_cartesian(ylim=c(ymin,1)) + 
        labs(title = paste(m," Flag Horizon: ",flag_i)) + 
        theme_minimal()
      
    }
    
    plotList = c(list(textGrob(paste(tokenName,m),gp=gpar(fontsize=20))),plots)
    
    g2 = gridExtra::arrangeGrob(
      grobs = plotList, nrow = 3)
    
    ggsave(paste0(outputFolder,prefix,m %>% str_remove_all(" "),"_",tokenName,".png"),
           g2,width = 12,height = 8)
    
    invisible(self)
    
  },
  
  EG_test = function(outputFolder,prefix = "coin_",max_diff = 7){
    dir.create(outputFolder,showWarnings = FALSE)
    
    df = self$df
    tokenName = self$tokenName
    
    df = df %>% select(-tk,-Open,-contains("flag"))
    # becasue the rolling bett can be NA at first few rows, remove them
    df = df %>% filter(!is.na(B1))
    # create lagged value for xreg variables
    lagVars = c("vertexNum","edgeNum","clusterCoef","B0","B1","B2")
    
    xvars = list(M1 = lagVars[1:3],
                 M2 = lagVars[1:4],
                 M3 = lagVars[1:5],
                 M4 = lagVars[1:6])
    
    y = df$openNorm
    
    res = map_dfr(seq(length(xvars)),function(i){
      X = df %>% select(one_of(xvars[[i]]))
      
      res_oneType = map_dfr(seq(max_diff),function(j){
        # EG cointegration test
        res = aTSA::coint.test(y,X %>% as.matrix(),d=j,output = FALSE)
        res = res %>% as.data.frame() %>% as_tibble() %>% rownames_to_column("LinearType")
        
        res$ModelType = names(xvars[i])
        res$diff = j
        return(res)
      })
      
    })
    res$tk = tokenName
    
    # output the res
    write_rds(res,paste0(outputFolder,prefix,tokenName,".rds"))
    self$egdf = res
    
    invisible(self)
  },
  
  EG_plots = function(outputFolder,prefix="coin_"){
    
    df = self$egdf
    tokenName = self$tokenName
    
    typeLookUp = c("1" = "NO drift and NO linear trend",
                   "2" = "drift and NO linear trend",
                   "3" = "drift and linear trend")
    
    egplots = list()
    pvplots = list()
    
    for (i in 1:3) {
      df_i = df %>% filter(LinearType==i)
      
      egplots[[i]]= ggplot(data = df_i,aes(x=diff,y=EG,color=ModelType)) + geom_line() + geom_point() +
        theme_minimal() + 
        labs(title = paste(tokenName,"--- EG test Null:",typeLookUp[i]),
             y="EG test statistics")
      
      pvplots[[i]]= ggplot(data = df_i,aes(x=diff,y=p.value,color=ModelType)) + geom_line() + geom_point() +
        theme_minimal() + 
        labs(title = paste(tokenName,"--- EG test Null:",typeLookUp[i]),
             subtitle = "p-value=0.1 means greater equal than 0.1",
             y="p-value")
      
    }
    
    g2 = gridExtra::arrangeGrob(
      egplots[[1]],pvplots[[1]],
      egplots[[2]],pvplots[[2]],
      egplots[[3]],pvplots[[3]], nrow = 3)
    
    ggsave(paste0(outputFolder,prefix,tokenName,".png"),g2,width = 20,height = 15)
    invisible(self)
  }

))











