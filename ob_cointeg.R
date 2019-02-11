# object for 

library(tidyverse)

# input two vectors and return a true or false
coinTest = function(v1,v2){
  v1 = v1 %>% unlist()
  v2 = v2 %>% unlist()
  tmp = egcm::egcm(v1,v2,urtest = "adf")
  res = egcm::is.cointegrated(tmp)
  return(res)
}


getCumPair = function(df,threshold=0){
  if (ncol(df)>1) {
    stop()
  }
  varName = colnames(df)
  tb_var = quo(!!rlang::sym(varName))
  res = df %>% mutate(dx = c(NA,diff(!!tb_var)),d = threshold,
                      plus = ifelse(dx-d>0,dx,d),
                      minus = ifelse(dx-d<0,dx,d))
  return(list(plus = cumsum(res$plus[-1]),
              minus = cumsum(res$minus[-1])))
}


cointeg = R6::R6Class("cointeg",public = list(
  df1 = NULL,
  df2 = NULL,
  tmp = NULL,
  ct = NULL,
  partialTest = NULL,

  # input two token files 
  initialize = function(tk1,tk2){
    df1 = read_rds(tk1)
    df2 = read_rds(tk2)
    
    # first trim the time range to equal
    commonPeriod = inner_join(df1,df2,by="day") %>% pull(day)
    self$df1= df1 %>% filter(day %in% commonPeriod) %>% select(-matches("flag"))
    self$df2= df2 %>% filter(day %in% commonPeriod) %>% select(-matches("flag"))
    
  },
  
  performCoinTest = function(tk1_var="openNorm",tk2_var="openNorm",df1=self$df1,df2=self$df2){
   
    # df1 = self$df1
    # df2 = self$df2
    
    m1 = quo(!!rlang::sym(tk1_var))
    m2 = quo(!!rlang::sym(tk2_var))
    
    v1_name = df1$tk %>% unique()
    v2_name = df2$tk %>% unique()
    v1 = df1 %>% select(!!m1) 
    v2 = df2 %>% select(!!m2) 
    
    
    vt = bind_cols(v1,v2) %>% drop_na()
    v1 = vt[,1]
    colnames(v1) = tk1_var
    v2 = vt[,2]
    colnames(v2) = tk2_var
    
    v1_shocks = getCumPair(v1)
    v2_shocks = getCumPair(v2)
    
    # row-wise tibble
    res = tribble(
      ~x,~y,~coinTest,
      tk1_var,tk2_var,coinTest(v1,v2),
      paste0(tk1_var,"_plus"),paste0(tk2_var,"_plus"),coinTest(v1_shocks$plus,v2_shocks$plus),
      paste0(tk1_var,"_plus"),paste0(tk2_var,"_minus"),coinTest(v1_shocks$plus,v2_shocks$minus),
      paste0(tk1_var,"_minus"),paste0(tk2_var,"_plus"),coinTest(v1_shocks$minus,v2_shocks$plus),
      paste0(tk1_var,"_minus"),paste0(tk2_var,"_minus"),coinTest(v1_shocks$minus,v2_shocks$minus)
    )
    res$x_tk = v1_name
    res$y_tk = v2_name
    
    res = res %>% select(x_tk,x,y_tk,y,coinTest)
    
    # self$ct = res
    return(res)
    
  },
  
  partialCoinTest = function(tk1_var="openNorm",tk2_var="openNorm"){
    
    l = nrow(self$df1)
    index = floor(l*0.5)
    
    df1 = self$df1[1:index,]
    df2 = self$df2[1:index,]
    
    part_1 = self$performCoinTest(tk1_var,tk2_var,df1,df2)
    
    df1 = self$df1[(index+1):l,]
    df2 = self$df2[(index+1):l,]
    
    part_2 = self$performCoinTest(tk1_var,tk2_var,df1,df2)
    
    res = part_1 %>% rename("coinTest_p1"="coinTest")
    res$coinTest_p2 = part_2$coinTest
    
    self$partialTest = res
    invisible(self)
  }
 

))


# object for 

library(tidyverse)

coinRes = R6::R6Class("coinRes",public = list(

  initialize = function(){
  
  }

))




















