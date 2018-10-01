

### to calculate the depth change & price change

simpleReturn <- function(x){
  size = length(x)
  y = c(NA,x)[1:size]
  res = (x-y) / y
  return(res)
}