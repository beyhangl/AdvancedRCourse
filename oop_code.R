#Q1
library(psych)

make_LD<-function(x)
{
  structure(x, class = "LongitudinalData")
}


subject<-function(x,y)
{
  data.frame(x, x$visit, x$value)
  
}
