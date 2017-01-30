##########################PROJECT PART 1

#Version 1
Factorial_loop<-function(number)
{
  for(i in 1:number) {if(number>0){result=result*i }}
}

#Version 2
Factorial_reduce<-function(number)
{
  library(purrr)
  if(number>0)
  {
    reduce(1:number, function(x, y){
    x * y
  })
}
}
#Version 3
Factorial_func<-function(number) {if(number<=0) 1 else number*Factorial_func(number-1) }

#Version 4


createMemFactorial <- function() {
  res <- 1
  memFactorial <- function(n) {
    if (n == 1) return(1)

    if (length(res) < n) res <<- `length<-`(res, n)

    if (!is.na(res[n])) return(res[n])

    res[n] <<- n * factorial(n-1)
    res[n]
  }
  memFactorial
}
memFactorial <- createMemFactorial()


##microbenchmark
library(microbenchmark)
Factorial_loop_perf <- microbenchmark(Factorial_loop(5))
Factorial_reduce_perf <- microbenchmark(Factorial_reduce(5))
Factorial_func_perf <- microbenchmark(Factorial_func(5))