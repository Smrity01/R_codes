GAMMA <- function(x){
  equation <- function(t)
  {  
    EXP <- (t**(x-1)) * (exp(-t))
  }
  return(equation)
}

gammaFunction_integrate <- function(x){
  gamma_val <- integrate(GAMMA(x),lower=0,upper=Inf)$value
  #$value <- absolute value
  return(gamma_val)  
}

main <- function(){
  cat('\f')
  cat('****CALCULATE GAMMA FUNCTION****')
  g <- as.numeric(readline(prompt= "Enter a number : "))
  answer <- gammaFunction_integrate(g)
  cat('The gamma value is: ',answer)
}
main()