estimation_of_variance<-function(n,variance,alpha){
  half_alpha <- alpha/2
  
  right <- ((n-1)*(variance**2)) / qchisq(half_alpha,n-1)
  left <- ((n-1)*(variance**2)) / qchisq(1-half_alpha,n-1)
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}
main<-function(){
  n <- as.numeric(readline(prompt = "Enter the sample size: "))
  variance <- as.numeric(readline(prompt = "Enter the sample variance: "))
  alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
  eov <- estimation_of_variance(n,variance,alpha)
  cat("confidence interval for population variance is:",eov[1],'< population_variance <',eov[2])
  
}
main()