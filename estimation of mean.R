estimation_of_mean <- function(n,sample_mean,variance,alpha){
  half_alpha <- alpha/2
  z_half_alpha<-qnorm(1-half_alpha)
  right <- sample_mean + (z_half_alpha * sqrt(variance/n))
  left <- sample_mean - (z_half_alpha * sqrt(variance/n))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
}

estimation_of_mean_T<-function(n,sample_mean,variance,alpha){
  half_alpha <- alpha/2
  t_half_alpha<-qt(1-half_alpha,df=n-1)
  right <- sample_mean + (t_half_alpha * sqrt(variance/n))
  left <- sample_mean - (t_half_alpha * sqrt(variance/n))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)

}
main<-function(){
  n <- as.numeric(readline(prompt = "Enter the sample size: "))
  s_mean <- as.numeric(readline(prompt = "Enter the sample mean: "))
  alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
  print("1.Population variance known 2.Population variance unknown")
  ans<-as.numeric(readline(prompt = "enter choice: "))
  
  if (ans==1){
      variance <- as.numeric(readline(prompt = "Enter the population variance: "))
      eom <- estimation_of_mean(n,s_mean,variance,alpha)
      cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
  }
  else {
    if(n>30){
      variance <- as.numeric(readline(prompt = "Enter the sample variance: "))
      eom <- estimation_of_mean(n,s_mean,variance,alpha)
      cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
    
    }
    else{
      variance <- as.numeric(readline(prompt = "Enter the sample variance: "))
      eom <- estimation_of_mean_T(n,s_mean,variance,alpha)
      cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
    
    }
    
  }
  
  
  }
main()