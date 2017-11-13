estimation_of_mean_difference <- function(n1,n2,X1,X2,variance1,variance2,alpha){
  mean_diff <- (X1-X2)
  half_alpha <- alpha/2
  z_half_alpha <- qnorm(1-half_alpha)
  right <- mean_diff + z_half_alpha * sqrt((variance1 / n1) + (variance2 / n2))
  left <- mean_diff - z_half_alpha * sqrt((variance1 / n1) + (variance2 / n2))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}

estimation_of_mean_difference_T <- function(n1,n2,X1,X2,variance1,variance2,alpha){
  half_alpha <- alpha/2
  degree_freedom <- n1 + n2-2
  t_half_alpha<-qt(1-half_alpha,df=degree_freedom)
  first <- (n1 - 1) * variance1
  second <- (n2 - 1) * variance2
  SP <- ( first + second )/ degree_freedom
  print(SP)
  right <- (X1 -X2) - (t_half_alpha * sqrt(SP) * sqrt((1/n1)+(1/n2)))
  left <- (X1 - X2) + (t_half_alpha * sqrt(SP) * sqrt((1/n1)+(1/n2)))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}


main<-function(){
  n1 <- as.numeric(readline(prompt = "Enter the first sample size: "))
  X1 <- as.numeric(readline(prompt = "Enter the first sample mean: "))
  n2 <- as.numeric(readline(prompt = "Enter the second sample size: "))
  X2 <- as.numeric(readline(prompt = "Enter the second sample mean: "))
  
  alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
  print("1.Population variance known 2.Population variance unknown")
  ans<-as.numeric(readline(prompt = "enter choice: "))
  
  if (ans==1){
    variance1 <- as.numeric(readline(prompt = "Enter the first population variance: "))
    variance2 <- as.numeric(readline(prompt = "Enter the first population variance: "))
    eom <- estimation_of_mean_difference(n1,n2,X1,X2,variance1,variance2,alpha)
    cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
  }
  else {
    if(n1>30 && n2>30){
      variance1 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
      variance2 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
      eom <- estimation_of_mean_difference(n1,n2,X1,X2,variance1,variance2,alpha)
      cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
      
    }
    else{
      variance1 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
      variance2 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
      eom <- estimation_of_mean_difference_T(n1,n2,X1,X2,variance1,variance2,alpha)
      cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
      
    }
    
  }
  
  
}
main()