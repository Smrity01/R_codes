estimation_of_proportion_diff<-function(n1,theta1,n2,theta2,alpha){
  half_alpha <- alpha/2
  z_half_alpha <- qnorm(1 - half_alpha)
  right <- (theta1 - theta2) + z_half_alpha * sqrt(((theta1 * (1 - theta1)) / n1) + ((theta2 * (1 - theta2)) / n2))
  left <- (theta1 - theta2) - z_half_alpha * sqrt(((theta1 * (1 - theta1)) / n1) + ((theta2 * (1 - theta2)) / n2))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}
main<-function(){
  n1 <- as.numeric(readline(prompt = "Enter the first sample size: "))
  theta1 <- as.numeric(readline(prompt = "Enter the proportion: "))
  n2 <- as.numeric(readline(prompt = "Enter the second sample size: "))
  theta2 <- as.numeric(readline(prompt = "Enter the second proportion: "))
  alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
  eop <- estimation_of_proportion_diff(n1,theta1,n2,theta2,alpha)
  cat("confidence interval for theta1-theta2 is:",eop[1],'< theta1-theta2 <',eop[2])
  
}
main()