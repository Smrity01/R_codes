estimation_of_proportion<-function(n,theta,alpha){
  half_alpha <- alpha/2
  z_half_alpha <- qnorm(1-half_alpha)
  right <- theta + z_half_alpha * sqrt((theta * (1 - theta)) / n)
  left <- theta - z_half_alpha * sqrt((theta * (1 - theta)) / n)
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}
main<-function(){
  n <- as.numeric(readline(prompt = "Enter the sample size: "))
  theta <- as.numeric(readline(prompt = "Enter the proportion: "))
  alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
  eop <- estimation_of_proportion(n,theta,alpha)
  cat("confidence interval for theta is:",eop[1],'< theta <',eop[2])

}
main()