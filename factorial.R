fact<-function(number){
  prod<-1
  for( i in 1:number){
    prod<- prod*i
  }
  return(prod)
}

main<-function(){
  print('******factorial of a number********')
number<-readline(prompt = "enter the number: ")
result<-fact(number)
cat('The factorial of given no.: ',result)
}
main()