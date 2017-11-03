fact<-function(number){
  prod <- 1
  for( i in 1:number){
    prod <- prod*i
  }
  return(prod)
}
permutation<-function(n,r){
  num <- fact(n)
  print(num)
  diff <- fact(n-r)
  print(diff)
  permut <- (num/diff)
  return(permut)
  
}

combination<-function(n,r){
  fact_n <- fact(n)
  diff <- fact(n-r)
  print(diff)
  fact_r <- fact(r)
  print(fact_r*diff)
  answer <- (fact_n/(fact_r*diff))
  comb <- formatC(answer,digits = 6,format = "f")
  return(comb)
  
}

basic_probability <-function(favorable,total){
  answer<- (favorable/total)
  prob <- formatC(answer,digits = 6,format = "f")
  return(prob)
}


intersection <- function(set1,set2){
  count<-0
  len_set1<-length(set1)
  len_set2<-length(set2)
  
  for (i in 1:len_set1){
    for (j in 1:len_set2){
      if (set1[i]==set2[j]){
        count<-count+1
      }
    }
  }
  
  return(count)
  
}

conditional_prob <- function(setA,setB){
  num_intersection <- intersection(setA,setB)
  num_A <- length(setA)
  num_B <- length(setB)
  prob1 <- num_intersection / num_B
  prob2 <- num_intersection / num_A
  answer<-c(prob1,prob2)
  return(answer)
}

bayes <- function(prob_Ai,B_condAi,n){
  prob_Ai_len <- length(prob_Ai)
  sum <- 0
  numerator <- as.numeric(prob_Ai[n]) * as.numeric(B_condAi[n])
  
  for (i in 1:prob_Ai_len){
    prod <- as.numeric(prob_Ai[i]) * as.numeric(B_condAi[i])
    sum <- sum + prod
  }
  answer<-(numerator/sum)
  p<-formatC(answer,digits = 6,format = "f")
  return(p)
  
}

probability_analysis <- function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------PROBABILITY ANALYSIS---------')
    #cat('\n*****ENTER YOUR CHOICE*****')
    cat('\n 1.Permutation \n 2.combination \n 3.Basic Probability \n 4.Conditional Probability \n 5.Bayes Theorem \n ')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****Permutation*****')
      n <- as.numeric(readline(prompt= "Enter the n: \n"))
      r <- as.numeric(readline(prompt= "Enter the r: \n"))
      #********Permutation**********
      p <- permutation(n,r)
      cat('\n Permuation: ',p)
      
      
    }
    else if (choice1==2){
      print('\n *****combination*****')
      n <- as.numeric(readline(prompt = 'Enter n: '))
      r <- as.numeric(readline(prompt = 'Enter r: '))
      #******Combination***********
      solution <- combination(n,r)
      cat('\n \n combination ',solution)
    }
    else if (choice1==3){
      print('\n *****Basic Probability *****')
      favourable <- as.numeric(readline(prompt = 'Enter favourable outcome: '))
      total <- as.numeric(readline(prompt = 'Enter total outcome: '))
      #******Basic Probability ***********
      bp <- basic_probability(favourable,total)
      cat('\n \n Basic Probability : ',bp)
      
    }
    else if (choice1==4){
      print('\n *****Conditional Probability*****')
      setA <- as.vector(strsplit(readline(prompt= "Enter the Set A (comma-separated list):"), ",")[[1]])
      setB <- as.vector(strsplit(readline(prompt= "Enter the setB (comma-separated list) \n"), ",")[[1]])
      
      #********Conditional Probability************
      prob<-conditional_prob(setA,setB)
      cat('\n \n Conditional Probability of P(A|B):',prob[1])
      cat('\n Conditional Probability of P(B|A):',prob[2])
    }
    else if (choice1==5){
      print('\n *****Bayes Theorem*****')
      #**********Bayes Theorem********
      prob_Ai<-as.vector(strsplit(readline(prompt= "Enter the Ai (comma-separated list):"), ",")[[1]])
      B_condAi<-as.vector(strsplit(readline(prompt= "Enter the B|Ai (comma-separated list):"), ",")[[1]])
      n<-as.integer(readline(prompt = "Enter which probability you want(value of 'i' in Ai|B) [enter as integer]:"))
      prob<-bayes(prob_Ai,B_condAi,1)
      cat('\n \n "Bayes Theorem" Probability (A',n,'|B):',prob) 
      
    }
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with probability analysis (yes/no): ')
    
  }
}
probability_analysis()