calculate_mean <- function(data){
  
  data_len <- length(data)
  total <- 0
  for (i in 1:data_len){
    total <- total + as.numeric(data[i])
  }
  answer <- total/data_len
  mean <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(mean))
}
mode <- function(data){
  unique_data <- unique(data)
  count <<- array()
  
  for ( i in 1:(length(unique_data)))
  {
    len <- length(grep(unique_data[i],data))
    count[i] <<- len
  }  
  index <- which(count == max(count))
  
  if( length(index) == 1 )
  {
    mode_value  <- unique_data[index]  
    ;return(mode_value)
  }
  else 
  {
    ;return('ERROR')
  }  
  
}

cal_median<-function(data){
  data <- sort(data)
  data_len <- length(data)
  
  if ((data_len %% 2) == 0)
  {
    index <- data_len/2
    index2 <- index+1
    median_element <- data[index]
    median_element2 <- data[index2]
    answer <- (median_element + median_element2)/2
    median_avg <- formatC(answer,digits = 6,format = "f")
    return(median_avg)
  }
  else {
    index <- (data_len+1)/2
    answer <- data[index]
    median_element <- formatC(answer,digits = 6,format = "f")
    return(median_element)
  }
  
  
}

cal_quartiles<-function(data){
  data_len <-length(data)
  sorted <- sort(data)
  #cat('\n sorted data:',sorted)
  q1_index <- as.numeric(data_len/4)+1
  q2 <- cal_median(data)
  q3_index <- as.numeric((3*data_len)/4)+1
  
  answer <- c(sorted[q1_index],q2,sorted[q3_index])
  quartile <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(quartile))
  
  
}

IQR<-function(data){
  quartile <- cal_quartiles(data)
  answer <- quartile[3]-quartile[1]
  iqr_data <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(iqr_data))
}

cal_skewness<-function(data){
  quartile <- cal_quartiles(data)
  answer <- ((quartile[3]+quartile[1])-(2*quartile[2])) / (IQR(data))
  skewness_data <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(skewness_data))
}
minimum<-function(data){
  sorted <- sort(data)
  return(sorted[1])
}
maximum<-function(data){
  sorted <- sort(data)
  data_len <- length(data)
  return(sorted[data_len])
}

range<-function(data){
  sorted_data <- sort(data)
  data_len <- length(data)
  answer <- data[data_len]-data[1]
  range_data <- formatC(answer,digits = 6,format = "f")
  return(range_data)
}

mean_absolute_dev<-function(data){
  data_len <- length(data)
  mean_data <- calculate_mean(data)
  summition <- 0
  for (i in 1:data_len){
    
    summition <-  summition + abs(data[i]-mean_data)
  }
  
  answer <- summition/data_len
  madev <- formatC(answer,digits = 6,format = "f")
  return(madev)
}
variance<-function(data)
{
  data_len <- length(data)
  total <- 0
  for (i in 1:data_len){
    product <- 1
    product <- data[i]*data[i]
    total <- total + product
  }
  mean_data <- calculate_mean(data)
  answer <- (total-(data_len * mean_data * mean_data)) / (data_len-1)
  var <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(var))
  
  
}


standard_dev<-function(data){
  answer <- sqrt(variance(data))
  sd <- formatC(answer,digits = 6,format = "f")
  return(sd)
}

temp_dev<-function(data,pow){
  summition <- 0
  data_len <- length(data)
  mean_data <- calculate_mean(data)
  for (i in 1:data_len){
    summition <-  summition + ((data[i]-mean_data)**pow)
  }
  return(summition)
  
}
moments<-function(data){
  m<-temp_dev(data,1)
  m1<-formatC(m,digits = 6,format = "f")
  m2<-formatC(temp_dev(data,2),digits = 6,format = "f")
  m3<-formatC(temp_dev(data,3),digits = 6,format = "f")
  m4<-formatC(temp_dev(data,4),digits = 6,format = "f")
  moment<-c(m1,m2,m3,m4)
  return(moment)
  
}

kurtosis<-function(data){
  moment<-moments(data)
  var<-variance(data)
  m4<-moment[4]
  m4f <- as.numeric(m4)
  k<-(m4f/(var**2))-3
  return(k)
}


descriptive_analysis<-function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------DESCRIPTIVE ANALYSIS---------')
    #cat('\n*****ENTER YOUR CHOICE*****')
    cat('\n 1.Mean \n 2.Median \n 3.Mode \n 4.Variance \n 5.Standard Deviation \n 6.Mean Absolute Deviation \n 7.Range \n 8.Quartiles \n 9.IQR \n 10.Minimum \n 11.Maximum \n 12.Skewness \n 13.Kurtosis \n 14.Moments')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('*****Mean of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********MEAN OF THE DATA**********
      m_data<-calculate_mean(data)
      cat('\n \n Mean of the data is: ',m_data)
      
    }
    else if (choice1==2){
      print('\n *****Median of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #******MEDIAN OF THE DATA***********
      med_data<-cal_median(data)
      cat('\n \n Median of the data: ',med_data)
      
    }
    else if (choice1==3){
      print('\n *****Mode of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #******MODE OF THE DATA***********
      mode_data <- mode(data)
      cat('\n \n Mode of the data: ',mode_data)
      
    }
    else if (choice1==4){
      print('\n *****Variance of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********VARIANCE OF THE DATA************
      var_data<-variance(data)
      cat('\n \n Variance of the data is :',var_data)
      
    }
    else if (choice1==5){
      print('\n *****Standard deviation of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #**********STANDARD DEVIATION OF THE DATA********
      sd_data<-standard_dev(data)
      cat('\n \n Standard Deviation of the data is :',sd_data) 
      
    }
    else if (choice1==6){
      print('\n *****Mean absolute deviation of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #***********MEAN ABSOLUTE OF THE DATA **********
      mean_a_dev<-mean_absolute_dev(data)
      cat('\n \n Mean Absolute Deviation of the data is : ',mean_a_dev)
      
    }
    
    else if (choice1==7){
      print('\n *****Range of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********RANGE OF THE DATA***************
      range_of_data<-range(data)
      cat('\n \n Range of the data is: ',range_of_data)
      
    }
    else if (choice1==8){
      print('\n *****Quartiles of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*******QUARTILE OF THE DATA**************
      quartile_data<-cal_quartiles(data)
      cat('\n \n quartiles of the data is : Q1=',quartile_data[1],' Q2=',quartile_data[2],' Q3=',quartile_data[3])
      
    }
    else if (choice1==9){
      print('\n *****IQR of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********IQR OF THE DATA**************
      iqr_of_data<-IQR(data)
      cat('\n \n IQR of the data is: ',iqr_of_data)
      
    }
    else if (choice1==10){
      print('\n *****Minimum of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********MINIMUM OF THE DATA*****
      min<-minimum(data)
      cat('\n \n Minimum of the data: ',min)
      
    }
    
    else if (choice1==11){
      print('\n *****Maximum of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********MAXIMUMOF THE DATA******
      max<-maximum(data)
      cat('\n \n Maximum of the data: ',max)
      
    }
    else if (choice1==12){
      print('\n *****Skweness of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********SKEWNESS OF THE DATA*********
      skewness_of_data<-cal_skewness(data)
      cat('\n \n Skewness of the data: ',skewness_of_data)
      
    }
    else if (choice1==13){
      print('\n *****Kurtosis of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      kur<-kurtosis(data)
      cat('\n \n Kurtosis of the data: ',kur)
      
    }
    else if (choice1==14){
      print('\n *****Moments of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********MOMENTS OF THE DATA*******
      moments_data<-moments(data)
      cat('\n \n ^^Moments^^ \n m1=',moments_data[1],'m2=',moments_data[2],'m3=',moments_data[3],'m4=',moments_data[4])
      
    }
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = '\n Do you want to continue with descriptive analysis (yes/no): ')
    
    }
}
descriptive_analysis()