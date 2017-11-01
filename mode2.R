unique_el <- unique(y)
count <<- array()

for ( i in 1:(length(unique_el)))
{
  len <- length(grep(unique_el[i],y))
  count[i] <<- len
}  
index <- which(count == max(count))

if( length(index) == 1 )
{
  mode_value  <- unique_el[index]  
  ;return(mode_value)
}
else 
{
  ;return('ERROR')
}  
