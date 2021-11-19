### Biocomp Exercise 9 
### Nov 19, 2021
### Dianna Perez

##create a fn that computes the coefficient(s) of variation for a specified 
##    column across file(s) in a specified directory

#specify the variable dir as the directory path of interest (later to be 
#   replaced with user input in first arg of fn call)
dir <- "~/Desktop/BiocompR/WagesData" 

#specify column variable, placeholder for what user will specify 
column <- 2

wages<-"wages.csv"

coeffvarfn <- function(dir, column, override = FALSE){
  #set the present working directory to the directory path 
  #setwd(dir)
 
  #generate the list of files in given directory to iterate through 
  files <- list.files(path=dir, full.names = TRUE)
  unlist(files) #this makes the list of files a vector, for indexing purposes
  
  #create vector to hold coeffs of variation
  coeffvector <- numeric(length=length(files))
  
  #iterate through each file and compute the coeff of var if file meets criteria
  for(i in 1:length(files)){
    
    #convert csv file to a data frame
    dframe<-read.table(file = files[i], header=TRUE, sep=",", stringsAsFactors = FALSE )
    
    #print warning if < 50 obs and override=TRUE, otherwise print error and put
    # NA for that file in vector
    if(nrow(dframe) < 50){ 
        
      if(override==FALSE){
          print(files[i])
          print("Error, this file contains less than 50 observations. A coefficient of variation will not be computed and the output vector will contain 'NA' for this file's computation, which can be found in the following index of the output vector: ")
          print(i)
          coeffvector[i]<-NA
        } else if(override==TRUE){
          print(files[i])
          print("Warning, this file contains less than 50 observations. The computed coefficient of variation for this file will be less reliable.")
          print("The unreliable coefficient of variation for this file can be found in index ")
          print(i)
          print(" of the output vector.")
          
            if(sum(is.na(dframe[,column])) > 0){
              print(files[i])
              print("This file has NA values present in the designated column, they were ignored in the computation and the resulting coefficient of variation can be found in the following index of the output vector:")
              print(i)
              dframe<- complete.cases(dframe[,column]) #ignores NAs
              coeffvector[i]=sd(dframe)/mean(dframe)
            } else {
              coeffvector[i]=sd(dframe[,column])/mean(dframe[,column])
            }
        }#end override conditional
      
    } else if(nrow(dframe) >= 50) {
      if(sum(is.na(dframe[,column])) > 0){
        print(files[i])
        print("This file has NA values present in the designated column, they were ignored in the computation and the resulting coefficient of variation can be found in the following index of the output vector:")
        print(i)
        dframe<- complete.cases(dframe[,column]) #ignores NAs
        coeffvector[i]=sd(dframe)/mean(dframe)
      } else {
        coeffvector[i]=sd(dframe[,column])/mean(dframe[,column])
      }
    } #end observation length conditional

  } #end for loop
  return(coeffvector)
}
