#These are the general helper functions designed to modularize the code
#,and help in debugging.getSeq function here is the general bottleneck
#of the time taken in the implementation itself.
#To check,in the R console,write  >system.time(getSeq(FPA))
#followed by  >system.time(improved_apriori(FPA))

getSeq <- function(data){
  
  #This function returns a sequence of sequences(of the order of the pages visited)
  #given data in the form of a table
  #data is grouped by Userkey and SessionId in ascending order.
  data <- group_by(data,UserKey,SessionId)
  sequences <- c()
  i=1
  
  #i is the current pointer of the element being accessed in the dataset
  while(i!=nrow(data)+1){
    
    sequence <- c()
    
    #element is taken out and stored in the initial/first position of the sequence
    sequence<- c(list(data[i,"PageKey"]))
    i=i+1
    
    #checks whether the next sequenceid is in the same order as the
    #current sequenceid
    while (i!=nrow(data)+1 & data[i,"SequenceId"]==data[i-1,"SequenceId"]+1) {
      sequence <- c(sequence,list(data[i,"PageKey"]))
      i=i+1
    }
    
    #sequences refers to the final sequence of answers itself
    sequences <- c(sequences,list(sequence))
  
  }
  
  #This is used to produce a simple list of lists 
  #lapply is a function similar to a for loop annd applies the
  #unnamed function to the entire list of sequences.
  sequences <- lapply(sequences,function(x) as.numeric(unlist(x)))
  return(sequences)
}

retfactor<- function(data){
  return(as.factor(as.character(data)))
}

getval <- function(data,x,y){
  #Gets value from a list of accessed pages generated using getSeq function
  return(data[[x]][[y]])
}


gettablefromlist <- function(data,length){
  
  #Generates a table from a given list sequence
  #by generating a factor containing unique sequences and get a table from it.
  fac <- as.factor(as.character(data))
  tt1 <- sample(fac,length,rep=TRUE)
  return(table(tt1))
}


getattr <- function(data,j,k=0){
  
  #Helper function to list data from a given table
  #If k==0,it gives the sequence
  #if k==1,it gives the number of times that sequence number occured
  if(k==0){return(names(data[j]))}
  else {return(data[[j]])}
}


char_to_list <- function(ex){
  
  #Removes the 'c' ,'(' and ')' from the character,if present,and produces a 
  #list using splitting at the ',' sign.
  tmp <- gsub('\\(|\\)|c| ','',ex)
  return( as.list(as.integer(unlist(strsplit(tmp,",")))) )
}

list_to_char <-function(data){
  
  ex <- toString(data)
  
  #Adds the prefixes to make it equal to the keys in the table generated
  ex <- paste("c(",ex,")",sep = '')
  
  return(ex)
}
