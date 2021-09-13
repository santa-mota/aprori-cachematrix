insert_val <-function(env,key,value,occurences,confid){
  
  #Inserts value into the given environment
  #Occurences refers to the positions in which the key occurs
  #Value refers to the the support of the given key
  
  key <- toString(key)
  
  #eval.parent used to make changes to the given environment itself
  #basically,a call by reference
  #henceno need to return the environment(env),as the changes are made directly there
  
  if(exists(key,env,inherits = FALSE)){
    eval.parent(substitute(env[[key]]$val <- env[[key]]$val+as.numeric(value)))
    eval.parent(substitute(env[[key]]$Tid <- c(env[[key]]$Tid,occurences)))
    eval.parent(substitute(env[[key]]$conf <- confid))
  }
  else {
    #creates a new structure(list) referring to that key,if not present
    
    eval.parent(substitute(env[[key]] <- list(val=0L,Tid=c(),conf=0)))
    eval.parent(substitute(env[[key]]$val <- env[[key]]$val+as.numeric(value)))
    eval.parent(substitute(env[[key]]$Tid <- c(env[[key]]$Tid,occurences)))
    eval.parent(substitute(env[[key]]$conf <- confid))
  
  }
}


table_lookup <- function(data){
  
  #Helper function to access data from the table
  #Given a list that exists in the table
  ex <- list_to_char(data)
  return(tt[ex])

}

create_init <- function(data,sup){
  
  #This function generates the initial identity table L1
  #The table is in the form of an environment (mset) 
  
  mset <- new.env(hash=TRUE,parent = emptyenv())
  init_ptr=1L
  
  for(i in 1:length(data)){
    
    #The various observed lists are converted to individual characters
    
    list_ex <- char_to_list(names(data[i]))
    k <- table_lookup(list_ex)
    
    #init_ptr is used to store the occurences of the data
    #Since the lists are in tabular, sorted form the occurences
    #simply equal the number of times that list occurs
    
    l <- init_ptr:(init_ptr+as.numeric(k-1))
    
    for(j in list_ex){
      insert_val(mset,j,k,l,1)
    }
    
    init_ptr<-init_ptr+as.numeric(k)
    
  }
  
  #Function rem_less is used toremove items with minimum support
  #Since confidence doen't matter in the base itemset,minimum is given as 0
  
  rem_less(mset,sup,confid = 0)
  
  return(mset)
}


create_ahead <- function(Lk,orig){
  
  #This creates the probable frequent itemset C(k+1) 
  #using the frequent itemset L(k)
  #Orig here refers to L1,used to find minimum confidence
  
  Ct <- new.env(hash=TRUE,parent = emptyenv()) 
  
  #obj refers to list of object names in the itemset L(k)
  obj <- ls(envir = Lk)
  len <- length(obj)
  
  #Combine values using the Apriori Theory
  for(i in 1:len){
    
    #First sequence of length k to be checked
    m1 <- char_to_list(obj[i])
    
    #Base Case to stop overflow
    if(i+1 > len){break;}
    
    for(j in (i+1):len){
      
      #Second sequence of length k to be combined
      m2 <- char_to_list(obj[j])
      
      #flag is the variable used to check if two objects/lists
      #can be appended using the Apriori Theory
      flag <- 0
      
      #Base Case in case of generation of C(2) from L(1) as 
      #lengths of the lists is 1 and need not be checked
      #but directly combined
      if(length(m1)==1){flag=0;}
      else
      {
        
        for(j in 1:(length(m1)-1)){
          if(m1[[j]]!=m2[[j]]){flag=1;break;}
        }
        
      }
      
      if(flag==0){
        #In case (k-2) elements of the sequences match
        #t is the combined sequence of length (k+1)
        t <- union(m1,m2)
        
        #Intersection of two Tids ,i.e., Tid ofthe combined sequence
        t_tid <- intersect(Lk[[toString(m1)]]$Tid,Lk[[toString(m2)]]$Tid);
        t_sup <- length(t_tid)
        
        
        #Find Minimum Confidence Here
        #Logically the confidence of any sequence s = Support(s-k)/Support(k)
        #where k is any non empty subsequence of the sequence s.Since the support
        #of any single element would be greater than the support of a sequence of length
        #2 or greater,hence,for minimum confidence,only singular elements are
        #considered.
        t_conf=1
        for(k in t){
          
          k <- toString(k)
          sup_ele <- orig[[k]]$val
          t_conf <- min(t_conf,(t_sup/sup_ele))
          
        }
        
        #Uncomment below statements to check support and confidence
        #of all generated combinations
        #ti <-paste(toString(t),toString(t_sup),toString(t_conf),sep = " ")
        #print(ti)
        
        
        #Insert value in the probable frequent itemset
        insert_val(Ct,t,t_sup,t_tid,t_conf)
        
      }
    }
    
  }
  return(Ct)
}



rem_less <- function(Ct,sup,confid){
  
  #This is the helper function to remove elements from the 
  #probable frequent itemset,C(k),with support and confidence
  #lesser than what is required.
  #Hence,it generates the frequent itemset L(k).
  
  obj_list <-ls(envir = Ct)
  len <- length(obj_list)
  
  
  #Support Test where support of the element is checked against minimum support
  for(i in 1:len){
    if(len==0){break;}
    if(Ct[[ obj_list[i] ]]$val < sup){
      rm(list=obj_list[i],envir = Ct,inherits = FALSE)
    }
  }
  
  obj_list <- ls(envir = Ct)
  len <- length(obj_list)
  
  
  #Confidence Test where confidence of the sequence is checked against the minimum 
  #confidence
  for(i in 1:len){
    if(len==0){break;}
    if(Ct[[obj_list[i]]]$conf < confid){
      rm(list=obj_list[i],envir=Ct,inherits=FALSE)
    }
    
  }
  return(Ct)
  
}

add_new <- function(L1,L2){
  
  #This function is used to add the elements of L(k),specifically, from L(2) onwards
  #into an environment Lans.
  obj_list <- ls(envir = L2)
  
  for(k in obj_list){
    
    insert_val(L1,k,L2[[k]]$val,L2[[k]]$Tid,L2[[k]]$conf)
  }
  
}

improved_apriori <- function(data,support=0.1,confidence=0.1){
  
  #change the column name 'FactPageAccessKey'to change the support value
  #max_support is required as a subjective value of support
  max_support <- length(data$FactPageAccesssKey)
  support_val <- support*max_support
  
  #getSeq is a function returns a sequence list from the input data
  #get table creates a table from the list containing the different sequences
  #and the times they occured
  tt <- gettablefromlist(getSeq(data))
  
  #This function creates the first itemset L(1)
  L1 <- create_init(tt,support_val)
  
  #This creates a new environment,Lans,to store the various elements from 
  #the different probable itemsets 
  #hash = TRUE makes the variable that would be declared in the environment hashed
  #leading to faster lookup and access times
  Lans <-new.env(hash=TRUE,parent = emptyenv())
  
  Lk <- L1
  
  #ls is a function which returns a list of object names (in string format)
  #of the environment specified
  len <- length(ls(envir = Lk))
  
  #This is done to perform the operation till no further elements can be added to 
  #the resulting itemset,Lans.
  while(len!=0){
    
    #Creates a probable frequent itemset,Ctemp (name for C(k) in the algorithm),
    #for further evaluation
    Ctemp <- create_ahead(Lk,L1)
    
    #Creates a frequent itemset,L(k),from Ctemp/C(k)
    rem_less(Ctemp,support_val,confidence)
    Lk <- Ctemp
    
    #Adds the remaining items in Lans
    add_new(Lans,Lk)
    
    len <- length(ls(envir = Lk))
  
  }
  
  return(Lans)
  
}



check_values <- function(env,data,lst=list(support=0L,confidence=0L,Tid=0L)){
  
  #This function is used as a helper function to check if a paricular dataset
  #is present in the environment itself
  
  #Converting to proper subscipt in case it isn't
  if(is.list(data)){data <- toString(data)}
  else if(is.vector(data)){data <- toString(char_to_list(toString(data)))}
  
  print(data)
  
  #Checks for presence of Data
  if(!exists(data,envir = env,inherits = FALSE)){
    print("Data not present in the itemset specified")
  }
  else
  {
    #If you don't want to view it,make it 0 in the list lst
    k <- env[[data]]
    if(lst$support==1){print(k$val)}
    if(lst$confidence==1){print(k$conf)}
    if(lst$Tid==1){print(k$Tid)}
  }
  
}

check_conf_rules <- function(data1,data2){
  
  
  if(is.vector(data1)){data1 <- as.list(unlist(data1))}
  else if(!is.list(data1)){data1 <- char_to_list(data1)}
  
  if(is.vector(data2)){data2 <- as.list(unlist(data2))}
  else if(!is.list(data2)){data2 <- char_to_list(data2)}
  
  
  }