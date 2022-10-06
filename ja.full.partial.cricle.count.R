ja.full.partial.cricle.count <- function(final.csv.features.merged, No.of.TAOI)
{
  
  #the final df for full/partial JA cricles counts
  #for each TAOI we will compute two numbers for full/partial JA cricles counts
  #the first col would be the sbj ID
  #MANUAL!!!
  no.of.subjects <- 1
  subject.NO <- 1
  ja.cricles.counts.df <-  matrix(nrow = no.of.subjects, 
                                  ncol = (2*No.of.TAOI +1) )
  ja.cricles.counts.df <- 
    as.data.frame(ja.cricles.counts.df)
  #assigning the colnames
  #seq(2,2*No.of.TAOI,2) is the vector corresponding to the even columns that are supposed to have number of full JA cycles
  #seq(3,2*No.of.TAOI+1,2) is the vector corresponding to the even columns that are supposed to have number of partial JA cycles
  colnames(ja.cricles.counts.df)[seq(2,2*No.of.TAOI,2)] <- 
    paste(TAOI.Target.ColName.Vctr, ".No.of.Full.JA.Cycles", sep = "__")
  colnames(ja.cricles.counts.df)[seq(3,2*No.of.TAOI+1,2)]<- 
    paste(TAOI.Target.ColName.Vctr, ".No.of.Partial.JA.Cycles", sep = "__")
  colnames(ja.cricles.counts.df)[1] <- "SubjectID"
  
  
  #in each iteratoin full/partial JA cricles will be calculated for one TAOI
  
  for (TAOI.No in c(1:No.of.TAOI)) 
  {
    #TAOI.No <- 1
    TOA.col.index <-  
      (TAOI.No - 1 ) * 6 + 1 
    No.of.Partial.JA.Circle <- 0 
    No.of.Full.JA.Circle <- 0 
    
    if (final.csv.features.merged[1,TOA.col.index] == -1) #this TAOI was not hitted
    {
      next()
    }
    
    #calculating the number of rows that we have in the corresponding column for the current TAOI
    no.of.rows <- 
      length(na.omit(final.csv.features.merged[,TOA.col.index]))
    
    #in this loop I'm going to count the "No.of.Partial.JA.Circle" 
    #and "No.of.Full.JA.Circle"
    #So, in each iteration; I should check the three last areas that were hitted
    
    current.row.index <- 1
    while (current.row.index <= (no.of.rows -2) ) 
    {
      first.hitted.area <- 
        final.csv.features.merged[current.row.index,TOA.col.index]
      second.hitted.area <- 
        final.csv.features.merged[current.row.index + 1,TOA.col.index]
      third.hitted.area <- 
        final.csv.features.merged[current.row.index + 2,TOA.col.index]
      
      last.three.looking.are.vctr <- 
        c(first.hitted.area, second.hitted.area, third.hitted.area)
      
      is.Full.JA.Circle <- 
        (sum(last.three.looking.are.vctr == c("T","F","T")) == 3) | (sum(last.three.looking.are.vctr == c("F","T","F")) == 3)
      
      #check for full joint attention
      if (is.Full.JA.Circle) 
      {
        No.of.Full.JA.Circle <- 
          No.of.Full.JA.Circle + 1
        #update the current.row.index for the nex iteration
        #the nex circle can start from  current.row.index + 2  (has overlap with the current one)
        current.row.index <- current.row.index + 2 
        next()
      }
      #The same for partial JA cricle======
      is.Partial.JA.Circle <- 
        (sum(last.three.looking.are.vctr == c("H","F","H")) == 3) |
        (sum(last.three.looking.are.vctr == c("H","T","H")) == 3) |  (sum(last.three.looking.are.vctr == c("F","H","F")) == 3) |  (sum(last.three.looking.are.vctr == c("T","H","T")) == 3)
      
      #check for full joint attention
      if (is.Partial.JA.Circle) 
      {
        No.of.Partial.JA.Circle <- 
          No.of.Partial.JA.Circle + 1
        #update the current.row.index for the nex iteration
        #the nex circle can start from  current.row.index + 2  (has overlap with the current one)
        current.row.index <- current.row.index + 2 
        next()
      }
      
      
      
      #update the current.row.index for the nex iteration
      current.row.index <- current.row.index + 1 
    }
    
    #write the JA.Circles
    ja.cricles.counts.df[subject.NO, TAOI.No*2] <- 
      No.of.Full.JA.Circle
    ja.cricles.counts.df[subject.NO, TAOI.No*2+1] <- 
      No.of.Partial.JA.Circle
    
    
  }
  return(ja.cricles.counts.df) 
}
