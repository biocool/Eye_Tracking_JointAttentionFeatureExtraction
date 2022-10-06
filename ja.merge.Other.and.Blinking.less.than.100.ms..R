#Merging the "other" and "blinking.OR.not.looking.at.the.display" with less than 100ms. with the previous lokking area
#INPUT: "final.csv.features" a df that is the output of "ja.Preprocessing.and.FeatureExtraction" which was processed using "write.JA.lookingPatterns2HDD" function. "subjectID": subject ID which is usally the first 5 characters of the file name. 
#OUTPUT: "final.csv.features.merged" the same df that "other" and "blinking.OR.not.looking.at.the.display" with less than 100ms were merged.
ja.merge.Other.and.Blinking.less.than.100.ms. <- function(final.csv.features, subjectID)
{
  #merging "other" and "blinking.OR.not.looking.at.the.display"====
  #below df is a sample df generated in the JA.FeatExtraction.Rmd
  dim(final.csv.features)
  #View(final.csv.features)
  #for each TAOI, we have six different cols: 
  #"looking.area",
  #    "start.time.relative", "start.time.original", 
  #    "end.time.relative", "end.time.original",
  #    "duration"
  #View(final.csv.features)
  
  #Renaming the name of area to make it more simple=====
  final.csv.features[final.csv.features == "Other"] <- 
    "O"
  final.csv.features[final.csv.features == "blinking.OR.not.looking.at.the.display"] <- 
    "B"
  final.csv.features[final.csv.features == "Hand"] <- 
    "H"
  final.csv.features[final.csv.features == "Face"] <- 
    "F"
  final.csv.features[final.csv.features == "TAOI"] <- 
    "T"
  
  for (TAOI.No in c(1:No.of.TAOI)) 
  {
    
    TOA.col.index <-  
      (TAOI.No - 1 ) * 6 + 1 
    No.of.Partial.JA.Circle <- 0 
    No.of.Full.JA.Circle <- 0 
    #browser()
    if (final.csv.features[1,TOA.col.index] == -1) #this TAOI was not hitted
    {
      next()
    }
    
    #calculating the number of rows that we have in the corresponding column for the current TAOI
    no.of.rows <- 
      length(na.omit(final.csv.features[,TOA.col.index]))
    
    #in this loop I'm going to count the "No.of.Partial.JA.Circle" 
    #and "No.of.Full.JA.Circle"
    #So, in each iteration; I should check the three last areas that were hitted
    
    current.row.index <- 2
    while (current.row.index <= (no.of.rows) ) 
    {
      prev.hitted.area <- 
        final.csv.features[(current.row.index - 1),TOA.col.index]
      current.hitted.area <- 
        final.csv.features[current.row.index ,TOA.col.index]
      current.hitted.area.duration <- 
        final.csv.features[current.row.index,TOA.col.index+5]
      
      #if there two consecutive hitted area with the same name (it could happen if in the previous iterations merge happened)
      #Then the two corresponding rwos shold be merged
      should.merge.with.prev.beacuse.the.same.area <- 
        (prev.hitted.area == current.hitted.area)
      
      if (should.merge.with.prev.beacuse.the.same.area) 
      {
        #adding the duration time to the prev. duration time
        final.csv.features[current.row.index - 1,TOA.col.index + 5] <-
          final.csv.features[current.row.index - 1,TOA.col.index + 5] + current.hitted.area.duration
        
        #shifting the rest of the df one row up (just that part of the df that is correspond to the current TAOI)
        final.csv.features[c((current.row.index):(no.of.rows)),c(TOA.col.index : (TOA.col.index + 5))] <- 
          final.csv.features[c((current.row.index+1):(no.of.rows+1)),c(TOA.col.index : (TOA.col.index + 5))]
        #becuse of shifiting the number of non.NA rows decreased by one!
        no.of.rows <-
          no.of.rows - 1
        #because we moved the rows, we shouldn't change the current.row.index. 
        #Now current.row.index shows a new row 
        next()
      }
      #browser()
      #check the merging criteri
      should.merge.with.prev.beacuse.of.short.blinkingORothers <- 
        (current.hitted.area %in% c("B", "O")) & current.hitted.area.duration <= 100
      
      if (should.merge.with.prev.beacuse.of.short.blinkingORothers) 
      {
        #adding the duration time to the prev. duration time
        final.csv.features[current.row.index - 1,TOA.col.index + 5] <-
          final.csv.features[current.row.index - 1,TOA.col.index + 5] + current.hitted.area.duration
        
        #shifting the rest of the df one row up (just that part of the df that is correspond to the current TAOI)
        final.csv.features[c((current.row.index):(no.of.rows)),c(TOA.col.index : (TOA.col.index + 5))] <- 
          final.csv.features[c((current.row.index+1):(no.of.rows+1)),c(TOA.col.index : (TOA.col.index + 5))]
        #becuse of shifiting the number of non.NA rows decreased by one!
        no.of.rows <-
          no.of.rows - 1
        #because we moved the rows, we shouldn't change the current.row.index. 
        #Now current.row.index shows a new row 
        next()
      }
      #update the current.row.index for the nex iteration
      current.row.index <- current.row.index + 1 
    }
    
  }
  sbj.vctr <- 
    rep(subjectID, nrow(final.csv.features))
  final.csv.features.merged<- 
    cbind(SubjectID= sbj.vctr, final.csv.features)
  return(final.csv.features.merged)
}