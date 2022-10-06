################################################################################
#Description=======
#This function gets a dataframe of contining the relevant columns for TAOIs and returns the looking patterns for all TAOIs
################################################################################
#INPUT====
#"ja.df.all.Cols": a df of joint attentoin data (having just the relevant columns)
################################################################################
#OUTPUT====
#"looking.pattern.sequence.list": a list of looking patterns for all TAOIs
#each element of the looking.pattern.sequence.list is a vector 
#the sequence in each vector has the following pattern
#"None", "A1", "start.time", "end.time", "duration", "A2", "start.time", "end.time", "duration", "A3", "start.time", "end.time", "duration"....
#"Ai" is the ith area that has been looking at by the subject (it can be: "TAOI", "Face", "Hand", or "Other")
################################################################################
#NOTE=====
#some variables like "TAOI.Detail.4.Input", "TAOI.Target.ColName.Vctr", "TAOI.Hand.ColName.Vctr", "TAOI.Face.ColName.Vctr" are supposed to be Global variables
################################################################################
#function definition========
ja.Preprocessing.and.FeatureExtraction <- function(ja.df.all.Cols)
{
  #extracting the col index for the three types of area:Target, Hand, Face==========
  
  TAOI.Target.Col.Indx <-
    which(colnames(ja.df.all.Cols) %in% TAOI.Target.ColName.Vctr)
  
  TAOI.Hand.Col.Indx <-
    which(colnames(ja.df.all.Cols) %in% TAOI.Hand.ColName.Vctr)
  
  TAOI.Face.Col.Indx <-
    which(colnames(ja.df.all.Cols) %in% TAOI.Face.ColName.Vctr)
  
  ################################################################################
  #Preprocessing
  ################################################################################
  #time offset calculation and replacing "-1"=======
  
  #the below index shows the first row of the file that has non.NA value for the "AOI.hit..BA.Joint.Attention.04.12.2019...01_HiSweetie_Target_Face."
  #which should be the first AOI that would be activated. 
  #and it should be the first row that TOBII showed the video and so we can set that as the time = 0
  timestamp.offset.index <- 
    min(which(!is.na(ja.df.all.Cols$AOI.hit..BA.Joint.Attention.04.12.2019...01_HiSweetie_Target_Face.)))
  #View(ja.df.all.Cols)
  #dim(ja.df.all.Cols)
  
  #this value should be used to substract from the "Recording.timestamp" to show the actual one 
  my.time.offset <- 
    ja.df.all.Cols$Recording.timestamp[timestamp.offset.index]
  #selecting the rows after frist activation (the rows before that are not useful)
  ja.df.all.Cols <- 
    ja.df.all.Cols[c(timestamp.offset.index:nrow(ja.df.all.Cols)),]
  
  #applying the offset to the timestamp 
  ja.df.all.Cols$Corrected.Recording.timestamp <-
    (ja.df.all.Cols$Recording.timestamp - my.time.offset)
  
  #View(ja.df.all.Cols$AOI.hit..BA.Joint.Attention.04.12.2019...01_HiSweetie_Target_Teddy.)
  
  #replacing "-1"
  #in the original file "-1" means inactive for each AOI ("0" means active but not being looked at; "1" means active and being looked at). However for having simpler analysis I'm gonna replace "-1" with 5
  # 5 means inactive
  ja.df.all.Cols[ja.df.all.Cols == -1] <- 5
  
  #Manually adding new cols for  TAOI.Hand.Face.Sum and TAOI.Final.Hand.Face.Logical======
  
  ##Adding new empty cols 
  ja.df.all.Cols$TAOI.Hand.Face.Sum1 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum2 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum3 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum4 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum5 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum6 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum7 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum8 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum9 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum10 <- "NA"
  ja.df.all.Cols$TAOI.Hand.Face.Sum11 <- "NA"
  
  TAOI.Hand.Face.Sum.ColName.Vctr<- 
    c("TAOI.Hand.Face.Sum1",
      "TAOI.Hand.Face.Sum2",
      "TAOI.Hand.Face.Sum3",
      "TAOI.Hand.Face.Sum4",
      "TAOI.Hand.Face.Sum5",
      "TAOI.Hand.Face.Sum6",
      "TAOI.Hand.Face.Sum7",
      "TAOI.Hand.Face.Sum8",
      "TAOI.Hand.Face.Sum9",
      "TAOI.Hand.Face.Sum10",
      "TAOI.Hand.Face.Sum11")
  
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical1<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical2<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical3<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical4<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical5<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical6<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical7<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical8<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical9<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical10<- "NA"
  ja.df.all.Cols$TAOI.Final.Hand.Face.Logical11<- "NA"
  
  
  TAOI.Final.Hand.Face.Logical.ColName.Vctr<- 
    c("TAOI.Final.Hand.Face.Logical1",
      "TAOI.Final.Hand.Face.Logical2",
      "TAOI.Final.Hand.Face.Logical3",
      "TAOI.Final.Hand.Face.Logical4",
      "TAOI.Final.Hand.Face.Logical5",
      "TAOI.Final.Hand.Face.Logical6",
      "TAOI.Final.Hand.Face.Logical7",
      "TAOI.Final.Hand.Face.Logical8",
      "TAOI.Final.Hand.Face.Logical9",
      "TAOI.Final.Hand.Face.Logical10",
      "TAOI.Final.Hand.Face.Logical11")
  
  #extracting the col index vectors
  TAOI.Hand.Face.Sum.Col.Indx <-
    which(colnames(ja.df.all.Cols) %in% TAOI.Hand.Face.Sum.ColName.Vctr)
  
  TAOI.Final.Hand.Face.Logical.Col.Indx <-
    which(colnames(ja.df.all.Cols) %in% TAOI.Final.Hand.Face.Logical.ColName.Vctr)
  
  #calculating values for TAOI.Hand.Face.Sum and TAOI.Final.Hand.Face.Logical====
  #adding hand face inetgrated col============
  #one of the most important feature is the number of times that fixation was moved from/to TAOI to/from hand/face.
  #So, I,m gonna add a new col that shows hand or face was hitted. 
  #NOTE: for some TAOI hand was not defined (for example hands was in the TAOI or wasn't visible)
  #adding hand face inetgrated col for each element of the "TAOI.Target.ColName.Vctr"
  #each new col. shows whether hand or face was hitted or not 
  
  #I'm gonna add new col. which is the sum of the face and hand 
  #Based on the different values for sum we can infer whether face/hand was hitted or not:
  #1 or 6 means face/hand was hitted (1: one was hitted and the other one was activ; 6:one was hitted and the other one was inactiv)
  for (i in c(1:No.of.TAOI)) 
  {
    ja.df.all.Cols[,TAOI.Hand.Face.Sum.Col.Indx[i]] <- 
      ja.df.all.Cols[,TAOI.Face.Col.Indx[i]] + ja.df.all.Cols[,TAOI.Hand.Col.Indx[i]]
    
    ja.df.all.Cols[,TAOI.Final.Hand.Face.Logical.Col.Indx[i]] <- 
      ((ja.df.all.Cols[,TAOI.Hand.Face.Sum.Col.Indx[i]] == 1) | (ja.df.all.Cols[,TAOI.Hand.Face.Sum.Col.Indx[i]] == 6))
    
  }
  
  #I noticed that different projects has different number of columns
  #so I'm gonna use "Whole_Scene" to find that last column that should be used 
  
  last.inofrmative.col.indx <- 
    max(which(!is.na(str_match(string = colnames(ja.df.all.Cols), pattern = "Whole_Scene"))))
  #browser()
  #all column names that are related to AOIs stasrt with "AOI hit"
  first.inofrmative.col.indx <- 
    min(which(!is.na(str_match(string = colnames(ja.df.all.Cols), pattern = "AOI.hit"))))
  
  ja.df.all.AOI.hitted.logical <- (ja.df.all.Cols[,c(first.inofrmative.col.indx:last.inofrmative.col.indx)]==1)
  #dim(ja.df.all.AOI.hitted.logical)
  
  ##the below vector is a numeric vector in which if an entry equals to "0" then it means that at the corresponding time the subject was blinking OR not looking.at.the.display
  blinking.OR.not.at.the.display <- 
    rowSums(ja.df.all.AOI.hitted.logical)
  length(blinking.OR.not.at.the.display)
  
  ja.df.all.Cols <- 
    ja.df.all.Cols %>% #Defining blinking.not.at.the.display for all TAOIs
    mutate(blinking.OR.not.at.the.display.Logical = (blinking.OR.not.at.the.display == 0 ))
  
  
  #Adding "Other" cols=========
  #Adding new columns, corresponds to each TAOI, to be considered as "Other". For each TAOI, this column shows whether none of the face/hand and TAOI was being looked at or not
  
  ja.df.all.Cols$TAOI.Other.Logical1 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical2 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical3 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical4 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical5 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical6 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical7 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical8 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical9 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical10 <- "NA"
  ja.df.all.Cols$TAOI.Other.Logical11 <- "NA"
  
  
  TAOI.Other.Logical.ColName.Vctr<- 
    c("TAOI.Other.Logical1",
      "TAOI.Other.Logical2",
      "TAOI.Other.Logical3",
      "TAOI.Other.Logical4",
      "TAOI.Other.Logical5",
      "TAOI.Other.Logical6",
      "TAOI.Other.Logical7",
      "TAOI.Other.Logical8",
      "TAOI.Other.Logical9",
      "TAOI.Other.Logical10",
      "TAOI.Other.Logical11")
  
  #extracting the col index vectors for "Other"
  TAOI.Other.Logical.Col.Indx <-
    which(colnames(ja.df.all.Cols) %in% TAOI.Other.Logical.ColName.Vctr)
  
  
  
  for (i in c(1:No.of.TAOI)) 
  {
    ja.df.all.Cols[,TAOI.Other.Logical.Col.Indx[i]] <- !(ja.df.all.Cols[,TAOI.Final.Hand.Face.Logical.Col.Indx[i]] | 
                                                           (ja.df.all.Cols[,TAOI.Target.Col.Indx[i]] == 1))&(!ja.df.all.Cols$blinking.OR.not.at.the.display.Logical)
  }
  
  #Selecting the final subset of cols======
  #selecting only features that we need in addition to the time stamp
  blinking.OR.not.at.the.display.indx <- 
    which(colnames(ja.df.all.Cols) == "blinking.OR.not.at.the.display.Logical") 
  
  
  ja.df.AOIs.Cols <- 
    ja.df.all.Cols
  
  #the following list contains the [start, end] time intervals for each TAOI (time is in ms.)
  #I got this intervals from Charlene in a csv file
  #each element of this list will be used to subset the original data frame 
  #So, the below list shoud be initialized manually
  time.inetrval.TAOI.list <- list(
    TAOI1.Time.Interval = c(TAOI.Detail.4.Input$StartTime[1], TAOI.Detail.4.Input$EndTime[1]), 
    TAOI2.Time.Interval = c(TAOI.Detail.4.Input$StartTime[2], TAOI.Detail.4.Input$EndTime[2]),
    TAOI3.Time.Interval = c(TAOI.Detail.4.Input$StartTime[3], TAOI.Detail.4.Input$EndTime[3]),
    TAOI4.Time.Interval = c(TAOI.Detail.4.Input$StartTime[4], TAOI.Detail.4.Input$EndTime[4]),
    TAOI5.Time.Interval = c(TAOI.Detail.4.Input$StartTime[5], TAOI.Detail.4.Input$EndTime[5]),
    TAOI6.Time.Interval = c(TAOI.Detail.4.Input$StartTime[6], TAOI.Detail.4.Input$EndTime[6]),
    TAOI7.Time.Interval = c(TAOI.Detail.4.Input$StartTime[7], TAOI.Detail.4.Input$EndTime[7])#,
    # TAOI8.Time.Interval = c(TAOI.Detail.4.Input$StartTime[8], TAOI.Detail.4.Input$EndTime[8]),
    # TAOI9.Time.Interval = c(TAOI.Detail.4.Input$StartTime[9], TAOI.Detail.4.Input$EndTime[9]),
    # TAOI10.Time.Interval = c(TAOI.Detail.4.Input$StartTime[10], TAOI.Detail.4.Input$EndTime[10]),
    # TAOI11.Time.Interval = c(TAOI.Detail.4.Input$StartTime[11], TAOI.Detail.4.Input$EndTime[11])
  )
  
  
  ################################################################################
  #Feature Extraction
  ################################################################################
  #initialization=====
  #this list will contain the sequence of the looking pattern. So, it would be in this format: c("None, "Hand/Face", "Face", "TAOI", "Other", ....)
  
  looking.pattern.sequence.list <- 
    list(TAOI1.looking.pattern.sequence = c("None"), 
         TAOI2.looking.pattern.sequence  = c("None"),
         TAOI3.looking.pattern.sequence = c("None"),
         TAOI4.looking.pattern.sequence  = c("None"),
         TAOI5.looking.pattern.sequence  = c("None"),
         TAOI6.looking.pattern.sequence  = c("None"),
         TAOI7.looking.pattern.sequence  = c("None")#,
         # TAOI8.looking.pattern.sequence  = c("None"),
         # TAOI9.looking.pattern.sequence  = c("None"),
         # TAOI10.looking.pattern.sequence  = c("None"),
         # TAOI11.looking.pattern.sequence  = c("None")
    )
  #for each AOI we need a "no.of.gaze.shifts"
  no.of.gaze.shifts.vctr[i] <- 
    rep(0, length(looking.pattern.sequence.list))
  
  #View(ja.df.AOIs.Cols)
  #Main loop===========
  #in this loop index i shows the index of the AOI that would be processed
  for (i in c(1:No.of.TAOI)) 
  {
    # #print("========================================")
    # #print("========================================")
    # #print("========================================")
    # #print(i)
    # my.time.offset
    #loop body ========
    #selecting a subset of the original df that is correspond to the time frame of TAOI
    #Note: the "Corrected.Recording.timestamp" is the corrected time by substracting "my.time.offset", which is the offset time for the current subject, from the tobbi's original "Recording.timestamp"
    
    
    ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI <-   
      subset(ja.df.AOIs.Cols, (Corrected.Recording.timestamp >= time.inetrval.TAOI.list[[i]][1]) & 
               (Corrected.Recording.timestamp <= time.inetrval.TAOI.list[[i]][2]))
    #REMOVE
    #View(ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI)
    #dim(ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI)
    
    #this shows the first time that the TAOI was hitted #TAOI.Target.Col.Indx[i] is the column's name in the original data 
    #that is correpsond to the TAOI.Target.Col.Indx[i]
    #this shows the first time the corresponding face/hand was hitted (It is not the actual time but is the row index)
    
    time.point.first.target.hit <- 
      min(which(ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[, TAOI.Target.Col.Indx[i]] == 1)) #TAOI.Target.Col.Indx[i] shows the column in the original data 
    
    #fidning the first hit of the Face/Hand 
    #TAOI.Final.Hand.Face.Logical.Col.Indx[i] is the column's name in the original data 
    #that is correpsond to the TAOI.Final.Hand.Face.Logical.Col.Indx[i]
    #this shows the first time the corresponding face/hand was hitted (It is not the actual time but is the row index)
    time.point.first.hand.face.hit <- 
      min(which(ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[, TAOI.Final.Hand.Face.Logical.Col.Indx[i]] ==TRUE))
    #if face/hand was hitted earlier than targe then we add 1 to no.of.gaze.shifts.vctr[i] 
    #==>fianlly we can consider time.point.first.hand.face.hit == 1 as 0 (becuase it means that in fact target was not hitted OR after hitting there were not gaze shift towards face/hand)
    
    
    #if subject didn't looked at hand/face or TAOI at all, then update the looking.pattern.sequence.list and skip the analysis for the current subject
    if ((time.point.first.target.hit == Inf) & time.point.first.hand.face.hit == Inf) 
    {
      looking.pattern.sequence.list[[i]] <- 
        cbind(looking.pattern.sequence.list[[i]], "No TAOI/Hand/Face")
      
      next
    }
    #in this while loop we iterate over the all time points 
    time.point.counter <- 
      1
    no.of.time.points <- 
      nrow(ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI)
    #extracting the looking pattern sequence for the current AOI
    while (time.point.counter < no.of.time.points) 
    {
      #the below time will be used for each value of looking  pattern sequence to compute the time duration 
      #Note: the "Corrected.Recording.timestamp" is the corrected time by substracting "my.time.offset", which is the offset time for the current subject, from the tobbi's original "Recording.timestamp"
      .start.time <- 
        ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter,"Corrected.Recording.timestamp"]
      
      #browser()
      #there are four mutually exclusive "if" corresponds to one of the four differenet possible situations: 1-TAOI 2-Face/Hand 3-Other 4- blinking.OR.not.at.the.display
      #====================================================================================================================================================================
      #if TAOI was hitted then increase the "no.of.gaze.shifts.vctr[i]" AND find the first time point that TAOI wasn't hitted
      if(ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, TAOI.Target.Col.Indx[i]]  ==  1) 
      {
        #print(time.point.counter)
        #print("TAOI")
        no.of.gaze.shifts.vctr[i] <- no.of.gaze.shifts.vctr[i] + 1
        looking.pattern.sequence.list[[i]] <- 
          cbind(looking.pattern.sequence.list[[i]], "TAOI")
        #while target is being looked at
        #Optimize flag (use min + which)
        while ( ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, TAOI.Target.Col.Indx[i]]  ==  1) 
        {
          time.point.counter <- time.point.counter + 1
          #check for the "out of range" index
          if (time.point.counter >= no.of.time.points)
          {
            break
          }
        }
        .end.time <- 
          ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter,"Corrected.Recording.timestamp"]
        .time.duration <- 
          .end.time - .start.time
        #Adding start, end and duration of the looking time for the current value of the looking.pattern.sequence
        #numbers in the pranthesis are the the corresponding Tobii's time stamps 
        #"looking.pattern.sequence.list[[i]]" is a vector of "looking area name" (TAOI/HANFace/Other/None/Blinking...) and their details (including: corrected starttime stamp (relative),  original start time stamp start.time, corrected end time stamp (relative),  original end time stamp start.time, time.duration)
        looking.pattern.sequence.list[[i]] <- 
          cbind(looking.pattern.sequence.list[[i]], 
                paste("corrected time stamp (relative) start.time",as.character(.start.time), "  original time stamp start.time", as.character((.start.time+my.time.offset)), sep = ";"),
                paste("corrected time stamp (relative) end.time",as.character(.end.time), "  original time stamp end.time", as.character((.end.time+my.time.offset)), sep = ";"),
                as.character(.time.duration))
        next
        
      } 
      #====================================================================================================================================================================
      #if hand/face was hitted then increase the "no.of.gaze.shifts.vctr[i]" AND find the first time point that hand/face wasn't hitted
      if (ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, 
                                                         TAOI.Final.Hand.Face.Logical.Col.Indx[i]] == TRUE) 
      {
        if (ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, 
                                                           TAOI.Face.Col.Indx[i]] == 1) 
        {
          #print(time.point.counter)
          #print("Face")
          
          looking.pattern.sequence.list[[i]] <- 
            cbind(looking.pattern.sequence.list[[i]], "Face")
          
        }else
        {
          looking.pattern.sequence.list[[i]] <- 
            cbind(looking.pattern.sequence.list[[i]], "Hand")
          #print(time.point.counter)
          #print("Hand")
          
        }
        no.of.gaze.shifts.vctr[i] <- no.of.gaze.shifts.vctr[i] + 1 
        while (ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, TAOI.Final.Hand.Face.Logical.Col.Indx[i]] == TRUE) 
        {
          time.point.counter <- time.point.counter + 1
          if (time.point.counter >= no.of.time.points)
          {
            break
          }
        }
        .end.time <- 
          ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter,"Corrected.Recording.timestamp"]
        .time.duration <- 
          .end.time - .start.time
        looking.pattern.sequence.list[[i]] <- 
          cbind(looking.pattern.sequence.list[[i]], 
                paste("corrected time stamp (relative) start.time",as.character(.start.time), "original time stamp start.time", as.character((.start.time+my.time.offset)), sep = ";"),
                paste("corrected time stamp (relative) end.time",as.character(.end.time), "original time stamp end.time", as.character((.end.time+my.time.offset)), sep = ";"),
                as.character(.time.duration))
        next
        
      }
      #====================================================================================================================================================================
      #if one of the other AOI was hitted then increase the "no.of.gaze.shifts.vctr[i]" AND find the first time point that "the other AOI" wasn't hitted
      
      
      if (ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, 
                                                         TAOI.Other.Logical.Col.Indx[i]] == TRUE) 
      {
        #print(time.point.counter)
        #print("Other")
        looking.pattern.sequence.list[[i]] <- cbind(looking.pattern.sequence.list[[i]], "Other")
        no.of.gaze.shifts.vctr[i] <- no.of.gaze.shifts.vctr[i] + 1 
        while (ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, 
                                                              TAOI.Other.Logical.Col.Indx[i]] == TRUE) 
        {
          time.point.counter <- time.point.counter + 1
          if (time.point.counter >= no.of.time.points)
          {
            break
          }
        }
        .end.time <- 
          ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter,"Corrected.Recording.timestamp"]
        .time.duration <- 
          .end.time - .start.time
        looking.pattern.sequence.list[[i]] <- 
          cbind(looking.pattern.sequence.list[[i]],
                paste("corrected time stamp (relative) start.time",as.character(.start.time), "original time stamp start.time", as.character((.start.time+my.time.offset)), sep = ";"),
                paste("corrected time stamp (relative) end.time",as.character(.end.time), "original time stamp end.time", as.character((.end.time+my.time.offset)), sep = ";"),
                as.character(.time.duration))
        next
      }
      #====================================================================================================================================================================
      #if one of the other AOI was hitted then increase the "no.of.gaze.shifts.vctr[i]" AND find the first time point that "the other AOI" wasn't hitted
      if (ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, 
                                                         blinking.OR.not.at.the.display.Logical.name] == TRUE) 
      {
        #print(time.point.counter)
        #print("blinking.OR.not.at.the.display")
        looking.pattern.sequence.list[[i]] <- 
          cbind(looking.pattern.sequence.list[[i]], "blinking.OR.not.looking.at.the.display")
        no.of.gaze.shifts.vctr[i] <- no.of.gaze.shifts.vctr[i] + 1 
        while (ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter, 
                                                              blinking.OR.not.at.the.display.Logical.name] == TRUE) 
        {
          time.point.counter <- time.point.counter + 1
          if (time.point.counter >= no.of.time.points)
          {
            break
          }
        }
        .end.time <- 
          ja.df.AOIs.Cols.Subset.Correspnd.2.CurrentTAOI[time.point.counter,"Corrected.Recording.timestamp"]
        .time.duration <- 
          .end.time - .start.time
        looking.pattern.sequence.list[[i]] <- 
          cbind(looking.pattern.sequence.list[[i]], 
                paste("corrected time stamp (relative) start.time",as.character(.start.time), "original time stamp start.time", as.character((.start.time+my.time.offset)), sep = ";"),
                paste("corrected time stamp (relative) end.time",as.character(.end.time), "original time stamp end.time", as.character((.end.time+my.time.offset)), sep = ";"),
                as.character(.time.duration))
        next
      }
      #====================================================================================================================================================================
      
      #just to avoid an infinitive loop if none of the TAOI and hand/face was looking at
      time.point.counter <- time.point.counter + 1
    }
  }
  
  #each element of the looking.pattern.sequence.list is a vector 
  #the sequence in each vector has the following pattern
  #"None", "A1", "start.time", "end.time", "duration", "A2", "start.time", "end.time", "duration", "A3", "start.time", "end.time", "duration"....
  #"Ai" is the ith area that has been looking at by the subject (it can be: "TAOI", "Face", "Hand", or "Other")
  return(looking.pattern.sequence.list)
}