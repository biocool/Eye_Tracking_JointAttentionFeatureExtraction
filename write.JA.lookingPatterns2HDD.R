
write.JA.lookingPatterns2HDD <- function(looking.pattern.sequence.list,
                                         filenames.full.path)
  
{
  #preparing the final df as to be written as a csv file========
  #preparing a df for final data in the new format 
  
  #I'm assuming that the maximum number of looking seq. length is 200, so nrow = 200
  #for each TAOI we need six different values looking.area;start.time.relative;start.time.original;end.time.relative;end.time.original;duration
  final.csv.features.df <- matrix(nrow = 200, ncol = No.of.TAOI*6)
  final.csv.features.df <- 
    as.data.frame(final.csv.features.df)
  #assiging appropriate colnames===== 
  common.col.names.4.all.TAOI.Target.vctr <- 
    c("looking.area",
      "start.time.relative", "start.time.original", 
      "end.time.relative", "end.time.original",
      "duration")
  col.names.4.all.TAOI.Target.vctr <- c()
  for (current.TAOI.Target.Name in TAOI.Target.ColName.Vctr) 
  {
    #adding the common.col.names.4.all.TAOI.Target.vctr (which are common columns for all TAOIs) to the end of the current.TAOI.Target.Name to make unique meqaningful names for those features
    col.names.4.current.TAOI.Target.vct <-
      paste(current.TAOI.Target.Name, common.col.names.4.all.TAOI.Target.vctr, sep = "")
    col.names.4.all.TAOI.Target.vctr <-
      c(col.names.4.all.TAOI.Target.vctr, col.names.4.current.TAOI.Target.vct)
  }
  
  #sanity check
  #length(col.names.4.all.TAOI.Target.vctr) == ncol(final.csv.features.df)#TRUE
  colnames(final.csv.features.df) <- 
    col.names.4.all.TAOI.Target.vctr
  
  #writing the data in the final.csv.df======
  
  for (i in c(1:No.of.TAOI)) 
  {
    #each element of the "looking.pattern.sequence.list[[i]]" is vector of looking pattern sequence for a TAOI.
    #the name of that TAOI is "TAOI.Target.ColName.Vctr[i]"
    #if "length(looking.pattern.sequence.list[[i]]) == 2" then the subject didn't look at the TAOI
    #else: 
    ## j = 2,6,10,...<==>seq(2,length(looking.pattern.sequence.list[[i]]),4)
    ## "looking.pattern.sequence.list[[i]][j]" is the area that was hitted (hand/face/taoi/other/blinking)
    ## "looking.pattern.sequence.list[[i]][j+1]" is in this format: "corrected time stamp (relative) start.time;17267;original time stamp start.time;31790"
    ## So, after spliting "looking.pattern.sequence.list[[i]][j+1]" using ";" as delim. the second and forth elements are relative and original start time, respectively.
    ## "looking.pattern.sequence.list[[i]][j+2]" is in this format: "corrected time stamp (relative) end.time;17280;original time stamp end.time;31803"
    ## So, after spliting "looking.pattern.sequence.list[[i]][j+2]" using ";" as delim. the second and forth  elements is relative and original end time, respectively.
    ##"looking.pattern.sequence.list[[i]][j+3]" is the duration time
    for (j in (seq(2,length(looking.pattern.sequence.list[[i]]),4))) 
    {
      row.indx <- 
        ceiling(j/4)#ceiling(j/4) shows the current row number for the ith TAOI 
      #col indices for corrent looking area are (i-1)*6 + 1 ...i*6
      first.col.indx <- (i-1)*6 + 1
      last.col.indx <- i*6 
      col.indx.vctr <- c(first.col.indx:last.col.indx)
      if (length(looking.pattern.sequence.list[[i]]) == 2)#the subject didn't look at the TAOI
      {
        final.csv.features.df[row.indx, col.indx.vctr[1]] <-
          "None"
        final.csv.features.df[row.indx, col.indx.vctr[2]] <-
          -1
        final.csv.features.df[row.indx, col.indx.vctr[3]] <-
          -1
        final.csv.features.df[row.indx, col.indx.vctr[4]] <-
          -1
        final.csv.features.df[row.indx, col.indx.vctr[5]] <-
          -1
        final.csv.features.df[row.indx, col.indx.vctr[6]] <-
          -1
      }else
      {
        start.time.details.vctr <- 
          strsplit(x = looking.pattern.sequence.list[[i]][j+1], split = ";", fixed = TRUE)
        end.time.details.vctr <- 
          strsplit(x = looking.pattern.sequence.list[[i]][j+2], split = ";", fixed = TRUE)
        
        final.csv.features.df[row.indx, col.indx.vctr[1]] <-
          looking.pattern.sequence.list[[i]][j]#Looking area
        final.csv.features.df[row.indx, col.indx.vctr[2]] <-
          as.numeric(start.time.details.vctr[[1]][2])
        final.csv.features.df[row.indx, col.indx.vctr[3]] <-
          as.numeric(start.time.details.vctr[[1]][4])
        final.csv.features.df[row.indx, col.indx.vctr[4]] <-
          as.numeric(end.time.details.vctr[[1]][2])
        final.csv.features.df[row.indx, col.indx.vctr[5]] <-
          as.numeric(end.time.details.vctr[[1]][4])
        final.csv.features.df[row.indx, col.indx.vctr[6]] <-
          as.numeric(looking.pattern.sequence.list[[i]][j+3])#duration
      }
    }
    
  }
  filenames.full.path <- 
    paste(filenames.full.path, ".final.csv.features.df.csv", sep = "")
  write.csv(final.csv.features.df,
            file = filenames.full.path)
  
  
}