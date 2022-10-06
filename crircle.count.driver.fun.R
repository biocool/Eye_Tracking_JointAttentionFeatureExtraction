
#NOTE: "filenames.full.path.list" is a global variable
crircle.count.driver.fun <- function()
{
  
  No.of.TAOI <- 7
  #total number of subjects (which is the total number of files in the folder)
  no.of.JA.files <-
    length(filenames.full.path.list)
  
  #The below df contains six features (looking.area,	 start.time.relative,	 start.time.original	 end.time.relative,	 end.time.original, and	 duration) for each TAOI for all subjects (after merging "other" and "blinking" with <=100ms duration).
  #The first column would be the subject ID and the rest are correspond to the features. So the total number of columns would be "6*No.of.TAOI + 1"
  
  #each subject would have 200 rows, so I'm gonna do rbind to add data for each subject to this final df.
  final.features.merged.df.4.all.subjects <-
    as.data.frame(matrix(nrow = 1, 
                         ncol = 6*No.of.TAOI + 1))
  
  colnames(final.features.merged.df.4.all.subjects)<- 
    colnames(final.csv.features.merged.4.this.sbj)
  
  
  #The below df contains the number of full and partial JA circles for all subjects.
  #the first column would be the subject ID and the rest are correspond to number of full/partial JA circles. So the total number of columns would be "2*No.of.TAOI + 1"
  ja.full.partial.cricle.count.df.4.all.subjects <-
    as.data.frame(matrix(nrow = no.of.JA.files, 
                         ncol = 2*No.of.TAOI + 1))
  
  loop.index <- 0
  #browser() 
  for (filenames.full.path in filenames.full.path.list) 
  {
    #browser() 
    loop.index <- 
      loop.index + 1
    
    #extracting the subject ID
    ##removing the path and using only the file name
    file_name_splited_vctr = strsplit(filenames.list[[loop.index]],"/")
    file_name_splited_last_element = file_name_splited_vctr[[1]][length(file_name_splited_vctr[[1]])]
    #browser()
    SubjectID <- 
      str_sub(file_name_splited_last_element,1,17)
    #print(">>>><<<< >>>><<<< >>>><<<< >>>><<<< >>>><<<< >>>><<<<")
    #print(paste(loop.index, " from ", no.of.JA.files))
    #print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
    final.csv.features.4.this.sbj <- 
      read.csv(file = filenames.full.path, row.names = 1)
    final.csv.features.merged.4.this.sbj <- 
      ja.merge.Other.and.Blinking.less.than.100.ms.(final.csv.features = final.csv.features.4.this.sbj, subjectID = SubjectID)
    
    final.features.merged.df.4.all.subjects <-
      rbind(final.features.merged.df.4.all.subjects , 
            final.csv.features.merged.4.this.sbj)
    #in calling "ja.full.partial.cricle.count" we should drop the first column
    #beacuse it's subject ID and in "ja.full.partial.cricle.count" function the first column is the looking area for the first TAOI
    ja.full.partial.cricle.count.df.4.current.sbj <- 
      ja.full.partial.cricle.count(final.csv.features.merged = final.csv.features.merged.4.this.sbj[,-1], 
                                   No.of.TAOI = No.of.TAOI)
    #assigning subject ID for this subject
    ja.full.partial.cricle.count.df.4.current.sbj$SubjectID <- 
      SubjectID
    #adding to the final df 
    ja.full.partial.cricle.count.df.4.all.subjects[loop.index,] <-
      ja.full.partial.cricle.count.df.4.current.sbj
  }
  
  #assigning the appropirate col names to the two final dfs
  colnames(final.features.merged.df.4.all.subjects) <- 
    colnames(final.csv.features.merged.4.this.sbj)
  colnames(ja.full.partial.cricle.count.df.4.all.subjects) <- 
    (colnames(ja.full.partial.cricle.count.df.4.current.sbj))
  #removing the first NA row
  final.features.merged.df.4.all.subjects <-
    final.features.merged.df.4.all.subjects[-1,]
  
  
  
  #writing dfs to HDD=====
  
  file.name <- paste(CricleCounts.folder.path, "final.features.merged.df.4.all.subjects.csv", sep = "")
  write.csv(final.features.merged.df.4.all.subjects, file = file.name)
  
  
  file.name <- paste(CricleCounts.folder.path, "ja.full.partial.cricle.count.df.4.all.subjects.csv", sep = "")
  write.csv(ja.full.partial.cricle.count.df.4.all.subjects, file = file.name)
  
  
  
}
