---
title: "Joint Attention Feature Extraction"
output: html_notebook
---
# Steps:

Preprocessing

1- Run the functions' definition

2- Run "Reading the TAOI Details from a csv file" chunk

3- Set the path in "Getting all file names and setting the paths" chunk and run it 


PostProcessing
5- Run "Getting all file names" chunk. This chunk read the csv files which were generated in the Preprocessing step. 

6- Run "Reading files and applying main functions, for the first feature extraction step, on them" chunk. This chunk produce the features in csv file for each subject.

7- Run the "Running circle count driver function and calculating the run time" chunk



Note: 
the first 17 character of the file name was considered as the subject ID. It includes the subject ID (the first five character and ET date)
"SubjectID <- 
    str_sub(filenames.list[[loop.index]],1,17)"
## libraries
```{r libs}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)
library(stringr)

```


# Function Definition


##Preprocessing functoin and feature extraction


##Writing fuction


















# Preprocessing

## Loading TAOIs' information


There are 11 TAOIs. However four of theme seem chalenging. 
So, I'm going to work on the rest and then I'll get back to those. 

```{r Reading the TAOI Details from a csv file }

#the below file contains the TAOIs and start and end time stamp in mil. sec. 
load(file = "./Data/TAOI.Detail.4.Input")
#write.csv(TAOI.Detail.4.Input, file = "./Data/TAOI.Detail.4.Input.csv")
#renaming the cols names=======
#in the raw inptu tsv file, col names contain "[", " ", "]", "-" whoich R converts the autamtically to "."
#So, I'm going to do the same replacement to have the sam TAOI names
TAOI.Detail.4.Input$Target.AOI.Names <- 
  gsub(pattern = "[", replacement = ".", x = TAOI.Detail.4.Input$Target.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Target.AOI.Names <- 
  gsub(pattern = " ", replacement = ".", x = TAOI.Detail.4.Input$Target.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Target.AOI.Names <- 
  gsub(pattern = "]", replacement = ".", x = TAOI.Detail.4.Input$Target.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Target.AOI.Names <- 
  gsub(pattern = "-", replacement = ".", x = TAOI.Detail.4.Input$Target.AOI.Names, fixed = TRUE)

#same for Hand.AOI.Names
TAOI.Detail.4.Input$Hand.AOI.Names <- 
  gsub(pattern = "[", replacement = ".", x = TAOI.Detail.4.Input$Hand.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Hand.AOI.Names <- 
  gsub(pattern = " ", replacement = ".", x = TAOI.Detail.4.Input$Hand.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Hand.AOI.Names <- 
  gsub(pattern = "]", replacement = ".", x = TAOI.Detail.4.Input$Hand.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Hand.AOI.Names <- 
  gsub(pattern = "-", replacement = ".", x = TAOI.Detail.4.Input$Hand.AOI.Names, fixed = TRUE)

#same for Face.AOI.Names
TAOI.Detail.4.Input$Face.AOI.Names <- 
  gsub(pattern = "[", replacement = ".", x = TAOI.Detail.4.Input$Face.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Face.AOI.Names <- 
  gsub(pattern = " ", replacement = ".", x = TAOI.Detail.4.Input$Face.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Face.AOI.Names <- 
  gsub(pattern = "]", replacement = ".", x = TAOI.Detail.4.Input$Face.AOI.Names, fixed = TRUE)
TAOI.Detail.4.Input$Face.AOI.Names <- 
  gsub(pattern = "-", replacement = ".", x = TAOI.Detail.4.Input$Face.AOI.Names, fixed = TRUE)

#final name vectors=====
TAOI.Target.ColName.Vctr <- TAOI.Detail.4.Input$Target.AOI.Names
TAOI.Hand.ColName.Vctr <- TAOI.Detail.4.Input$Hand.AOI.Names
TAOI.Face.ColName.Vctr <- TAOI.Detail.4.Input$Face.AOI.Names


#seting No.of.TAOI
No.of.TAOI <- 
  length(TAOI.Target.ColName.Vctr)
```

##Reading all files and looping over them by calling the Main fuction

```{r Getting all file names and setting the paths}

rootFolder <- 
  "./Data/WholeData/BA Joint Attention Raw Data Exports for Javad/"
#listing all tsv files in the path (these are raw eye-tracking exports)
##the first 5 character of each element in the below lis show the subject ID
filenames.list <- list.files(rootFolder, pattern="*.tsv", recursive = TRUE, full.names = FALSE)
#file name with full path
filenames.full.path.list <- list.files(rootFolder, pattern="*.tsv", full.names = TRUE, recursive = TRUE)

CricleCounts.folder.path <- 
  "./Data/WholeData/CircleCount/"

```


looping over them by calling the Main fuction
```{r Reading files and applying main functions, warning=FALSE}
start.time <- Sys.time()

#just for printing an appropriate message
loop.index <- 0
no.of.JA.files <-
  length(filenames.full.path.list)
for (filenames.full.path in filenames.full.path.list) 
{
  loop.index <- 
    loop.index + 1
  print(">>>><<<< >>>><<<< >>>><<<< >>>><<<< >>>><<<< >>>><<<<")
  print(paste(loop.index, " from ", no.of.JA.files))
  print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
  current.JA.df <- 
    read.csv(file = filenames.full.path, sep = "\t")
  #In some of the exports like project 15 instead of "Recording.timestamp" we have "Recording.timestamp..ms.". So, I'm goign to replace this colnames to have a consistent colnames. 
  if ("Recording.timestamp..ms."%in%(colnames(current.JA.df))) 
  {
    indx.of.timeStampCol <- 
      which(colnames(current.JA.df)=="Recording.timestamp..ms.")
    colnames(current.JA.df)[indx.of.timeStampCol] <- "Recording.timestamp"
      
  }
  skip_to_next <- FALSE
  tryCatch(
  expr = {
    looking.pattern.sequence.list.4.current.JA.df <- 
    ja.Preprocessing.and.FeatureExtraction(current.JA.df)
  }, 
  error = function(e)
  { print(filenames.full.path)
    skip_to_next <- TRUE
  }
  
  )
  if (skip_to_next) 
  {
    next()
    
  }
  write.JA.lookingPatterns2HDD(looking.pattern.sequence.list = looking.pattern.sequence.list.4.current.JA.df, filenames.full.path = 
                               filenames.full.path)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
print("Preprocessing time.taken")
print(time.taken)
```


#POST PROCESSING (CRICLE COUNT) 

##Reading the data

```{r Getting all file names.}
#"check.names=FALSE" allows to have % in the colnames

#listing all csv files in the path
##the first 5 character of each element in the below lis show the subject ID
filenames.list <- list.files(rootFolder, pattern="*.csv", recursive = TRUE)

filenames.full.path.list <- list.files(rootFolder, pattern="*.csv", full.names = TRUE, recursive = TRUE)
```

```{r Running circle count driver function and calculating the run time}
start.time <- Sys.time()
crircle.count.driver.fun()
end.time <- Sys.time()
time.taken <- end.time - start.time
print("Postprocessing time.taken")
print(time.taken)

```
 



#Notes
1- only "Blinking" and "Other" are merged with the prev. looking area if the duration is <100 m.s.
2- if "Blinking" and "Other" are the first area then those are not merging (because there is no prev. area)



