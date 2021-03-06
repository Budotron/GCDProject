---
title: "GCD Project"
author: "Varun Boodram"
date: "October 8, 2014"
output: html_document
---

One of the most exciting areas in all of data science right now is wearable computing - see for example [this article](http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/) . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

Create one R script called run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


### 1. Merging the data

### Load required packages


```r
packages<-c("data.table", "dplyr", "reshape2")
sapply(packages, require, character.only = TRUE)
```

```
## data.table      dplyr   reshape2 
##       TRUE       TRUE       TRUE
```

### Obtain the data programatically:

Unless this step had been performed earlier, the function getdata() was used to crete a directory to store the downloaded file, and the download performed. The linked file was a .zip file, whose contents are paths to .txt files in which the data are recorded. The enclosing directory for these files were automatically downloaded to the working directory. 


```r
getdata<-function(fileUrl, dir, filename, ext){
        # create directory, if it is not already present
        dirName<-paste(dir, sep = "")
        if(!file.exists(dirName)){
                dir.create(path = dirName)
        }
        # Get the data, unless this step has already been done
        dest<-paste("./", dirName,"/", filename, ext, sep = "")
        if(!file.exists(dest)){
                download.file(url = fileUrl, destfile = dest, 
                              method = "curl") 
                datedownloaded<-date()
        }
        dest
}
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp<-getdata(fileUrl = fileURL, dir = "Fitness", 
              filename = "data", ext = ".zip")
alldata<-unzip(zipfile = temp)
alldata
```

```
##  [1] "./UCI HAR Dataset/activity_labels.txt"                         
##  [2] "./UCI HAR Dataset/features.txt"                                
##  [3] "./UCI HAR Dataset/features_info.txt"                           
##  [4] "./UCI HAR Dataset/README.txt"                                  
##  [5] "./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt"   
##  [6] "./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt"   
##  [7] "./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt"   
##  [8] "./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt"  
##  [9] "./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt"  
## [10] "./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt"  
## [11] "./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt"  
## [12] "./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt"  
## [13] "./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt"  
## [14] "./UCI HAR Dataset/test/subject_test.txt"                       
## [15] "./UCI HAR Dataset/test/X_test.txt"                             
## [16] "./UCI HAR Dataset/test/y_test.txt"                             
## [17] "./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt" 
## [18] "./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt" 
## [19] "./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt" 
## [20] "./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt"
## [21] "./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt"
## [22] "./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt"
## [23] "./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt"
## [24] "./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt"
## [25] "./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt"
## [26] "./UCI HAR Dataset/train/subject_train.txt"                     
## [27] "./UCI HAR Dataset/train/X_train.txt"                           
## [28] "./UCI HAR Dataset/train/y_train.txt"
```

### Select sets to read in for merging

The readMe was examined to determine which sets were pertinent for creating the tidy data set. Those dealing with Inertial Signals were deemed unnecessary for this project.


```r
library(data.table)
# grep() is used in the sequel to avoid the possibility of introducing a transcription error for alldata[i]

# read in the subject ids, and concatenate
subject_test<-fread(alldata[grep("subject_test", alldata)])
subject_train<-fread(alldata[grep("subject_train", alldata)])
subjectdata<-rbind(subject_train, subject_test)
colnames(subjectdata)<-"subjectID"
# read in the activity ids, and concatenate
activity_test<-fread(alldata[grep("(/)y_test", alldata)])
activity_train<-fread(alldata[grep("(/)y_train", alldata)])
activityids<-rbind(activity_train, activity_test)
colnames(activityids)<-"activityNum"
# read in the feature values, and concatenate
val_train<-read.table(alldata[grep("(/)X_train", alldata)])
val_test<-read.table(alldata[grep("(/)X_test", alldata)])
vals<-rbind(val_train, val_test)
# the values being measured in vals are the features. These replace the generic names
val_headers<-fread(alldata[grep("features.txt", alldata)])
colnames(val_headers)<-c("number", "feature")
# sanity check: confirm that the number of columns in vals is indeed equal to the number of rows in val_headers
c(dim(vals), dim(val_headers))
```

```
## [1] 10299   561   561     2
```

```r
colnames(vals)<-val_headers[, feature]
# concatenate the data tables into a single data table
mergeddata<-cbind(subjectdata, activityids, vals)
```

The subject ids of merged were not concurrent. This was fixed


```r
unique(mergeddata$subjectID)
```

```
##  [1]  1  3  5  6  7  8 11 14 15 16 17 19 21 22 23 25 26 27 28 29 30  2  4
## [24]  9 10 12 13 18 20 24
```

```r
mergeddata<-tbl_df(arrange(mergeddata, subjectID))
unique(mergeddata$subjectID)
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
## [24] 24 25 26 27 28 29 30
```

### 2. Extracting the required features


```r
# select from merged data the first two columns, and any others that contain the phrases "mean" or "std" in isolation
extracteddata<-select(mergeddata, 
                      subjectID, 
                      activityNum, 
                      contains("mean\\(\\)"), 
                      contains("std\\(\\)"))
list(Head=head(names(extracteddata), 6), 
     Tail=tail(names(extracteddata), 4))
```

```
## $Head
## [1] "subjectID"            "activityNum"          "tBodyAcc-mean()-X"   
## [4] "tBodyAcc-mean()-Y"    "tBodyAcc-mean()-Z"    "tGravityAcc-mean()-X"
## 
## $Tail
## [1] "fBodyAccMag-std()"          "fBodyBodyAccJerkMag-std()" 
## [3] "fBodyBodyGyroMag-std()"     "fBodyBodyGyroJerkMag-std()"
```

### 3. & 4. Naming the activities in the data set
The activity names are contained in activity.labels.txt. These were merged with extracteddata by the shared variable name, activity num. The resulting data frame is tidy


```r
# obtain the names of each activity for each label
activity_names<-fread(alldata[grep("activity.labels", alldata)])
setnames(activity_names, c("V1", "V2"), c("activityNum", "activity"))
# remove capitalization and underscores
activity_names$activity<-tolower(activity_names$activity)
activity_names$activity<-gsub(pattern = "_", 
                              replacement = "", 
                              x = activity_names$activity)
# merge the data set containing the activity names with the previously computated data set
cleandata<-merge(y = extracteddata, 
                 x = activity_names, 
                 by = "activityNum")
cleandata<-arrange(cleandata, subjectID)
```

### 5. Obtaining the average of each variable for each activity and each subject

cleandata was melted to long-format data with id varibales activity and subjectID. The long-formatted data was then recast to a wide format, for which the mean was caculated on each variable. 


```r
# melt the data frame computed in the last step into a long format data frame
meltdata<-melt(cleandata, id=c("activity", "subjectID"))
# cast the long format data to wide format, and call mean() on all variables but subjectID and activity
averaged_data<-dcast(data = meltdata, 
                     formula = subjectID+activity~variable, 
                     fun.aggregate = mean)
# the column names of averaged_data need to be changed to reflect the fact that the data they contain are averaged values
new_colNames<-paste("ave", colnames(averaged_data), sep=".")
setnames(x = averaged_data, 
         old = colnames(averaged_data[4:ncol(averaged_data)]), 
         new = new_colNames[4:length(new_colNames)])
write.table(x = averaged_data, 
            file = "./Fitness/averagedData.txt", 
            row.names = F)
```
