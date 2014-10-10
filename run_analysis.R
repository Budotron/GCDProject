packages<-c("data.table", "dplyr", "reshape2")
sapply(packages, require, character.only = TRUE)
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
colnames(vals)<-val_headers[, feature]
# concatenate the data tables into a single data table
mergeddata<-cbind(subjectdata, activityids, vals)
mergeddata<-tbl_df(arrange(mergeddata, subjectID))
# select from merged data the first two columns, and any others that contain the phrases "mean" or "std" in isolation
extracteddata<-select(mergeddata, 
                      subjectID, 
                      activityNum, 
                      contains("mean\\(\\)"), 
                      contains("std\\(\\)"))
list(Head=head(names(extracteddata), 6), 
     Tail=tail(names(extracteddata), 4))
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