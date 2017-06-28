
# Clean up workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.
#set working directory to the location where the UCI HAR Dataset was unzipped

# Read in the data from files
activityLabels <- read.table("./activity_labels.txt",stringsAsFactors = F)
features <- read.table("./features.txt")

subjectTrain <- read.table("train/subject_train.txt")
xTrain <- read.table("train/X_train.txt")
yTrain <- read.table("train/y_train.txt")

subjectTest <- read.table("test/subject_test.txt")
xTest <- read.table("test/X_test.txt")
yTest <- read.table("test/y_test.txt")

# Assigin column names to the data imported above
colnames(xTrain) = features[,2]
colnames(xTest) = features[,2]
colnames(activityLabels) = c("activityCode", "activityName")
colnames(subjectTrain) = "subjectNumber"
colnames(subjectTest) = "subjectNumber"
colnames(yTrain) = "activityCode"
colnames(yTest) = "activityCode"

# 3. Use descriptive activity names to name the activities in the data set
yTrain$activityName <- activityLabels$activityName[yTrain$activityCode]
yTest$activityName <- activityLabels$activityName[yTest$activityCode]

finalTrain <- cbind(subjectTrain,activityName = yTrain$activityName,xTrain)
finalTest <- cbind(subjectTest,activityName = yTest$activityName,xTest)





# Combine training and test data to create a final data set
finalData <- rbind(finalTrain,finalTest)




# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

feat <- features[,2]
colNumbers <- c(1,2,grep("mean[^F].*[)]$",feat), grep("std.*[)]$",feat) )

finalDataSel <- finalData[,colNumbers]

# 4. Appropriately label the data set with descriptive activity names. 
# Cleaning up the variable names
colNames <- names(finalDataSel)


for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("BodyBody","Body",colNames[i])
  colNames[i] = gsub("Mag","Magnitude",colNames[i])
  
}
# Reassigning the new descriptive column names to the finalData set
colnames(finalDataSel) <- colNames

ennd <- finalDataSel

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData <- aggregate(ennd[,-c(1,2)], by=list(activityId=ennd$activityName,subjectNumber = ennd$subjectNumber),mean)


write.table(tidyData, './tidyData.txt',row.name=FALSE,sep='\t')
