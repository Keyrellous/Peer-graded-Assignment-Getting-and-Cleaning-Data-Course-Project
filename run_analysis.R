#Merging  training and  test datasets to create a sigle data set
#Trainings tables
trainX<-read.table("/Users/keyrellousadib/Desktop/Independent training /Data Science JohnHopkins track /R programming/UCI HAR Dataset/train/X_train.txt")
trainY<-read.table("/Users/keyrellousadib/Desktop/Independent training /Data Science JohnHopkins track /R programming/UCI HAR Dataset/train/Y_train.txt")
subjectTrain<-read.table("/Users/keyrellousadib/Desktop/Independent training /Data Science JohnHopkins track /R programming/UCI HAR Dataset/train/subject_train.txt")
#Testing tables
testX<-read.table("/Users/keyrellousadib/Desktop/Independent training /Data Science JohnHopkins track /R programming/UCI HAR Dataset/test/X_test.txt")
testY<-read.table("/Users/keyrellousadib/Desktop/Independent training /Data Science JohnHopkins track /R programming/UCI HAR Dataset/test/Y_test.txt")
subjectTest<-read.table("/Users/keyrellousadib/Desktop/Independent training /Data Science JohnHopkins track /R programming/UCI HAR Dataset/test/subject_test.txt")
#Features 
Features<-read.table("features.txt")
#Activity labels
ActivityLabels<-read.table("activity_labels.txt")
#Column names
colnames(trainX) <- Features[,2]
colnames(trainY) <-"ActivityId"
colnames(subjectTrain) <- "SubjectId"
colnames(testX) <- Features[,2] 
colnames(testY) <- "ActivityId"
colnames(subjectTest) <- "SubjectId"
colnames(ActivityLabels) <- c('ActivityId','ActivityType')

#Merging all data in a single dataset
TrainMerge<-cbind(trainX,trainY,subjectTrain)
TestMerge<-cbind(testX,testY,subjectTest)
AllData<-rbind(TrainMerge,TestMerge)

#Subsetting the measurements on  mean and standard deviation
#Coloumn names 
ColNames <- colnames(AllData)
#Mean and Std labels 
MuSdLabels<- (grepl("ActivityId" , colNames) | grepl("SubjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))

#Subsetting AllData 
SubsetMuSd <- AllData[ , MuSdLabels == TRUE]

#Using descriptive activity names to name the activities in the data set
?merge
labelActivities<-merge(SubsetMuSd,ActivityLabels,by= "ActivityId", all.x = TRUE)

#Cleaning the abbreviations 
ActivityColumns<-colnames(labelActivities)

ActivityColumns <- gsub("^f", "frequencyDomain", ActivityColumns)
ActivityColumns <- gsub("^t", "timeDomain", ActivityColumns)
ActivityColumns <- gsub("Acc", "Accelerometer", ActivityColumns)
ActivityColumns <- gsub("Gyro", "Gyroscope", ActivityColumns)
ActivityColumns <- gsub("Mag", "Magnitude", ActivityColumns)
ActivityColumns <- gsub("Freq", "Frequency", ActivityColumns)
ActivityColumns <- gsub("mean", "Mean", ActivityColumns)
ActivityColumns <- gsub("std", "StandardDeviation", ActivityColumns)

colnames(labelActivities)<-ActivityColumns
#Creating a second, independent tidy data set with the average of each variable for each activity and each subject
#Making second tidy data set
library(dplyr)
labelActivities %>%
  group_by(SubjectId, ActivityType) %>%
  summarise_all(funs(mean))->FinalTidyData

#Writing second tidydata set
write.table(FinalTidyData, "FinalTidyData.txt", row.name=FALSE)

