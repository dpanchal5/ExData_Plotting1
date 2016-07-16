# Getting and cleaning data class Project
# Filename: run_analysis.R
# Author: Divya Panchal
#
#You should create one R script called run_analysis.R that does the following.
#
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
library(plyr)
#Set the path
setwd("D:/DivyaDataScience/GettingandCleaningData/Project")
path <- getwd()

#Get the required data and files

URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

projectfile <- "Dataset.zip"
if (!file.exists(path)) 
  {
  dir.create(path)
  }
download.file(URL, file.path(path, projectfile))

#Unzip the file
unzip(zipfile="Dataset.zip",exdir="./data")

#Unzipped files are in the folder "UCI HAR Dataset". Retrieve list of the files
retrievefile <- file.path("./data" , "UCI HAR Dataset")
allfiles<-list.files(retrievefile, recursive=TRUE)

#For this project, the files in the Inertial Signals folders will not used.
#The files that will be used are listed as follows:

#test/subject_test.txt
#test/X_test.txt
#test/y_test.txt
#train/subject_train.txt
#train/X_train.txt
#train/y_train.txt

#Reading the data from the text files into respective variables
#Since there are 6 files, reading the files into 6 separate variables

#Read the files for activity
Y_test  <- read.table(file.path(retrievefile, "test" , "Y_test.txt" ),header = FALSE)
Y_train <- read.table(file.path(retrievefile, "train", "Y_train.txt"),header = FALSE)

#Read the files for subject
subject_train <- read.table(file.path(retrievefile, "train", "subject_train.txt"),header = FALSE)
subject_test  <- read.table(file.path(retrievefile, "test" , "subject_test.txt"),header = FALSE)

#Read the files for data
X_test  <- read.table(file.path(retrievefile, "test" , "X_test.txt" ),header = FALSE)
X_train <- read.table(file.path(retrievefile, "train", "X_train.txt"),header = FALSE)

#Load Features and Labels
features <- read.table(file.path(retrievefile, "features.txt"),colClasses=c("character"))
activity_labels <- read.table(file.path(retrievefile, "activity_labels.txt"),col.names = c("ActivityId", "Activity"))

#Merge the training and the test sets to create one data set.
traindata<-cbind(cbind(X_train, subject_train), Y_train)

#Merge test data
testdata<-cbind(cbind(X_test, subject_test), Y_test)

#Merge train and test data
Data<-rbind(traindata, testdata)

#Assign labels to the columns
colLabels<-rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(Data)<-colLabels


#Create a CSV file
write.csv(Data, file = "tidydata.csv",row.name=TRUE)

#Extract the mean and standard deviation
Data <- Data[,grepl("mean|std|Subject|ActivityId", names(Data))]

#Use descriptive activity names to name the activities in the data set

Data <- join(Data, activity_labels, by = "ActivityId", match = "first")
Data <- Data[,-1]

#Appropriately label the data set
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))
names(Data)<-gsub("std", "StandardDeviation", names(Data))
names(Data)<-gsub("Freq", "Frequency", names(Data))

#Generate a second tidy data set
Data_avg = ddply(Data, c("Subject","Activity"), numcolwise(mean))
write.table(Data_avg, file = "Data_avg.txt", row.name=FALSE)