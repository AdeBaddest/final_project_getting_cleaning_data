library(dplyr) #loading dplyr package

#downloading the dataset
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename<-"Getting_and_Cleaning_data.zip"
download.file(fileURL, filename)

#check if file exists. if yes, unzip the file to the same folder
unzip(filename)

#file paths variables
features_file="UCI HAR Dataset/features.txt"
activities_file="UCI HAR Dataset/activity_labels.txt"
subject_test_file="UCI HAR Dataset/test/subject_test.txt"
x_test_file="UCI HAR Dataset/test/X_test.txt"
y_test_file="UCI HAR Dataset/test/y_test.txt"
subject_train_file="UCI HAR Dataset/train/subject_train.txt"
x_train_file="UCI HAR Dataset/train/X_train.txt"
y_train_file="UCI HAR Dataset/train/y_train.txt"


#assigning dataframes
features<-read.table(features_file, col.names = c("n","functions"))
activites<-read.table(activities_file, col.names = c("code","activity"))

subject_test<-read.table(subject_test_file, col.names = "subject")
x_test<-read.table(x_test_file, col.names = features$functions)
y_test<-read.table(y_test_file, col.names = "code")


subject_train<-read.table(subject_train_file, col.names = "subject")
x_train<-read.table(x_train_file, col.names = features$functions)
y_train<-read.table(y_train_file, col.names = "code")

#merge the training and test sets to create one data set
New_X<-rbind(x_train, x_test)
New_Y<-rbind(y_train, y_test)
New_subject<-rbind(subject_train, subject_test)
New_DATASET<-cbind(New_subject, New_Y, New_X)


#Extract only the measurements on the mean and standard deviation for each measurement
TidyData<-New_DATASET %>% select(subject, code, contains("mean"), contains("std"))


#Use descriptive activity names to name the activities in the data set.
TidyData$code <- activites[TidyData$code, 2]

#Appropriately label the data set with descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))


#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

