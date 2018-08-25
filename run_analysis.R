#downloading the file
library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}

#Loading features
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

#Loading training dataset
trainingdata.x <- read.table('./UCI HAR Dataset/train/X_train.txt')
trainingdata.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
trainingdata.subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

trainingdata <-  data.frame(trainingdata.subject, trainingdata.activity, trainingdata.x)
names(trainingdata) <- c(c('subject', 'activity'), features)

#Loading test dataset
testdata.x <- read.table('./UCI HAR Dataset/test/X_test.txt')
testdata.activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
testdata.subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

testdata <-  data.frame(testdata.subject, testdata.activity, testdata.x)
names(testdata) <- c(c('subject', 'activity'), features)

#Merges the training and the test sets to create one data set.
mergeddata <- rbind(trainingdata, testdata)

#Extracts only the measurements on the mean and standard deviation for each measurement.
meanAndStdDev <- grep('mean|std', features)
data.sub <- mergeddata[,c(1,2,meanAndStdDev + 2)]

#Uses descriptive activity names to name the activities in the data set
activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity.labels <- as.character(activity.labels[,2])
data.sub$activity <- activity.labels[data.sub$activity]

#Appropriately labels the data set with descriptive variable names.
newname <- names(data.sub)
newname <- gsub("[(][)]", "", newname)
newname <- gsub("^t", "TimeDomain_", newname)
newname <- gsub("^f", "FrequencyDomain_", newname)
newname <- gsub("Acc", "Accelerometer", newname)
newname <- gsub("Gyro", "Gyroscope", newname)
newname <- gsub("Mag", "Magnitude", newname)
newname <- gsub("-mean-", "_Mean_", newname)
newname <- gsub("-std-", "_StandardDeviation_", newname)
newname <- gsub("-", "_", newname)
names(data.sub) <- newname

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean)
write.table(x = tidydata, file = "data_tidy.txt", row.names = FALSE)