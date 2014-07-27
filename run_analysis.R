#setting working directory 

setwd("E:/2.TO LEARN/COURSERA/3. Getting and Cleaning Data/Project")

# dowloading the UCI HAR Dataset from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = zip.file, method = 'curl')

#Merging the training and the test sets in order to create one data set

data1 <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
data2 <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
head(data1)
head(data2)
X <- rbind(data1, data2)
head(X)

data1  <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
data2 <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
Subject <- rbind(data1, data2)

data1  <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
data2 <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
Y <- rbind(data1, data2)

# Extracting  the mean and standard deviation for each measurement

features <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
measurement <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, measurement]
names(X) <- features[measurement, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X)) 

# Using descriptive activity names to name the activities in the data set

activities <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

#labelling the data set with descriptive activity names

names(S) <- "subject"
tidy1 <- cbind(Subject, Y, X)
write.table(tidy1, "tidy_dataset.txt")

# Creating second independent tidy data set with the average of each variable for each activity and each subject

uniqueSubjects = unique(Subject)[,1]
numSubjects = length(unique(Subject)[,1])
numActivities = length(activities[,1])
numCols = dim(tidy1)[2]
tidy2 = tidy1[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    tidy2[row, 1] = uniqueSubjects[s]
    tidy2[row, 2] = activities[a, 2]
    tmp <- tidy1[tidy1$subject==s & tidy1$activity==activities[a, 2], ]
    tidy2[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(tidy2, "independent_tidy_dataset .txt")
