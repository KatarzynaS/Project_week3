The data set UCI HAR were downloaded from
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and unzipped.

The original data description can be examined from 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.

Action take to create the tidy date set can be summarized as fallow:
1.	Merging the training and the test sets in order to create one data set
-	Firstly by x train and test, secondly y train and test thirdly subject train and test 
2.	Extracting  the mean and standard deviation for each measurement
-	based on features.txt 
3.	Using descriptive activity names to name the activities in the data set
-	From activity_labels.txt
4.	Labelling the data set with descriptive activity names
5.	Creating second independent tidy data set with the average of each variable for each activity and each subject.

The variables in the data set:
-	Subject : the participant ID (1 to 30)
-	Activity: the name of activity performed when the measurements were taken
walking 
walkingdownstairs
walkingupstairs
sitting
standing
laying

-	 Variables wit statistics of corresponding Measures taken 

"tbodyacc-mean-x" "tbodyacc-mean-y" "tbodyacc-mean-z" "tbodyacc-std-x" "tbodyacc-std-y" "tbodyacc-std-z"
"tgravityacc-mean-x" "tgravityacc-mean-y" "tgravityacc-mean-z" 
"tgravityacc-std-x" "tgravityacc-std-y" "tgravityacc-std-z" "tbodyaccjerk-mean-x" "tbodyaccjerk-mean-y"
"tbodyaccjerk-mean-z" "tbodyaccjerk-std-x" "tbodyaccjerk-std-y" "tbodyaccjerk-std-z" "tbodygyro-mean-x" 
"tbodygyro-mean-y" "tbodygyro-mean-z" "tbodygyro-std-x" "tbodygyro-std-y" "tbodygyro-std-z" "tbodygyrojerk-mean-x" 
"tbodygyrojerk-mean-y" "tbodygyrojerk-mean-z" "tbodygyrojerk-std-x" "tbodygyrojerk-std-y" "tbodygyrojerk-std-z" 
"tbodyaccmag-mean" "tbodyaccmag-std" "tgravityaccmag-mean" "tgravityaccmag-std" "tbodyaccjerkmag-mean"
"tbodyaccjerkmag-std" "tbodygyromag-mean" "tbodygyromag-std" "tbodygyrojerkmag-mean" "tbodygyrojerkmag-std" 
"fbodyacc-mean-x" "fbodyacc-mean-y" "fbodyacc-mean-z" "fbodyacc-std-x" "fbodyacc-std-y" "fbodyacc-std-z"
"fbodyaccjerk-mean-x" "fbodyaccjerk-mean-y" "fbodyaccjerk-mean-z" "fbodyaccjerk-std-x" "fbodyaccjerk-std-y"
"fbodyaccjerk-std-z" "fbodygyro-mean-x" "fbodygyro-mean-y" "fbodygyro-mean-z" "fbodygyro-std-x" "fbodygyro-std-y"
"fbodygyro-std-z" "fbodyaccmag-mean" "fbodyaccmag-std" "fbodybodyaccjerkmag-mean" "fbodybodyaccjerkmag-std" 
"fbodybodygyromag-mean" "fbodybodygyromag-std" "fbodybodygyrojerkmag-mean" "fbodybodygyrojerkmag-std"
