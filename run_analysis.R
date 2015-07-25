library(dplyr)

names <- read.table("UCI HAR Dataset/features.txt")$V2
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
num2label <- function(x) activity_labels$V2[activity_labels$V1 == x]

data_test <- tbl_df(read.table("UCI HAR Dataset/test/X_test.txt", col.names = names))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")$V1
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")$V1
data_test <- mutate(data_test,
                    Subject = subject_test,
                    Activity = activity_test)

data_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = names)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")$V1
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")$V1
data_train <- mutate(data_train,
                     Subject = subject_train,
                     Activity = activity_train)

activity <- tbl_df(merge(data_train, data_test, all = TRUE)) %>%
            select(Subject,
                   Activity,
                   tBodyAcc.mean...X:angle.Z.gravityMean.) %>%
            mutate(Activity = sapply(Activity, num2label)) %>%
            arrange(Subject) %>%
            select(Subject,
                   Activity,
                   contains("mean"),
                   contains("sd")) %>%
            group_by(Subject, Activity)

rm(names, activity_labels, num2label,
   subject_test,  activity_test,  data_test,
   subject_train, activity_train, data_train)

activitySummary <- summarise(activity,
                             mean(tBodyAcc.mean...X),
                             mean(tBodyAcc.mean...Y),
                             mean(tBodyAcc.mean...Z),
                             mean(tGravityAcc.mean...X),
                             mean(tGravityAcc.mean...Y),
                             mean(tGravityAcc.mean...Z),
                             mean(tBodyAccJerk.mean...X),
                             mean(tBodyAccJerk.mean...Y),
                             mean(tBodyAccJerk.mean...Z),
                             mean(tBodyGyro.mean...X),
                             mean(tBodyGyro.mean...Y),
                             mean(tBodyGyro.mean...Z),
                             mean(tBodyGyroJerk.mean...X),
                             mean(tBodyGyroJerk.mean...Y),
                             mean(tBodyGyroJerk.mean...Z),
                             mean(tBodyAccMag.mean..),
                             mean(tGravityAccMag.mean..),
                             mean(tBodyAccJerkMag.mean..),
                             mean(tBodyGyroMag.mean..),
                             mean(tBodyGyroJerkMag.mean..),
                             mean(fBodyAcc.mean...X),
                             mean(fBodyAcc.mean...Y),
                             mean(fBodyAcc.mean...Z),
                             mean(fBodyAcc.meanFreq...X),
                             mean(fBodyAcc.meanFreq...Y),
                             mean(fBodyAcc.meanFreq...Z),
                             mean(fBodyAccJerk.mean...X),
                             mean(fBodyAccJerk.mean...Y),
                             mean(fBodyAccJerk.mean...Z),
                             mean(fBodyAccJerk.meanFreq...X),
                             mean(fBodyAccJerk.meanFreq...Y),
                             mean(fBodyAccJerk.meanFreq...Z),
                             mean(fBodyGyro.mean...X),
                             mean(fBodyGyro.mean...Y),
                             mean(fBodyGyro.mean...Z),
                             mean(fBodyGyro.meanFreq...X),
                             mean(fBodyGyro.meanFreq...Y),
                             mean(fBodyGyro.meanFreq...Z),
                             mean(fBodyAccMag.mean..),
                             mean(fBodyAccMag.meanFreq..),
                             mean(fBodyBodyAccJerkMag.mean..),
                             mean(fBodyBodyAccJerkMag.meanFreq..),
                             mean(fBodyBodyGyroMag.mean..),
                             mean(fBodyBodyGyroMag.meanFreq..),
                             mean(fBodyBodyGyroJerkMag.mean..),
                             mean(fBodyBodyGyroJerkMag.meanFreq..),
                             mean(angle.tBodyAccMean.gravity.),
                             mean(angle.tBodyAccJerkMean..gravityMean.),
                             mean(angle.tBodyGyroMean.gravityMean.),
                             mean(angle.tBodyGyroJerkMean.gravityMean.),
                             mean(angle.X.gravityMean.),
                             mean(angle.Y.gravityMean.),
                             mean(angle.Z.gravityMean.))

activitySummary