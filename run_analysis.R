# 0. Load data into R
# Features set
data_features <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
names(data_features) <- c("ID", "Name")

# Train set
data_X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
data_y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
data_subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
data_train <- cbind(data_subject_train, data_y_train, data_X_train)

# Test set
data_X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
data_y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
data_subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
data_test <- cbind(data_subject_test, data_y_test, data_X_test)

# 1.Merges the training and the test sets to create one data set.
mergedData = rbind(data_train, data_test)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
names(mergedData) <- c("Subject", "Label", data_features$Name)
valid_columns <- c(TRUE, TRUE, grepl("mean\\(\\)", data_features$Name) | grepl("std\\(\\)", data_features$Name))
tidyData <- mergedData[, valid_columns]

# 3.Uses descriptive activity names to name the activities in the data set.
activities <- data.frame("Label"=1:6, "Activity"=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
tidyData2 = merge(activities, tidyData, all=TRUE)
tidyData3 = tidyData2[, 2:69]

# 4.Appropriately labels the data set with descriptive variable names.
names(tidyData3)
columnNames <- names(tidyData3)
columnNames <- gsub("^f","Freq", columnNames)
columnNames <- gsub("^t","Time", columnNames)
columnNames <- gsub("-mean\\(\\)","Mean", columnNames)
columnNames <- gsub("-std\\(\\)","Std", columnNames)
columnNames <- gsub("-",".", columnNames)
names(tidyData3) <- columnNames

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData4 <- melt(tidyData3, id=c("Subject", "Activity"), measure.vars=3:68)
tidyData5 <- ddply(tidyData4, .(Subject, Activity, variable), summarize, mean=mean(value))
names(tidyData5) <- c("Subject", "Activity", "Variable", "Mean")
write.table(tidyData5, "result.txt", row.name=FALSE)