## Download the raw data
features <- read.table("./UCI HAR Dataset/features.txt")
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

## Set the names of x_test and x_train as names of features
names(x_test) <- features[,2]
names(x_train) <- features[,2]

## Merge the test set and the subjects
x_test$subject <- subject_test$V1

## Add the descriptive activity names to the test labels 
for (i in seq_len(nrow(y_test))) { 
    for (j in seq_len(nrow(activity))) { 
        if (y_test[i,"V1"] == activity[j,"V1"]) {
            y_test[i,"activity"]  <- activity[j,"V2"]
        }
    }
}
## Unite all test data
x_test$code_activity <- y_test$V1
x_test$activity <- y_test$activity

## Merge the training set and the subjects
x_train$subject <- subject_train$V1

## Add the descriptive activity names to the training labels
for (i in seq_len(nrow(y_train))) { 
    for (j in seq_len(nrow(activity))) { 
        if (y_train[i,"V1"] == activity[j,"V1"]) {
            y_train[i,"activity"]  <- activity[j,"V2"]
        }
    }
}
## Unite all training data 
x_train$code_activity <- y_train$V1
x_train$activity <- y_train$activity

## Merge the test data and the training data
x <- rbind(x_test, x_train)

## Single out the features concerning only the mean and standard deviation
features_short <- features[grepl("mean\\()|std\\()", features$V2),]

## Select the data according to the picked features
x_short <- cbind(x[features_short$V1], x[562:564])

## Split the data into groups by the subjects and the activities
s <- split(x_short, list(x_short$subject, x_short$activity))

## For each group calculate the average of each variable
q <- sapply(s, function(x) colMeans(x[, 1:66]))

## Arrange the data frame with the tidy data
z <- data.frame()
activity <- arrange(activity, activity$V2)
k <- 1L
for (i in 1:6) { 
    for (j in 1:30) { 
        z[k,"subject"] <- j
        z[k,"activity"] <- activity[i,"V2"]
        for (t in 1:66) {
            z[k,t+2] <- q[t,k]
        }
        
        k <- k +1
    }
}
names_z <- c("subject", "activity", row.names(q))
names(z) <- gsub("\\()","",names_z)

## Write down a txt file with the results of the calculation
write.table(z, "./UCI HAR Dataset/data_set.txt", row.names=FALSE)
