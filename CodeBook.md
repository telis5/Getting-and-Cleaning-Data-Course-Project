---
title: "CodeBook"
author: "telis5"
date: "18.04.2015"
---

## Project Description
Getting and Cleaning Data Course Project

##Study design and data processing

###Collection of the raw data
* The raw dataset includes the following files:      
    + features.txt: List of all features.
    + activity_labels.txt: Links the class labels with their activity name.
    + train/X_train.txt: Training set.
    + train/y_train.txt: Training labels.
    + train/subject_train.txt: Each row identifies the subject who performed the activity for each window sample. 
    + test/X_test.txt: Test set.
    + test/y_test.txt: Test labels.
    + test/subject_test.txt: Each row identifies the subject who performed the activity for each window sample.    
* For each record it is provided:   
    + Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
    + Triaxial Angular velocity from the gyroscope. 
    + A 561-feature vector with time and frequency domain variables. 
    + Its activity label. 
    + An identifier of the subject who carried out the experiment.

##Description of the variables in the data_set.txt file
* General description of the file including:
    + Dimensions of the dataset: 180 obs. of  68 variables
    + Variables present in the dataset:           
 $ subject                  : int  1 2 3 4 5 6 7 8 9 10 ...   
 $ activity                 : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...    
 $ tBodyAcc.mean.X          : num  0.222 0.281 0.276 0.264 0.278 ...   
 $ tBodyAcc.mean.Y          : num  -0.0405 -0.0182 -0.019 -0.015 -0.0183 ...  
 $ tBodyAcc.mean.Z          : num  -0.113 -0.107 -0.101 -0.111 -0.108 ...  
 $ tBodyAcc.std.X           : num  -0.928 -0.974 -0.983 -0.954 -0.966 ...   
 $ tBodyAcc.std.Y           : num  -0.837 -0.98 -0.962 -0.942 -0.969 ...   
 $ tBodyAcc.std.Z           : num  -0.826 -0.984 -0.964 -0.963 -0.969 ...   
 $ tGravityAcc.mean.X       : num  -0.249 -0.51 -0.242 -0.421 -0.483 ...  
 $ tGravityAcc.mean.Y       : num  0.706 0.753 0.837 0.915 0.955 ...  
 $ tGravityAcc.mean.Z       : num  0.446 0.647 0.489 0.342 0.264 ...   
 $ tGravityAcc.std.X        : num  -0.897 -0.959 -0.983 -0.921 -0.946 ...   
 $ tGravityAcc.std.Y        : num  -0.908 -0.988 -0.981 -0.97 -0.986 ...  
 $ tGravityAcc.std.Z        : num  -0.852 -0.984 -0.965 -0.976 -0.977 ...   
 $ tBodyAccJerk.mean.X      : num  0.0811 0.0826 0.077 0.0934 0.0848 ...  
 $ tBodyAccJerk.mean.Y      : num  0.00384 0.01225 0.0138 0.00693 0.00747 ...   
 $ tBodyAccJerk.mean.Z      : num  0.01083 -0.0018 -0.00436 -0.00641 -0.00304 ...  
 $ tBodyAccJerk.std.X       : num  -0.958 -0.986 -0.981 -0.978 -0.983 ...  
 $ tBodyAccJerk.std.Y       : num  -0.924 -0.983 -0.969 -0.942 -0.965 ...  
 $ tBodyAccJerk.std.Z       : num  -0.955 -0.988 -0.982 -0.979 -0.985 ...   
 $ tBodyGyro.mean.X         : num  -0.01655 -0.01848 -0.02082 -0.00923 -0.02189 ...   
 $ tBodyGyro.mean.Y         : num  -0.0645 -0.1118 -0.0719 -0.093 -0.0799 ...  
 $ tBodyGyro.mean.Z         : num  0.149 0.145 0.138 0.17 0.16 ...   
 $ tBodyGyro.std.X          : num  -0.874 -0.988 -0.975 -0.973 -0.979 ...   
 $ tBodyGyro.std.Y          : num  -0.951 -0.982 -0.977 -0.961 -0.977 ...   
 $ tBodyGyro.std.Z          : num  -0.908 -0.96 -0.964 -0.962 -0.961 ...   
 $ tBodyGyroJerk.mean.X     : num  -0.107 -0.102 -0.1 -0.105 -0.102 ...   
 $ tBodyGyroJerk.mean.Y     : num  -0.0415 -0.0359 -0.039 -0.0381 -0.0404 ...   
 $ tBodyGyroJerk.mean.Z     : num  -0.0741 -0.0702 -0.0687 -0.0712 -0.0708 ...   
 $ tBodyGyroJerk.std.X      : num  -0.919 -0.993 -0.98 -0.975 -0.983 ...   
 $ tBodyGyroJerk.std.Y      : num  -0.968 -0.99 -0.987 -0.987 -0.984 ...   
 $ tBodyGyroJerk.std.Z      : num  -0.958 -0.988 -0.983 -0.984 -0.99 ...  
 $ tBodyAccMag.mean         : num  -0.842 -0.977 -0.973 -0.955 -0.967 ...   
 $ tBodyAccMag.std          : num  -0.795 -0.973 -0.964 -0.931 -0.959 ...   
 $ tGravityAccMag.mean      : num  -0.842 -0.977 -0.973 -0.955 -0.967 ...   
 $ tGravityAccMag.std       : num  -0.795 -0.973 -0.964 -0.931 -0.959 ...   
 $ tBodyAccJerkMag.mean     : num  -0.954 -0.988 -0.979 -0.97 -0.98 ...   
 $ tBodyAccJerkMag.std      : num  -0.928 -0.986 -0.976 -0.961 -0.977 ...   
 $ tBodyGyroMag.mean        : num  -0.875 -0.95 -0.952 -0.93 -0.947 ...   
 $ tBodyGyroMag.std         : num  -0.819 -0.961 -0.954 -0.947 -0.958 ...   
 $ tBodyGyroJerkMag.mean    : num  -0.963 -0.992 -0.987 -0.985 -0.986 ...   
 $ tBodyGyroJerkMag.std     : num  -0.936 -0.99 -0.983 -0.983 -0.984 ...   
 $ fBodyAcc.mean.X          : num  -0.939 -0.977 -0.981 -0.959 -0.969 ...    
 $ fBodyAcc.mean.Y          : num  -0.867 -0.98 -0.961 -0.939 -0.965 ...   
 $ fBodyAcc.mean.Z          : num  -0.883 -0.984 -0.968 -0.968 -0.977 ...   
 $ fBodyAcc.std.X           : num  -0.924 -0.973 -0.984 -0.952 -0.965 ...   
 $ fBodyAcc.std.Y           : num  -0.834 -0.981 -0.964 -0.946 -0.973 ...   
 $ fBodyAcc.std.Z           : num  -0.813 -0.985 -0.963 -0.962 -0.966 ...   
 $ fBodyAccJerk.mean.X      : num  -0.957 -0.986 -0.981 -0.979 -0.983 ...   
 $ fBodyAccJerk.mean.Y      : num  -0.922 -0.983 -0.969 -0.944 -0.965 ...   
 $ fBodyAccJerk.mean.Z      : num  -0.948 -0.986 -0.979 -0.975 -0.983 ...  
 $ fBodyAccJerk.std.X       : num  -0.964 -0.987 -0.983 -0.98 -0.986 ...  
 $ fBodyAccJerk.std.Y       : num  -0.932 -0.985 -0.971 -0.944 -0.966 ...   
 $ fBodyAccJerk.std.Z       : num  -0.961 -0.989 -0.984 -0.98 -0.986 ...   
 $ fBodyGyro.mean.X         : num  -0.85 -0.986 -0.97 -0.967 -0.976 ...   
 $ fBodyGyro.mean.Y         : num  -0.952 -0.983 -0.978 -0.972 -0.978 ...   
 $ fBodyGyro.mean.Z         : num  -0.909 -0.963 -0.962 -0.961 -0.963 ...   
 $ fBodyGyro.std.X          : num  -0.882 -0.989 -0.976 -0.975 -0.981 ...  
 $ fBodyGyro.std.Y          : num  -0.951 -0.982 -0.977 -0.956 -0.977 ...   
 $ fBodyGyro.std.Z          : num  -0.917 -0.963 -0.967 -0.966 -0.963 ...   
 $ fBodyAccMag.mean         : num  -0.862 -0.975 -0.966 -0.939 -0.962 ...   
 $ fBodyAccMag.std          : num  -0.798 -0.975 -0.968 -0.937 -0.963 ...   
 $ fBodyBodyAccJerkMag.mean : num  -0.933 -0.985 -0.976 -0.962 -0.977 ...   
 $ fBodyBodyAccJerkMag.std  : num  -0.922 -0.985 -0.975 -0.958 -0.976 ...   
 $ fBodyBodyGyroMag.mean    : num  -0.862 -0.972 -0.965 -0.962 -0.968 ...   
 $ fBodyBodyGyroMag.std     : num  -0.824 -0.961 -0.955 -0.947 -0.959 ...  
 $ fBodyBodyGyroJerkMag.mean: num  -0.942 -0.99 -0.984 -0.984 -0.985 ...   
 $ fBodyBodyGyroJerkMag.std : num  -0.933 -0.989 -0.983 -0.983 -0.983 ...   

###Variables
* subject - identifier of the subject who carried out the experiment. Its range is from 1 to 30. 

* activity - an activity which was performed by each person: LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, 
WALKING_UPSTAIRS.

* The rest of variables show the average of the series of measurements for each activity and each subject. The names of the variables consist of following descriptors:
    + prefix 't' or 'f' denotes time or frequency domain signals;
    + 'tBodyAcc.XYZ' and 'tGravityAcc.XYZ' - body and gravity acceleration signals;
    + 'tGyro.XYZ' - gyroscope 3-axial raw signals;
    + 'tBodyAccJerk.XYZ' and 'tBodyGyroJerk-XYZ' - Jerk signals;
    + 'tBodyAccMag', 'tGravityAccMag', 'tBodyAccJerkMag', 'tBodyGyroMag', 'tBodyGyroJerkMag' - the calculated magnitude; 
    + 'fBodyAcc.XYZ', 'fBodyAccJerk.XYZ', 'fBodyGyro.XYZ', 'fBodyAccJerkMag', 'fBodyGyroMag', 'fBodyGyroJerkMag' - Fast Fourier Transform (FFT) applied to some the signals;
    + '.mean' and '.std' - the mean and the standard deviation;
    + '.XYZ' - denotes 3-axial signals in the X, Y and Z directions.