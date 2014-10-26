---
title: "Codebook"
author: "Matt Littleton"
date: "Sunday, October 26, 2014"
output: html_document
---

## Course Project - Getting and Cleaning Data

###Description
  
  Code book to accompany to the course project for Getting and cleaning Data

###Data Source
  
    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
    
    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
    
###Information from Data Source

  Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto. 
  Smartlab - Non Linear Complex Systems Laboratory 
  DITEN - Universit√  degli Studi di Genova, Genoa I-16145, Italy. 
  activityrecognition '@' smartlab.ws 
  www.smartlab.ws 
  
  Data Set Information:
  
  The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.
  
  The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 
  
  Check the README.txt file for further details about this dataset.
  
  Attribute Information:
  
  For each record in the dataset it is provided: 
  - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
  - Triaxial Angular velocity from the gyroscope. 
  - A 561-feature vector with time and frequency domain variables. 
  - Its activity label. 
  - An identifier of the subject who carried out the experiment.
  
  Relevant Papers:
  
  N/A
  
  Citation Request:
  
  [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on      Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living     (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

###Variables in final dataset

Subject : ID of the person completeing the test (30 total)
Activity_Name : List of activities completed by each subject (WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING)

Time_BodyAcceleration-Mean()-X  																																																																		
Time_BodyAcceleration-Mean()-Y																																																																			
Time_BodyAcceleration-Mean()-Z																																																																			
Time_BodyAcceleration-StrdDev()-X																																																																			
Time_BodyAcceleration-StrdDev()-Y																																																																			
Time_BodyAcceleration-StrdDev()-Z																																																																			
Time_GravityAcceleration-Mean()-X																																																																			
Time_GravityAcceleration-Mean()-Y																																																																			
Time_GravityAcceleration-Mean()-Z																																																																			
Time_GravityAcceleration-StrdDev()-X																																																																			
Time_GravityAcceleration-StrdDev()-Y																																																																			
Time_GravityAcceleration-StrdDev()-Z																																																																			
Time_BodyAccelerationJerk-Mean()-X																																																																			
Time_BodyAccelerationJerk-Mean()-Y																																																																			
Time_BodyAccelerationJerk-Mean()-Z																																																																			
Time_BodyAccelerationJerk-StrdDev()-X																																																																			
Time_BodyAccelerationJerk-StrdDev()-Y																																																																			
Time_BodyAccelerationJerk-StrdDev()-Z																																																																			
Time_BodyGyro-Mean()-X																																																																			
Time_BodyGyro-Mean()-Y																																																																			
Time_BodyGyro-Mean()-Z																																																																			
Time_BodyGyro-StrdDev()-X																																																																			
Time_BodyGyro-StrdDev()-Y																																																																			
Time_BodyGyro-StrdDev()-Z																																																																			
Time_BodyGyroJerk-Mean()-X																																																																			
Time_BodyGyroJerk-Mean()-Y																																																																			
Time_BodyGyroJerk-Mean()-Z																																																																			
Time_BodyGyroJerk-StrdDev()-X																																																																			
Time_BodyGyroJerk-StrdDev()-Y																																																																			
Time_BodyGyroJerk-StrdDev()-Z																																																																			
Time_BodyAccelerationMag-Mean()																																																																			
Time_BodyAccelerationMag-StrdDev()																																																																			
Time_GravityAccelerationMag-Mean()																																																																			
Time_GravityAccelerationMag-StrdDev()																																																																			
Time_BodyAccelerationJerkMag-Mean()																																																																			
Time_BodyAccelerationJerkMag-StrdDev()																																																																			
Time_BodyGyroMag-Mean()																																																																			
Time_BodyGyroMag-StrdDev()																																																																			
Time_BodyGyroJerkMag-Mean()																																																																			
Time_BodyGyroJerkMag-StrdDev()																																																																			
Freq_BodyAcceleration-Mean()-X																																																																			
Freq_BodyAcceleration-Mean()-Y																																																																			
Freq_BodyAcceleration-Mean()-Z																																																																			
Freq_BodyAcceleration-StrdDev()-X																																																																			
Freq_BodyAcceleration-StrdDev()-Y																																																																			
Freq_BodyAcceleration-StrdDev()-Z																																																																			
Freq_BodyAccelerationJerk-Mean()-X																																																																			
Freq_BodyAccelerationJerk-Mean()-Y																																																																			
Freq_BodyAccelerationJerk-Mean()-Z																																																																			
Freq_BodyAccelerationJerk-StrdDev()-X																																																																			
Freq_BodyAccelerationJerk-StrdDev()-Y																																																																			
Freq_BodyAccelerationJerk-StrdDev()-Z																																																																			
Freq_BodyGyro-Mean()-X																																																																			
Freq_BodyGyro-Mean()-Y																																																																			
Freq_BodyGyro-Mean()-Z																																																																			
Freq_BodyGyro-StrdDev()-X																																																																			
Freq_BodyGyro-StrdDev()-Y																																																																			
Freq_BodyGyro-StrdDev()-Z																																																																			
Freq_BodyAccelerationMag-Mean()																																																																			
Freq_BodyAccelerationMag-StrdDev()																																																																			
Freq_BodyBodyAccelerationJerkMag-Mean()																																																																			
Freq_BodyBodyAccelerationJerkMag-StrdDev()																																																																			
Freq_BodyBodyGyroMag-Mean()																																																																			
Freq_BodyBodyGyroMag-StrdDev()																																																																			
Freq_BodyBodyGyroJerkMag-Mean()																																																																			
Freq_BodyBodyGyroJerkMag-StrdDev()																																																																			


