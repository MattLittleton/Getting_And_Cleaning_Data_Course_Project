run <- function() {
  
  library(plyr)
  
  ##The first section reads in the tables for the test data and assigns them column names
  
  test_Data <- read.table("X_test.txt", header = FALSE)
  
  test_newNames <-  read.table("../features.txt", header = FALSE)
  
  test_yinfo <- read.table("y_test.txt", header = FALSE)
  
  colnames(test_yinfo) <- c("Activity_Type")
  
  test_subject <- read.table("subject_test.txt", header = FALSE)
  
  colnames(test_subject) <- c("Subject")
  
  colnames(test_newNames) <- c("","Names")
  
  test_newNames <- test_newNames[,"Names"]
  
  colnames(test_Data) <- test_newNames
  
  ##Determines which columns have std or mean (the ones we want to keep)
  
  test_bnames <- ((grepl("std()", colnames(test_Data)) | grepl("mean()", colnames(test_Data))) & !grepl("Freq", colnames(test_Data)))
  
  ##Removes the columns that don't
  
  test_Data <- test_Data[, test_bnames]
  
  ##Combines all test files together
  
  test_Data <- cbind(test_Data,test_yinfo,test_subject)
  
  #################################################################################
  
  ##The second part does does the exact same as the first, only for the train data
  
  train_Data <- read.table("../train/X_train.txt", header = FALSE)
  
  train_newNames <-  read.table("../features.txt", header = FALSE)
  
  train_yinfo <- read.table("../train/y_train.txt", header = FALSE)
  
  colnames(train_yinfo) <- c("Activity_Type")
  
  train_subject <- read.table("../train/subject_train.txt", header = FALSE)
  
  colnames(train_subject) <- c("Subject")
  
  colnames(train_newNames) <- c("","Names")
  
  train_newNames <- train_newNames[,"Names"]
  
  colnames(train_Data) <- train_newNames
  
  train_bnames <- ((grepl("std()", colnames(train_Data)) | grepl("mean()", colnames(train_Data))) & !grepl("Freq", colnames(train_Data)))
  
  train_Data <- train_Data[, train_bnames]
  
  train_Data <- cbind(train_Data,train_yinfo,train_subject)
  
  #################################################################################
  
  ##Combine test and train data
  
  total_Data <- rbind(test_Data, train_Data)
  
  ##Read in the activity labels
  
  activity_Data <- read.table("../activity_labels.txt", header = FALSE)
  
  ##Assign column names to activity labels table
    
  colnames(activity_Data) <- c("Activity_Type","Activity_Name") 
  
  ##Merge the labels in based on new column names

  total_Data <- merge(total_Data, activity_Data)
  
  ##Grabs all current column names for renaming 
  
  total_Data_Names <- colnames(total_Data)
  
  ##Specifies which old values will be replaced with new values
  
  old_Values <- c("mean", "std", "Acc", "^(t)", "^(f)")
  
  new_Values <- c("Mean","StrdDev", "Acceleration", "Time_", "Freq_")
  
  ##Loops through all column names to see if any old values exist in them, if so replace with the new value in total_Data_Names
  
  i <- 1
  
  while (i <= length(total_Data_Names)) {
    j <- 1

    while (j <= length(old_Values)){
    total_Data_Names[i] <- gsub(old_Values[j], new_Values[j], as.character(total_Data_Names[i]))
    j <- j + 1
    }
    
    i <- i + 1 
  }
  
  ##Assign the updated column names
  
  colnames(total_Data) <- total_Data_Names
  
  #################################################################################
  
  ##Grabs all unique subjects and activies for subsetting
  
  subjects_List <- unique(total_Data$Subject)
  activities_List <- unique(total_Data$Activity_Name)
  
  ##Creates an empty dataframe for storage
  
  total_Data_Means <- data.frame()
  
  ##Loops through each subject and each activity 
  
  for (s in subjects_List) {
    for (a in activities_List) {
      
      ##subsets based on each
      newData <- total_Data[(total_Data$Subject == s & total_Data$Activity_Name == a),]
      
      ##removes the non-numeric column so the mean can be taken
      newData <- newData[, -which(names(newData) %in% c("Activity_Name"))]
      
      ##finds the mean of all columns
      means <- colMeans(newData)
      
      ##adds back in the non-numeric column
      means <- c(means, a)
      
      ##converts means to a dataframe
      means <- data.frame(lapply(means, type.convert), stringsAsFactors=FALSE)
      
      ##assigns the proper column names
      colnames(means) <- total_Data_Names
        
      ##appends to storage list
      total_Data_Means <- rbind(total_Data_Means, means)

    }
  }
  
  ##removes unneccessary column, moves subject and activity name columns to the front of the dataframe and sorts on them
  total_Data_Means <- total_Data_Means[, -which(names(total_Data_Means) %in% c("Activity_Type"))]
  total_Data_Means <- total_Data_Means[,c(ncol(total_Data_Means),1:(ncol(total_Data_Means)-1))]
  total_Data_Means <- total_Data_Means[,c(ncol(total_Data_Means),1:(ncol(total_Data_Means)-1))]
  total_Data_Means <- arrange(total_Data_Means, Subject, Activity_Name)
  
  ##writes results to a table
  write.table( total_Data_Means, file="data_final.txt" )
}