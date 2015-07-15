require(dplyr)

run_analysis <- function(){
  
  
  ################
  ### Get Data ###
  ################
  
  
  pathDS <- file.path("./data" , "UCI HAR Dataset")
  pathDStest <- file.path(pathDS, "test")
  pathDStrain <- file.path(pathDS, "train")
  
  # Read Features (data)
  
  train.data<-read.table(file.path(pathDStrain, "X_train.txt"),header= FALSE,nrows=5)
  classes <- lapply(x.train, class)
  train.data<-read.table(file.path(pathDStrain, "X_train.txt"),header= FALSE,colClasses=classes)
  train.data <- tbl_df(x.train)

  test.data <- tbl_df(read.table(file.path(pathDStest, "X_test.txt"),header= FALSE,colClasses=classes))   

  
  # Read Subject files
  
  train.subject <- tbl_df(read.table(file.path(pathDStrain, "subject_train.txt"),header= FALSE))
  test.subject <- tbl_df(read.table(file.path(pathDStest, "subject_test.txt"),header= FALSE))   
  
  
  
  # Read Activity files
  
  train.activity <- tbl_df(read.table(file.path(pathDStrain, "Y_train.txt"),header= FALSE))
  test.activity <- tbl_df(read.table(file.path(pathDStest, "Y_test.txt"),header= FALSE)) 
  
  
  # Read Names Features 
  
  feature.names <- tbl_df(read.table(file.path(pathDS,"features.txt"),header = FALSE, stringsAsFactors = FALSE))
  
  
  
  #######################################################################
  ### 1. Merges the training and the test sets to create one data set ###
  #######################################################################
  
  # Concatenate the data tables (data,subjects,activities) by rows
  
  traintest <- bind_rows(train.data,test.data)
  traintest.subjects <- bind_rows(train.subject,test.subject)
  traintest.activity <- bind_rows(train.activity,test.activity)
  
  # Concatenate the data tables (data,subjects,activities) by columns
  
  subject.activity <- bind_cols(traintest.subjects,traintest.activity)
  traintest.all <- bind_cols(traintest,subject.activity)
  
  # Assing original column names 
  
  names(traintest.all)<- c(feature.names$V2,"subject","activity")
  
  
  
 
  ################################################################################################# 
  ### 2. Extracts only the measurements on the mean and standard deviation for each measurement ###
  ################################################################################################# 
  
  
  traintest.meanstd <- traintest.all[,grepl("mean\\(\\)|std\\(\\)|subject|activity", names(traintest.all))]
  
  
  
  #################################################################################
  ### 3. Uses descriptive activity names to name the activities in the data set ###
  #################################################################################
  
  
  activity.labels <- read.table(file.path(pathDS,"activity_labels.txt"), header=FALSE)
  traintest.meanstd.act <- mutate(traintest.meanstd, activity = activity.labels$V2[activity])
  
  
  ####################################################################
  ### 4. Appropriately labels the data set with descriptive names. ###
  ####################################################################
  
  
  names(traintest.meanstd.act) <- gsub("([()])","",names(traintest.meanstd.act))
  names(traintest.meanstd.act)<-gsub("^f", "frequency", names(traintest.meanstd.act))
  names(traintest.meanstd.act)<-gsub("^t", "time", names(traintest.meanstd.act))
  names(traintest.meanstd.act)<-gsub("Acc", "Accelerometer", names(traintest.meanstd.act))
  names(traintest.meanstd.act)<-gsub("Gyro", "Gyroscope", names(traintest.meanstd.act))
  names(traintest.meanstd.act)<-gsub("Mag", "Magnitude", names(traintest.meanstd.act))
  names(traintest.meanstd.act)<-gsub("BodyBody", "Body", names(traintest.meanstd.act))
  names(traintest.meanstd.act)<-make.names(names(traintest.meanstd.act))
  
  

  ###################################################################################
  ### 5. From the data set in step 4, creates a second, independent tidy data set ###
  ### with the average of each variable for each activity and each subject        ###
  ###################################################################################
  
  
  newData <- group_by(data, activity, subject)  
  newData.summary <- summarise_each(newData, funs(mean))
  write.table(newData.summary, file = "tidyData.txt",row.name=FALSE)
  
  
  
  }