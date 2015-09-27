run_analysis <- function(infoPath, testPath, trainPath, outputPath) {
    library(dplyr)      ##loads dplyr package
    
    ##Sets path to folder containing activity and features information
        ##Reads activity_labels file and assigns names to its columns
        # setwd("~/Personal/Coursera/data-science/Getting and Cleaning Data/UCI HAR Dataset")
        setwd(infoPath)
        activities <- read.table("activity_labels.txt")
        names(activities) <- c("Label", "Activity")
        
        ##Reads features file and creates a vector of names of all features
        ##along with columns for the subject and activity label
        features <- read.table("features.txt")
        featureNames <- c("Subject", "Label", as.character(features[,2]))
        
    
    ##Reads test datasets from given path and combines into one data frame
    setwd(testPath)
    #setwd("~/Personal/Coursera/data-science/Getting and Cleaning Data/UCI HAR Dataset/test")
    Xtest <- read.table("X_test.txt")
    Ytest <- read.table("y_test.txt")
    testSubjects <- read.table("subject_test.txt")
    test <- cbind(testSubjects, Ytest, Xtest)
    
    ##Rename combined dataset columns using featureNames from line 12
    names(test) <- featureNames

    ##Reads train datasets from given path and combines into one data frame
    setwd(trainPath)
    ##setwd("~/Personal/Coursera/data-science/Getting and Cleaning Data/UCI HAR Dataset/train")
    Xtrain <- read.table("X_train.txt")
    Ytrain <- read.table("y_train.txt")
    trainSubjects <- read.table("subject_train.txt")
    train <- cbind(trainSubjects, Ytrain, Xtrain)
    
    ##Rename combined dataset columns using featureNames from line 12
    names(train) <- featureNames

    ##Combine test and train sets
    testAndTrain <- rbind(test, train)
    
    ##Retrieve indices for all required columns (means and standard deviations)
    means <- grep("mean()", featureNames, fixed = TRUE)
    stdevs <- grep("std", featureNames, fixed = TRUE)
    myColumns <- sort(c(1:2, means, stdevs))
    
    ##Select subset of combined data to only pull desired columns
    meansAndStd <- testAndTrain[,myColumns]
    
    ##Join dataset with activities data to include activity names
    reqData <- merge(activities, meansAndStd, by = "Label", sort = FALSE)
    
    ##Rename columns to include "Average" prefix to describe operation being
    ##performed on each variable as part of this exercise
    correctedNames <- make.names(gsub("()", "", names(reqData), fixed = TRUE))
    finalNames <- c(correctedNames[1:3], paste("Average", correctedNames[-(1:3)], sep = "_"))
    names(reqData) <- finalNames
    
    ##Drop first column (Activity "Label" numeric value -- the actual name is now
    ##included because of earlier merge
    allReqData <- tbl_df(reqData[,-1])
    
    ##Chained functions - groups by activity and subject, and calculates means
    ##for each pairing of activity/subject
    final <- allReqData %>% 
        group_by(Activity, Subject) %>%
        summarize_each(funs(mean))
    
    ##Output result to text file
    setwd(outputPath)
    write.table(final, "Getting and Cleaning Data Project Output.txt", row.names = FALSE)
}