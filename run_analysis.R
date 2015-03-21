# This script requires to run at the same direcotory of "UCI HAR Dataset" direcotyr
# You must call mergeData function in order to obtain de result Data.Table with the desired mean.
# Example:
#       myResult <- mergeData()

# Required library: 
# - data.table
# - dplyr

mergeData <- function() {

        #Step 1.: read activity label
        atcivitynameDF<- read.table ("UCI HAR Dataset//activity_labels.txt")
        atcivitynameDT <- data.table(atcivitynameDF) 
        
        # Setting column names
        setnames (atcivitynameDT, c("label", "label_name"))
        
        # Setting the key
        setkey(atcivitynameDT,label)
        
        # Step 2.: Read train data features
        featuresSetDF <- read.table ("UCI HAR Dataset//features.txt")
        
        # Step 3.: Create DataTable to manage train data
        # Reading train data 
        trainDataDF<-read.table ("UCI HAR Dataset//train//X_train.txt")        
        
        #  Use data features Data.Frame to give names to all coloumn of trainDataDF
        setnames (trainDataDF, as.character(featuresSetDF$V2))
        
        # Get only mean and standard deviation for each measurement 
        trainDataDT<-data.table(subset(trainDataDF, select=c("tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z","tGravityAcc-std()-X","tGravityAcc-std()-Y","tGravityAcc-std()-Z","tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z","tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z","tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z","tBodyAccMag-std()","tGravityAccMag-std()","tBodyAccJerkMag-std()","tBodyGyroMag-std()","tBodyGyroJerkMag-std()","fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z","fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y","fBodyAccJerk-std()-Z","fBodyGyro-std()-X","fBodyGyro-std()-Y","fBodyGyro-std()-Z","fBodyAccMag-std()","fBodyBodyAccJerkMag-std()","fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-std()","tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z","tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z","tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z","tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z","tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z","tBodyAccMag-mean()","tGravityAccMag-mean()","tBodyAccJerkMag-mean()","tBodyGyroMag-mean()","tBodyGyroJerkMag-mean()","fBodyAcc-mean()-X","fBodyAcc-mean()-Y","fBodyAcc-mean()-Z","fBodyAcc-meanFreq()-X","fBodyAcc-meanFreq()-Y","fBodyAcc-meanFreq()-Z","fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y","fBodyAccJerk-mean()-Z","fBodyAccJerk-meanFreq()-X","fBodyAccJerk-meanFreq()-Y","fBodyAccJerk-meanFreq()-Z","fBodyGyro-mean()-X","fBodyGyro-mean()-Y","fBodyGyro-mean()-Z","fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y","fBodyGyro-meanFreq()-Z","fBodyAccMag-mean()","fBodyAccMag-meanFreq()","fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-meanFreq()","fBodyBodyGyroMag-mean()","fBodyBodyGyroMag-meanFreq()","fBodyBodyGyroJerkMag-mean()","fBodyBodyGyroJerkMag-meanFreq()","angle(tBodyAccMean,gravity)","angle(tBodyAccJerkMean),gravityMean)","angle(tBodyAccJerkMean),gravityMean)","angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)","angle(X,gravityMean)","angle(Y,gravityMean)","angle(Z,gravityMean)")))
        
        # Read train activity file 
        trainLabels<-read.table ("UCI HAR Dataset//train//y_train.txt")
        
        # Add actyvity data to trainData Data.Table
        trainDataDT<-trainDataDT[, label:=trainLabels]
        
        setkey(trainDataDT,label)
        trainDataDT<-merge (trainDataDT,atcivitynameDT)
        
        # read subject  file 
        trainSubjects<-read.table ("UCI HAR Dataset//train//subject_train.txt")
        
        # Add subject data to trainData
        trainDataDT<-trainDataDT[, subject:=trainSubjects]
        
        # Step 4.: Create DataTable to manage test data
        # Read test data
        testDataDF<- read.table ("UCI HAR Dataset//test//X_test.txt")
        setnames (testDataDF, as.character(featuresSetDF$V2))

        # Get only mean and standard deviation for each measurement 
        testDataDT<-data.table(subset(testDataDF, select=c("tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z","tGravityAcc-std()-X","tGravityAcc-std()-Y","tGravityAcc-std()-Z","tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z","tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z","tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z","tBodyAccMag-std()","tGravityAccMag-std()","tBodyAccJerkMag-std()","tBodyGyroMag-std()","tBodyGyroJerkMag-std()","fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z","fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y","fBodyAccJerk-std()-Z","fBodyGyro-std()-X","fBodyGyro-std()-Y","fBodyGyro-std()-Z","fBodyAccMag-std()","fBodyBodyAccJerkMag-std()","fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-std()","tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z","tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z","tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z","tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z","tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z","tBodyAccMag-mean()","tGravityAccMag-mean()","tBodyAccJerkMag-mean()","tBodyGyroMag-mean()","tBodyGyroJerkMag-mean()","fBodyAcc-mean()-X","fBodyAcc-mean()-Y","fBodyAcc-mean()-Z","fBodyAcc-meanFreq()-X","fBodyAcc-meanFreq()-Y","fBodyAcc-meanFreq()-Z","fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y","fBodyAccJerk-mean()-Z","fBodyAccJerk-meanFreq()-X","fBodyAccJerk-meanFreq()-Y","fBodyAccJerk-meanFreq()-Z","fBodyGyro-mean()-X","fBodyGyro-mean()-Y","fBodyGyro-mean()-Z","fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y","fBodyGyro-meanFreq()-Z","fBodyAccMag-mean()","fBodyAccMag-meanFreq()","fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-meanFreq()","fBodyBodyGyroMag-mean()","fBodyBodyGyroMag-meanFreq()","fBodyBodyGyroJerkMag-mean()","fBodyBodyGyroJerkMag-meanFreq()","angle(tBodyAccMean,gravity)","angle(tBodyAccJerkMean),gravityMean)","angle(tBodyAccJerkMean),gravityMean)","angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)","angle(X,gravityMean)","angle(Y,gravityMean)","angle(Z,gravityMean)")))
        
        # read test acivity  file 
        testLabels<-read.table ("UCI HAR Dataset//test//y_test.txt")
        
        # Add actyvity data to testData
        testDataDT<-testDataDT[, label:=testLabels]
        
        setkey(testDataDT,label)  
        testDataDT<-merge (testDataDT,atcivitynameDT)
                
        # read subject  file 
        testSubjects<-read.table ("UCI HAR Dataset//test//subject_test.txt")
        
        # Add subject data to trainData
        testDataDT<-testDataDT[, subject:=testSubjects]
        
        # Step 5.: merge test and train data
        allDataDT <- rbind (trainDataDT,testDataDT )
        
        # Change coulumn name in order to avoid problems with special characters on original colnames
        setnames(allDataDT,c("allDataDT1","allDataDT2","allDataDT3","allDataDT4","allDataDT5","allDataDT6","allDataDT7","allDataDT8","allDataDT9","allDataDT10","allDataDT11","allDataDT12","allDataDT13","allDataDT14","allDataDT15","allDataDT16","allDataDT17","allDataDT18","allDataDT19","allDataDT20","allDataDT21","allDataDT22","allDataDT23","allDataDT24","allDataDT25","allDataDT26","allDataDT27","allDataDT28","allDataDT29","allDataDT30","allDataDT31","allDataDT32","allDataDT33","allDataDT34","allDataDT35","allDataDT36","allDataDT37","allDataDT38","allDataDT39","allDataDT40","allDataDT41","allDataDT42","allDataDT43","allDataDT44","allDataDT45","allDataDT46","allDataDT47","allDataDT48","allDataDT49","allDataDT50","allDataDT51","allDataDT52","allDataDT53","allDataDT54","allDataDT55","allDataDT56","allDataDT57","allDataDT58","allDataDT59","allDataDT60","allDataDT61","allDataDT62","allDataDT63","allDataDT64","allDataDT65","allDataDT66","allDataDT67","allDataDT68","allDataDT69","allDataDT70","allDataDT71","allDataDT72","allDataDT73","allDataDT74","allDataDT75","allDataDT76","allDataDT77","allDataDT78","allDataDT79","allDataDT80","allDataDT81","allDataDT82","allDataDT83","allDataDT84","allDataDT85","allDataDT86","allDataDT87","allDataDT88","allDataDT89","allDataDT90","label_name"
                            , "subject"))
        setkey(allDataDT, subject, label_name)
        
        # Find mean of all measurements grouped by subject and label_name activity
        resultData <- as.data.table(allDataDT[, list(
                mean(allDataDT2),
                mean(allDataDT3),
                mean(allDataDT4),
                mean(allDataDT5),
                mean(allDataDT6),
                mean(allDataDT7),
                mean(allDataDT8),
                mean(allDataDT9),
                mean(allDataDT10),
                mean(allDataDT11),
                mean(allDataDT12),
                mean(allDataDT13),
                mean(allDataDT14),
                mean(allDataDT15),
                mean(allDataDT16),
                mean(allDataDT17),
                mean(allDataDT18),
                mean(allDataDT19),
                mean(allDataDT20),
                mean(allDataDT21),
                mean(allDataDT22),
                mean(allDataDT23),
                mean(allDataDT24),
                mean(allDataDT25),
                mean(allDataDT26),
                mean(allDataDT27),
                mean(allDataDT28),
                mean(allDataDT29),
                mean(allDataDT30),
                mean(allDataDT31),
                mean(allDataDT32),
                mean(allDataDT33),
                mean(allDataDT34),
                mean(allDataDT35),
                mean(allDataDT36),
                mean(allDataDT37),
                mean(allDataDT38),
                mean(allDataDT39),
                mean(allDataDT40),
                mean(allDataDT41),
                mean(allDataDT42),
                mean(allDataDT43),
                mean(allDataDT44),
                mean(allDataDT45),
                mean(allDataDT46),
                mean(allDataDT47),
                mean(allDataDT48),
                mean(allDataDT49),
                mean(allDataDT50),
                mean(allDataDT51),
                mean(allDataDT52),
                mean(allDataDT53),
                mean(allDataDT54),
                mean(allDataDT55),
                mean(allDataDT56),
                mean(allDataDT57),
                mean(allDataDT58),
                mean(allDataDT59),
                mean(allDataDT60),
                mean(allDataDT61),
                mean(allDataDT62),
                mean(allDataDT63),
                mean(allDataDT64),
                mean(allDataDT65),
                mean(allDataDT66),
                mean(allDataDT67),
                mean(allDataDT68),
                mean(allDataDT69),
                mean(allDataDT70),
                mean(allDataDT71),
                mean(allDataDT72),
                mean(allDataDT73),
                mean(allDataDT74),
                mean(allDataDT75),
                mean(allDataDT76),
                mean(allDataDT77),
                mean(allDataDT78),
                mean(allDataDT79),
                mean(allDataDT80),
                mean(allDataDT81),
                mean(allDataDT82),
                mean(allDataDT83),
                mean(allDataDT84),
                mean(allDataDT85),
                mean(allDataDT86),
                mean(allDataDT87),
                mean(allDataDT88),
                mean(allDataDT89),
                mean(allDataDT90)
                ),by = list(subject, label_name)])                                   
        
        # restore orginal colnames
        setnames (resultData, c("Subject", "Activity", "tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z","tGravityAcc-std()-X","tGravityAcc-std()-Y","tGravityAcc-std()-Z","tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z","tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z","tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z","tBodyAccMag-std()","tGravityAccMag-std()","tBodyAccJerkMag-std()","tBodyGyroMag-std()","tBodyGyroJerkMag-std()","fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z","fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y","fBodyAccJerk-std()-Z","fBodyGyro-std()-X","fBodyGyro-std()-Y","fBodyGyro-std()-Z","fBodyAccMag-std()","fBodyBodyAccJerkMag-std()","fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-std()","tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z","tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z","tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z","tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z","tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z","tBodyAccMag-mean()","tGravityAccMag-mean()","tBodyAccJerkMag-mean()","tBodyGyroMag-mean()","tBodyGyroJerkMag-mean()","fBodyAcc-mean()-X","fBodyAcc-mean()-Y","fBodyAcc-mean()-Z","fBodyAcc-meanFreq()-X","fBodyAcc-meanFreq()-Y","fBodyAcc-meanFreq()-Z","fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y","fBodyAccJerk-mean()-Z","fBodyAccJerk-meanFreq()-X","fBodyAccJerk-meanFreq()-Y","fBodyAccJerk-meanFreq()-Z","fBodyGyro-mean()-X","fBodyGyro-mean()-Y","fBodyGyro-mean()-Z","fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y","fBodyGyro-meanFreq()-Z","fBodyAccMag-mean()","fBodyAccMag-meanFreq()","fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-meanFreq()","fBodyBodyGyroMag-mean()","fBodyBodyGyroMag-meanFreq()","fBodyBodyGyroJerkMag-mean()","fBodyBodyGyroJerkMag-meanFreq()","angle(tBodyAccMean,gravity)","angle(tBodyAccJerkMean),gravityMean)","angle(tBodyAccJerkMean),gravityMean)","angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)","angle(X,gravityMean)","angle(Y,gravityMean)","angle(Z,gravityMean)"))
        
        # Write result file
        write.table(resultData,row.name=FALSE, file="./result.txt", append=FALSE)
        
        # return the data.table
        resultData

}