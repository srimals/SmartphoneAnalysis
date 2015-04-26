run_analysis <- function() {
        
        
        #***** START **** Read file content to appropriate data frame varaiables.
        dTestActivity <- read.table("y_test.txt",header=FALSE)
        dTrainActivity <- read.table("y_train.txt", header = FALSE)
        
        dTestFeatures <- read.table("X_test.txt", header=FALSE)
        dTrainFeatures <- read.table("X_train.txt", header=FALSE)
        
        dTestSubject <- read.table("subject_test.txt", header=FALSE)
        dTrainSubject <- read.table("subject_train.txt", header=FALSE)
        
        dFeaturesLabel <- read.table("features.txt", header=FALSE)
        activityLabels <- read.table("activity_labels.txt", header=FALSE)
        #***** END **** Read file content to appropriate data frame varaiables.
        
        
        # *** START *** MERGE Training and Test Data Frame using row bind
        dAllActivity <- rbind(dTrainActivity, dTestActivity)
        dAllFeatures <- rbind(dTrainFeatures, dTestFeatures)
        dAllSubject <- rbind(dTrainSubject, dTestSubject)
        # *** END *** MERGE Training and Test Data Frame using row bind
        
        #rename column names.
        names(dAllSubject) <- c("subject")
        names(dAllActivity) <- c("activity")
        
        #rename columns as per features label file.
        names(dAllFeatures) <- dFeaturesLabel$V2
        
        
        #bind subject and activity
        finalData <- cbind(dAllSubject, dAllActivity)
        
        #bind features into subject and activities.
        finalData <- cbind(finalData, dAllFeatures)
        
        #mark subset of only mean and stddeviation measurements columns.
        meanAndStds <- dFeaturesLabel$V2[grep("mean\\(\\)|std\\(\\)", dFeaturesLabel$V2)]
        
        #subset the data to only mean and std deviation measurements.
        Data <- subset(finalData, select=c("subject", "activity", as.character(meanAndStds)))
        
        # **** START **** apply activity lables to dataset and remove interim columns used.
        activityLabels <- read.table("activity_labels.txt", header=FALSE)
        
        Data$ROWID <- 1:nrow(Data)
        
        unOrderedData <- merge(x=Data, y=activityLabels, by.x="activity", by.y="V1", all=TRUE)
        
        Data <- unOrderedData[order(unOrderedData$ROWID),]
        
        Data$ROWID <- NULL
        Data$activity <- NULL
        # **** END **** apply activity lables to dataset and remove interim columns used.
        
        
        #rename columns with more descriptivie.
        names(Data) <- gsub("Acc", "Accelerometer", names(Data))
        names(Data) <- gsub("Gyro", "Gyroscope", names(Data))
        names(Data) <- gsub("Mag", "Magnitude", names(Data))
        names(Data) <- gsub("BodyBody", "Body", names(Data))
        names(Data) <-gsub("^t", "time.", names(Data))
        names(Data) <-gsub("^f", "frequency.", names(Data))
        names(Data) <- gsub("V2", "activity", names(Data))
        
        #Aggregate data based on subject and activity and output by order(subject and activity)
        AggregateData <- aggregate(. ~subject + activity, Data, mean)
        AggregateData <- AggregateData[order(AggregateData$subject,AggregateData$activity),]
        
        #Output Content
        AggregateData
}