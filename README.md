---
title: "Smartphone Analysis"
author: "Srini Kuppuswamy"
date: "Sunday, April 26, 2015"
output: html_document
---

#Run below function analysis the data captured by smartphone by subjects and different activities performed by each subject.

## Pre-requisite. extract all the files from zip into a single folder and copy run_analysis.R file into the same folder before running the script.


```{r}
run_analysis()
```

##  Read file content to appropriate data frame varaiables.

```{r}
        dTestActivity <- read.table("y_test.txt",header=FALSE)
        dTrainActivity <- read.table("y_train.txt", header = FALSE)
        
        dTestFeatures <- read.table("X_test.txt", header=FALSE)
        dTrainFeatures <- read.table("X_train.txt", header=FALSE)
        
        dTestSubject <- read.table("subject_test.txt", header=FALSE)
        dTrainSubject <- read.table("subject_train.txt", header=FALSE)
        
        dFeaturesLabel <- read.table("features.txt", header=FALSE)
        activityLabels <- read.table("activity_labels.txt", header=FALSE)

```

## MERGE Training and Test Data Frame using row bind
```{r}
        dAllActivity <- rbind(dTrainActivity, dTestActivity)
        dAllFeatures <- rbind(dTrainFeatures, dTestFeatures)
        dAllSubject <- rbind(dTrainSubject, dTestSubject)
```

##rename column names.
```{r}
        names(dAllSubject) <- c("subject")
        names(dAllActivity) <- c("activity")
        
```

##rename column names.
```{r}
        names(dAllSubject) <- c("subject")
        names(dAllActivity) <- c("activity")
```


##rename columns as per features label file.
```{r}
        names(dAllFeatures) <- dFeaturesLabel$V2
```

***********************************************************



##rename columns as per features label file.
```{r}
        names(dAllFeatures) <- dFeaturesLabel$V2
```



##bind subject and activity
```{r}
        finalData <- cbind(dAllSubject, dAllActivity)
```




##bind features into subject and activities.
```{r}
        finalData <- cbind(finalData, dAllFeatures)
```



##mark subset of only mean and stddeviation measurements columns.
```{r}
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
```



##rename columns with more descriptivie.
```{r}
        names(Data) <- gsub("Acc", "Accelerometer", names(Data))
        names(Data) <- gsub("Gyro", "Gyroscope", names(Data))
        names(Data) <- gsub("Mag", "Magnitude", names(Data))
        names(Data) <- gsub("BodyBody", "Body", names(Data))
        names(Data) <-gsub("^t", "time.", names(Data))
        names(Data) <-gsub("^f", "frequency.", names(Data))
        names(Data) <- gsub("V2", "activity", names(Data))
```



##Aggregate data based on subject and activity and output by order(subject and activity)
```{r}
        AggregateData <- aggregate(. ~subject + activity, Data, mean)
        AggregateData <- AggregateData[order(AggregateData$subject,AggregateData$activity),]
```



##Final output
```{r}
        AggregateData
```

