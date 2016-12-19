# Set Working directoy (setwd) to directory this script was dowloaded to

#import data.table package if nessessary
#use cmd: install.packages("data.table") 
library(data.table)

#Create data dir
dir.create("rawdata", showWarnings = FALSE)

# Dowload Files
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
              './rawdata/datafiles.zip')

# Unzip data files
data <- unzip('./rawdata/datafiles.zip')

# read in data files
tableSubjectTrain <- fread(file.path('./UCI HAR Dataset/', "train", "subject_train.txt"))
tableSubjectTest  <- fread(file.path('./UCI HAR Dataset/', "test" , "subject_test.txt" ))

tableActivityTrainY <- fread(file.path('./UCI HAR Dataset/', "train", "Y_train.txt"))
tableActivityTestY  <- fread(file.path('./UCI HAR Dataset/', "test" , "Y_test.txt" ))

tableActivityTrainX <- fread(file.path('./UCI HAR Dataset/', "train", "X_train.txt"))
tableActivityTestX  <- fread(file.path('./UCI HAR Dataset/', "test" , "X_test.txt" ))

#merge data
tableSubject <- rbind(tableSubjectTrain, tableSubjectTest)
setnames(tableSubject, "V1", "subject")

tableActivity <- rbind(tableActivityTrainY, tableActivityTestY)
setnames(tableActivity, "V1", "activity")

tableActivityX <- rbind(tableActivityTrainX, tableActivityTestX)

tableSubject <- cbind(tableSubject, tableActivity)
masterTable <- cbind(tableSubject, tableActivityX)

setkey(masterTable, subject, activity)

#Read in feature lists:
tableFeatures <- fread(file.path('./UCI HAR Dataset/', "features.txt"))
setnames(tableFeatures, names(tableFeatures), c("Feature", "featureName"))

#Subset Mean and Standard Deviation
tableFeatures <- tableFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

#Match column names to masterTable
tableFeatures$Code <- tableFeatures[, paste0("V", Feature)]
head(tableFeatures)
#tableFeatures$Code

#Var Names Subset
 featureByCode<- c(key(masterTable), tableFeatures$Code)
 masterTable <- masterTable[, featureByCode, with=FALSE]
 
 #Read in descriptive names for activities
 tableActivityLabels <-  fread(file.path('./UCI HAR Dataset/', "activity_labels.txt"))
 setnames(tableActivityLabels, names(tableActivityLabels), c("activity", "activityName"))

 #Merge activity descriptions into masterTable
 masterTable <- merge(masterTable, tableActivityLabels, by="activity", all.x=TRUE) 
 
 # Add activity name as a key
 setkey(masterTable, subject, activity, activityName)
 
# Melt masterTable to make more readable
 masterTable <- data.table(melt(masterTable, key(masterTable), variable.name="Code"))
 
 # Merge activity one more time
 masterTable <- merge(masterTable, tableFeatures[, list(Feature, featureName, Code)], by="Code", all.x=TRUE)
 #masterTable <- merge(masterTable, masterTableFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
 
 # Create 2 factors with activity and feature information
 masterTable$activity <- factor(masterTable$activityName)
 masterTable$feature <- factor(masterTable$featureName)
 
 
 ## greping for oservations with omore than one feature
 grepMasterTable <- function (regex) {
     grepl(regex, masterTable$feature)
 }
 ## Features with 2 categories
 n <- 2
 y <- matrix(seq(1, n), nrow=n)
 x <- matrix(c(grepMasterTable("^t"), grepMasterTable("^f")), ncol=nrow(y))
 masterTable$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
 x <- matrix(c(grepMasterTable("Acc"), grepMasterTable("Gyro")), ncol=nrow(y))
 masterTable$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
 x <- matrix(c(grepMasterTable("BodyAcc"), grepMasterTable("GravityAcc")), ncol=nrow(y))
 masterTable$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
 x <- matrix(c(grepMasterTable("mean()"), grepMasterTable("std()")), ncol=nrow(y))
 masterTable$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
 ## Features with 1 category
 masterTable$featJerk <- factor(grepMasterTable("Jerk"), labels=c(NA, "Jerk"))
 masterTable$featMagnitude <- factor(grepMasterTable("Mag"), labels=c(NA, "Magnitude"))
 ## Features with 3 categories
 n <- 3
 y <- matrix(seq(1, n), nrow=n)
 x <- matrix(c(grepMasterTable("-X"), grepMasterTable("-Y"), grepMasterTable("-Z")), ncol=nrow(y))
 masterTable$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
 
 setkey(masterTable, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
 tidyData <- masterTable[, list(count = .N, average = mean(value)), by=key(masterTable)]
 
 codebook <- knit("run_analysis.R", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
 
