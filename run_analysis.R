#Create data dir
dir.create("rawdata", showWarnings = FALSE)

# Dowload Files
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
              './rawdata/datafiles.zip')

# Unzip data files
data <- unzip('./rawdata/datafiles.zip')