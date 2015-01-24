run_analysis <- function() {
  	library(reshape2)
  	library(plyr)
  #setting working directory to downloaded folder
	setwd('./UCI HAR Dataset')

  #read the raw label files into R
	taskactivity <- read.table("activity_labels.txt", col.names=c("V1","Activity.Name"))
  	taskfeatures <- read.table("features.txt", stringsAsFactors=FALSE)
  
  #remove () from the features as columnnames
   	taskfeatures <- cbind(taskfeatures,names=gsub("()","",taskfeatures$V2, fixed=TRUE), stringsAsFactors=FALSE)
  
  #read the raw test and train files into R
  	testsubjects <- read.table("test/subject_test.txt", col.names="subject")
  	testyaxis <- read.table("test/y_test.txt", col.names="activity")
  	testxaxis <- read.table("test/X_test.txt", col.names=taskfeatures$names)
  	trainsubjects <- read.table("train/subject_train.txt", col.names="subject")
  	trainyaxis <- read.table("train/y_train.txt", col.names="activity")
  	trainxaxis <- read.table("train/X_train.txt", col.names=taskfeatures$names)
  #combines the test and train data set data together with the subject number(xaxis) and activity info(yaxis)

  testtotaldata <- cbind(testsubjects, testyaxis, sample="test", testxaxis, stringsAsFactors=FALSE)
  traintotaldata <- cbind(trainsubjects, trainyaxis, sample="train", trainxaxis, stringsAsFactors=FALSE)

  #combining both train and test data together
  alldata <- rbind(testtotaldata,traintotaldata)

  #give descriptive activity names to the combined data set
  alldata <- merge(alldata,taskactivity, by.x="activity", by.y="V1", sort=FALSE)

  #selects only the standard deviation and mean variables from the combined set
  stdmeansubjests <- alldata[,grep("mean|std|subject|Activity.Name|sample",names(alldata))]
  #reshapes the data into a summarized form and then sorts it
  filteredsubjects <- melt(stdmeansubjests, c(1,2,82))
  finalsubjects <- aggregate(filteredsubjects$value, filteredsubjects[,1:4], mean)
  finalsubjects  <- arrange(finalsubjects , subject, Activity.Name, variable)
  names(finalsubjects) <- c("Subject.ID","Sample.Source","Activity.Name","Variable.Name","Mean.Value")
  write.table(sumdata, 'tidydata.txt',row.name=FALSE)
  #setting back to parent directory   
   setwd('..')
}
