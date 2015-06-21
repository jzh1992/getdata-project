# Load raw data from directories
features <- read.table("features.txt")
subtest <- read.table("subject_test.txt")
subtrain<-read.table("subject_train.txt")
xtest<-read.table("X_test.txt")
xtrain<-read.table("X_train.txt")
ytest<-read.table("Y_test.txt")
ytrain<-read.table("Y_train.txt")
activity_label<-read.table("activity_labels.txt")

##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################
# Binding data
test_sensor_data <- cbind(cbind(xtest,subtest),y_train)
test_sensor_data <- cbind(cbind(xtest,subtest),ytest)
sensor_data<-rbind(train_sensor_data,test_sensor_data)

# Label columns
sensor_labels<-rbind(rbind(features,c(562,"Subject")),c(563,"ActivityId"))[,2]
names(sensor_data)<-sensor_labels

############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################
labelIndex<-c(grep("-mean|-std",sensor_labels),562,563)
labels<-sensor_labels[labelIndex]
alldata<-sensor_data[,labelIndex]

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################
alldata$ActivityId<-activity_label[alldata$ActivityId,2]
names(alldata)[81]<-"Activity"

##############################################################
# 4. Appropriately labels the data set with descriptive names.
##############################################################

# Remove parentheses
names(alldata) <- gsub('\\(|\\)',"",names(alldata), perl = TRUE)
# Make syntactically valid names
names(alldata) <- make.names(names(alldata))
# Make clearer names
names(alldata) <- gsub('Acc',"Acceleration",names(alldata))
names(alldata) <- gsub('GyroJerk',"AngularAcceleration",names(alldata))
names(alldata) <- gsub('Gyro',"AngularSpeed",names(alldata))
names(alldata) <- gsub('Mag',"Magnitude",names(alldata))
names(alldata) <- gsub('^t',"TimeDomain.",names(alldata))
names(alldata) <- gsub('^f',"FrequencyDomain.",names(alldata))
names(alldata) <- gsub('\\.mean',".Mean",names(alldata))
names(alldata) <- gsub('\\.std',".StandardDeviation",names(alldata))
names(alldata) <- gsub('Freq\\.',"Frequency.",names(alldata))
names(alldata) <- gsub('Freq$',"Frequency",names(alldata))
######################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################
library("reshape2")
meltData<-melt(alldata,id=c("Subject","Activity"),measure.vars=names(alldata)[1:79])
dcast(meltData,Subject+Activity~variable,mean)
averageData<-dcast(meltData,Subject+Activity~variable,mean)
write.table(averageData, file = "result.txt",row.names=FALSE)
