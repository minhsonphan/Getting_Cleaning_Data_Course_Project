packages <- c("data.table")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

######################################################################################### 
##      Merges the training and the test sets to create one data set for measurement data
##      Extracts only the measurements on the mean and standard deviation for each measurement
##      Add a column to identify test or training data
##      Add a column to subject id
##      Add a column for the activity
##      Add a column to identify experiment id
#########################################################################################

process_measurement <- function(test_training = "test"){
        
        ## Load the features in a table
        file_features <- file.path(getwd(), "./Data/UCI HAR Dataset/features.txt")
        dt_features <- read.table(file_features)
        
        ## Retrieve the columns names corresponding to "mean()" or "std()"
        dt_features_mean_std <<- dt_features[grepl("mean()|std()",dt_features$V2),]
        
        if(test_training=="test"){
                file_path="./Data/UCI HAR Dataset/test/X_test.txt"
        }else{
                file_path="./Data/UCI HAR Dataset/train/X_train.txt"                
        }
        
        ## Load the X_test.txt in a table
        file_X <- file.path(getwd(), file_path)
        dt_file_X <- read.table(file_X)
        
        ## Retrieve a logical vector identifying the features corrsponding to "mean()" or "std()"
        ## and the columns from dt_file_X corresponding to "mean()" or "std()"
        dt_file_X2 <- dt_file_X[,grepl("mean()|std()",dt_features$V2)]
        
        ## add the column names to dt_file_X
        names(dt_file_X2)<-  dt_features_mean_std$V2
        
        ## add a column "settype" that contains either "test data" or "training data"
        if(test_training=="test"){
                test_training_column <- rep("test data",nrow(dt_file_X2))
        }else{
                test_training_column <- rep("training data",nrow(dt_file_X2))
        }
        
        dt_file_X2 <- cbind(test_training_column,dt_file_X2)
        names(dt_file_X2)[1]<-"settype"
        
        ## add a column "activity" that contains the label of the activity"
        if(test_training=="test"){
                file_path="./Data/UCI HAR Dataset/test/y_test.txt"
        }else{
                file_path="./Data/UCI HAR Dataset/train/y_train.txt"                
        }
        
        ## Load the activity id in a table
        file_activity <- file.path(getwd(), file_path)
        dt_file_activity <- read.table(file_activity)
        
        ## Load the activity labels in a table
        file_path="./Data/UCI HAR Dataset/activity_labels.txt"
        file_activity_label <- file.path(getwd(), file_path)
        dt_file_activity_label <- read.table(file_activity_label)
        
        dt_file_activity$label<-dt_file_activity_label$V2[match(dt_file_activity$V1,dt_file_activity_label$V1)]
        
        ## add the column
        dt_file_X2 <- cbind(dt_file_activity$label,dt_file_X2)
        names(dt_file_X2)[1]<-"activity"
        
        
        ## add a column "subject" that contains the id of the subject"
        if(test_training=="test"){
                file_path="./Data/UCI HAR Dataset/test/subject_test.txt"
        }else{
                file_path="./Data/UCI HAR Dataset/train/subject_train.txt"                
        }
        
        ## Load the subject.txt in a table
        file_subject <- file.path(getwd(), file_path)
        dt_file_subject <- read.table(file_subject)
        
        ## add the column
        dt_file_X2 <- cbind(dt_file_subject$V1,dt_file_X2)
        names(dt_file_X2)[1]<-"subject"
        
        
        
        dt_file_X2        
        
        
}



######################################################################################### 
##      Merges the training and the test sets to create one data set for 
##              - Triaxial acceleration from the accelerometer (total acceleration)
##              - Triaxial body acceleration.
##              - Triaxial Angular velocity from the gyroscope. 
##      Add a column to identify test or training data
##      Add a column to subject id
##      Add a column for the activity
##      Add a column to identify experiment id
##      Add a column to identify the axis (X, Y, Z)
#########################################################################################

process_triaxal_data <- function(test_training = "test",file_name, axisXYZ){
        
        if(test_training=="test"){
                file_path <- paste("./Data/UCI HAR Dataset/test/Inertial Signals/", file_name, sep="")
        }else{
                file_path <- paste("./Data/UCI HAR Dataset/train/Inertial Signals/", file_name, sep="")
        }
        
        ## Load the file in a table
        file_X <- file.path(getwd(), file_path)
        dt_file_X <- read.table(file_X)
        
        ## add the column names to dt_file_X
        column_names <- vector("character",128)
        for(i in 1:128){
                column_names[i]<-paste("reading", as.character(i), sep="")        
        }
        names(dt_file_X)<-  column_names
        
        
        ## add a column "axis" that contains either "X", "Y" or "Z"
        if(axisXYZ=="X"){
                axis_column <- rep("X",nrow(dt_file_X))
        }else if(axisXYZ=="Y"){
                axis_column <- rep("Y",nrow(dt_file_X))
        }else{
                axis_column <- rep("Z",nrow(dt_file_X))
        }
        
        dt_file_X <- cbind(axis_column,dt_file_X)
        names(dt_file_X)[1]<-"axis"
        
        
        ## add a column "settype" that contains either "test data" or "training data"
        if(test_training=="test"){
                test_training_column <- rep("test data",nrow(dt_file_X))
        }else{
                test_training_column <- rep("training data",nrow(dt_file_X))
        }
        
        dt_file_X <- cbind(test_training_column,dt_file_X)
        names(dt_file_X)[1]<-"settype"
        
        ## add a column "activity" that contains the label of the activity"
        if(test_training=="test"){
                file_path="./Data/UCI HAR Dataset/test/y_test.txt"
        }else{
                file_path="./Data/UCI HAR Dataset/train/y_train.txt"                
        }
        
        ## Load the activity id in a table
        file_activity <- file.path(getwd(), file_path)
        dt_file_activity <- read.table(file_activity)
        
        ## Load the activity labels in a table
        file_path="./Data/UCI HAR Dataset/activity_labels.txt"
        file_activity_label <- file.path(getwd(), file_path)
        dt_file_activity_label <- read.table(file_activity_label)
        
        dt_file_activity$label<-dt_file_activity_label$V2[match(dt_file_activity$V1,dt_file_activity_label$V1)]
        
        ## add the column
        dt_file_X <- cbind(dt_file_activity$label,dt_file_X)
        names(dt_file_X)[1]<-"activity"
        
        
        ## add a column "subject" that contains the id of the subject"
        if(test_training=="test"){
                file_path="./Data/UCI HAR Dataset/test/subject_test.txt"
        }else{
                file_path="./Data/UCI HAR Dataset/train/subject_train.txt"                
        }
        
        ## Load the subject.txt in a table
        file_subject <- file.path(getwd(), file_path)
        dt_file_subject <- read.table(file_subject)
        
        ## add the column
        dt_file_X <- cbind(dt_file_subject$V1,dt_file_X)
        names(dt_file_X)[1]<-"subject"
        
        ## add a column "experiment_identifier" that contains the id of the experiment"
        experiment_id <- seq(1,nrow(dt_file_X),1)
        
        ## add the column experimentId
        dt_file_X <- cbind(experiment_id,dt_file_X)
        names(dt_file_X)[1]<-"experimentId" 
        
        dt_file_X        
        
        
}

## Creates data directory if needed
if(!file.exists("data")){
        dir.create("data")
}

## dowload file and unzip
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile="./data/getdata_projectfiles_UCI HAR Dataset.zip")
outDir<-"./data"
unzip("./data/getdata_projectfiles_UCI HAR Dataset.zip",exdir=outDir)


## process the Triaxial body acceleration 
dt_body_acc_X_test <- process_triaxal_data(test_training = "test",file_name = "body_acc_x_test.txt", axisXYZ="X")
dt_body_acc_Y_test <- process_triaxal_data(test_training = "test",file_name = "body_acc_y_test.txt", axisXYZ="Y")
dt_body_acc_Z_test <- process_triaxal_data(test_training = "test",file_name = "body_acc_z_test.txt", axisXYZ="Z")

dt_body_acc_X_training <- process_triaxal_data(test_training = "training",file_name = "body_acc_x_train.txt", axisXYZ="X")
dt_body_acc_Y_training <- process_triaxal_data(test_training = "training",file_name = "body_acc_y_train.txt", axisXYZ="Y")
dt_body_acc_Z_traning <- process_triaxal_data(test_training = "training",file_name = "body_acc_z_train.txt", axisXYZ="Z")

dt_body_acc_final <- rbind(dt_body_acc_X_test,dt_body_acc_Y_test,dt_body_acc_Z_test,dt_body_acc_X_training,dt_body_acc_Y_training,dt_body_acc_Z_traning)

## write dt_body_acc_final into CSV file
write.table(dt_body_acc_final, file = "./Data/Tidy Dataset/triaxial_body_acceleration.csv", sep = ",", row.names=FALSE)


## process the Triaxial Angular velocity from the gyroscope. 
dt_body_gyro_X_test <- process_triaxal_data(test_training = "test",file_name = "body_gyro_x_test.txt", axisXYZ="X")
dt_body_gyro_Y_test <- process_triaxal_data(test_training = "test",file_name = "body_gyro_y_test.txt", axisXYZ="Y")
dt_body_gyro_Z_test <- process_triaxal_data(test_training = "test",file_name = "body_gyro_z_test.txt", axisXYZ="Z")

dt_body_gyro_X_training <- process_triaxal_data(test_training = "training",file_name = "body_gyro_x_train.txt", axisXYZ="X")
dt_body_gyro_Y_training <- process_triaxal_data(test_training = "training",file_name = "body_gyro_y_train.txt", axisXYZ="Y")
dt_body_gyro_Z_training <- process_triaxal_data(test_training = "training",file_name = "body_gyro_z_train.txt", axisXYZ="Z")

dt_body_gyro_final <- rbind(dt_body_gyro_X_test,dt_body_gyro_Y_test,dt_body_gyro_Z_test,dt_body_gyro_X_training,dt_body_gyro_Y_training,dt_body_gyro_Z_training)

## write dt_body_gyro_final into CSV file
write.table(dt_body_gyro_final, file = "./Data/Tidy Dataset/triaxial_body_gyro.csv", sep = ",", row.names=FALSE)



## process the Triaxial acceleration from the accelerometer (total acceleration)
dt_total_acc_X_test <- process_triaxal_data(test_training = "test",file_name = "total_acc_x_test.txt", axisXYZ="X")
dt_total_acc_Y_test <- process_triaxal_data(test_training = "test",file_name = "total_acc_y_test.txt", axisXYZ="Y")
dt_total_acc_Z_test <- process_triaxal_data(test_training = "test",file_name = "total_acc_z_test.txt", axisXYZ="Z")

dt_total_acc_X_training <- process_triaxal_data(test_training = "training",file_name = "total_acc_x_train.txt", axisXYZ="X")
dt_total_acc_Y_training <- process_triaxal_data(test_training = "training",file_name = "total_acc_y_train.txt", axisXYZ="Y")
dt_total_acc_Z_training <- process_triaxal_data(test_training = "training",file_name = "total_acc_z_train.txt", axisXYZ="Z")

dt_total_acc_final <- rbind(dt_total_acc_X_test,dt_total_acc_Y_test,dt_total_acc_Z_test,dt_total_acc_X_training,dt_total_acc_Y_training,dt_total_acc_Z_training)

## write dt_total_acc_final into CSV file
write.table(dt_total_acc_final, file = "./Data/Tidy Dataset/triaxial_total_acc.csv", sep = ",", row.names=FALSE)




## process the measurement table
dt_measurement_test <- process_measurement("test")
dt_measurement_training <- process_measurement("training")
dt_measurement<-rbind(dt_measurement_test,dt_measurement_training)
dt_measurement_mean<-dt_measurement

## add a column "experiment_identifier" that contains the id of the experiment"
experiment_id <- seq(1,nrow(dt_measurement),1)

## add the column experimentId
dt_measurement <- cbind(experiment_id,dt_measurement)
names(dt_measurement)[1]<-"experimentId"


## write dt_measurement into CSV file
write.table(dt_measurement, file = "./Data/Tidy Dataset/measurement.csv", sep = ",", row.names=FALSE)

## process a second, independent tidy data set with the average of each variable for each activity and each subject.
dt_measurement_mean <- melt(dt_measurement_mean,id=c("subject","activity","settype"),measure.vars=dt_features_mean_std$V2)
dt_measurement_mean <- data.table(dt_measurement_mean)
dt_measurement_mean2 <- dt_measurement_mean[,mean(value, na.rm = TRUE),by=list(subject,activity,settype,variable)]

names(dt_measurement_mean2)[5]<-"mean"


## write dt_measurement_mean2 into CSV file
write.table(dt_measurement_mean2, file = "./Data/Tidy Dataset/measurement_mean.csv", sep = ",", row.names=FALSE)


