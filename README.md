# Getting-and-Cleaning-Data-Course-Project

The repository contains the following items:
        - run_analysis.R
                - This script performs the following tasks
                        - create a sub-directory called "data" in the default directory
                        - download the dataset from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones in the "data"" directory
                        - unzip the dataset
                        - process and produce 2 tidy datasets in the "data/tidy dataset" directory
                                - Tidy dataset #1
                                        - triaxial_body_acceleration.csv
                                                - this file contains triaxial estimated body acceleration
                                        - triaxial_body_gyro.csv
                                                - this file contains triaxial Angular velocity from the gyroscope. 
                                        - triaxial_total_acc.csv
                                                - this file contains triaxial acceleration from the accelerometer
                                        - measurement.csv
                                                - this file contains the 561-feature vectors with time and frequency domain variables. 
                                - Tidy dataset #2
                                        - measurement_mean.csv
                                                - this file contains the average of each measurement variable for each activity and each subject
        - the codebook