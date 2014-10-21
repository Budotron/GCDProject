GCDProject
==========

The enclosed R script, run_analysis.R, tidies the [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) for further downstream analysis. It outputs a $180\times 69$ data set, which contains the average of each measurement collected from the accelerometers from the Samsung Galaxy S smartphone worn on the waist of the study's participants, for each activity and each subject. 

The transformations applied to the raw data (in order) are as follows

1. Merging 2947 testing data points with 7352 training data points
2. Adding the feature names to the concatenated data set
3. Subsetting the resulting data set to acquire the mean and standard deviation of measurements for each subject and activity identifier
4. Renaming the features to make them more descriptive
5. Averaging the values of each feature

The resulting data set is exported as a .txt file. 
