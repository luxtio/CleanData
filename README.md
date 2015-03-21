# CleanData
CleanData peer assignment

The run_analysis.R script requires to run at the same directory of "UCI HAR Dataset" direcotyr
You must call mergeData function in order to obtain de result Data.Table with the desired mean.
Example:
      myResult <- mergeData()

Required library: 
- data.table
- dplyr

Columns name on the result Data.Table are the same of the original columns plus the first and the second that are:
- "Subject"  : id of subjetc that run the test
- "Activity" : the label of the corresponding id activity
