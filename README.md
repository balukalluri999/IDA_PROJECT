# About the Project
## The Project is Classification of Breast cancer data using Decision Tree algorithm
The classifier model was build using the ID3 algorithm.
### Walk through of the R file
- Install and import the necessary libraries
- Change the path to the folder where the dataset is located.
- Be careful in giving the path : R allows "/" instead of "\" in path of the file.
- read.csv is method that loads the csv file to dataframe in R.
- Clean the data given.
    1. Removing the nan values.
    2. Removing the outliers
    3. Remove the coloumn which are highly correlated.
- Spliting the dataset into training and test set with the ratio of 2:1
- Build the model using the ID3 algorithm 
- Train the model with this train set
- Draw the desicion tree
- Predicted the output for the test set data
- Calculated the F1 score, Accuracy, Precision and Reccall.
- Finally increased the accuracy using the k-fold validation and plotted the ROC curve 
### Files in the current folder
- "IDA_Project.R" has the R code for the complete project.
- "Project_G12.pdf" is the report of the project which briefly explain the problem statement, implementation of code and results acquired.
- "Breastcancer.csv" has the dataset for the problem. 
# IDA_PROJECT
