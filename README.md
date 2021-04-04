# Implementation Instruction of QVT

[1]	Download QVT.R , then open it using free software R (https://cran.r-project.org/)  or R Studio (https://www.rstudio.com/products/rstudio/download/).  
[2]	Lines 3-7: show the required R packages.   If these packages have been downloaded before, run lines 3-7 directly, or install the missing packages using install.packages(“XX”) function.  
[3]	Lines 10-29: the function of computing macro-F, which works as the performance metric.   The other performance metric AUC will be obtained using package ROCR.  
[4]	Line 32: read in the data. Here the read.delim() function is used as an example.  
[5]	Lines 35-44: the data preprocessing, including making the small-sample-size data.   Here 50 samples are extracted from each class randomly. The value can be changed.  
[6]	Lines 47-52: make the training data and testing data.   The training data are used to build learning model, and the testing data are used to evaluate the performance of learning model.  
[7]	Lines 55-57: perform the data normalization.  
[8]	Lines 60-168: the implementation of QVT.  
[9]	Line 170: output the result of feature selection.   The num is the number of column of variable in the original data, score is the importance score.  
[10]	Lines 173-210: using classifier svm to build the learning model, and test the performance.   The numOfvari is the used number of top features in building the learning model.    QVT(Lasso)_SVM_F is the value of macro_F, QVT(Lasso)_SVM_AUC is the value of AUC.  

