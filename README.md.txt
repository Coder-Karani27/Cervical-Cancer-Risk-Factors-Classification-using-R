### AUTHOR : Hamza Karani, Vishwakarma Inst of Technology, Pune

This classification project was performed using R language and RStudio. 
The dataset used is "Cervical Cancer Risk Factors Dataset" from UCI Repository. 
The dataset consists of 858 observations (rows) and 36 attributes (columns).The .csv file is included in the repository.
The dataset consists of missing values and is uncleaned. 
I have cleaned the data and ridden of NA values 
Data Cleaning and preprocessing is the first step and is in the file Preprocessing.R
2 attributes "Time since first diagnosis" and "Time since last diagnosis" have been dropped because 92% values are missing.
The target variables are Hinselmann, Schiller, Biopsy and Citology. These are clinical tests to find out whether patient suffers from Cervical cancer or not. 
I have applied 6 Supervised algorithms on each of the target variables, hence a total of 24 algorithms are applied.
1 - SVM Linear Kernel
2 - SVM Radial Kernel
3 - KNN
4 - Random Forest
5 - Logistic Regression 
6 - Gaussian Naive Bayes
Please note that the variables chosen to train each of the model varies, the preprocessing techniques as well as the train test split ratio varies. Only the best fit models are considered. Best fit model is chosen in terms of Sensitivity and Specificity of TESTING DATA and not on Accuracy.
###################################
The order of execution of files :
1 - Preprocessing.R
2 - Hinselmann.R
3 - Biopsy.R
4 - Citology.R
5 - Schiller.R
###################################