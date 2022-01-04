options(warn=-1)
library('caret') 
library('lattice')
library('ggplot2')
library('naniar')
library('ROSE')
library('rpart')
library('randomForest')
######################################### LINEAR SVM ALGORITHM#################################
set.seed(3033)
intrain<-createDataPartition(y=cervical_no_na$Hinselmann,p=0.75,list=FALSE)
training<-cervical_no_na[intrain,]
testing<-cervical_no_na[-intrain,]

dim(training)
dim(testing)

#check for class imbalance
table(training$Hinselmann)
table(testing$Hinselmann)

#fix the class imbalance by both oversampling and undersampling
training_balanced <- ovun.sample(Hinselmann ~ ., data = training, method = "both", p=0.5, seed = 1)$data
testing_balanced <- ovun.sample(Hinselmann ~ ., data = testing, method = "both", p=0.5, seed = 1)$data
table(training_balanced$Hinselmann)
table(testing_balanced$Hinselmann)

#Training the model 
trctrl<-trainControl(method="repeatedcv",number=16,repeats=4)

#Tuning the C value to check for best fit
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1))
svm_Linear_Grid <- train(Hinselmann ~ 
                              Biopsy+
                              Schiller+
                              Citology+
                              STDs+
                              Smokes+
                              Hormonal.Contraceptives+
                              IUD+
                              Dx+
                              Hormonal.Contraceptives..years.+
                              IUD..years., data = training_balanced, method = "svmLinear",
                              trControl=trctrl,
                              preProcess = c("center", "scale"),
                              tuneGrid = grid,
                              tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing_balanced)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing_balanced$Hinselmann,dnn = c("Prediction", "Reference")))

##########################################SVMRADIAL ALGORITHM ###################################
trctrl<-trainControl(method="repeatedcv",number=16,repeats = 4)
svm_Rad<- train(Hinselmann ~
                  Num.of.pregnancies+
                  First.sexual.intercourse+
                  Biopsy+
                  Schiller+
                  Citology+
                  STDs+
                  Smokes+
                  Hormonal.Contraceptives+
                  IUD+
                  Dx+
                  Smokes..years.+ 
                  Hormonal.Contraceptives..years.+
                  IUD..years., data = training_balanced, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                      #tuneGrid = expand.grid(sigma = c(0.01, 0.015, 0.02, 0.025, 0.10, 0.2, 0.5), C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1)),
                         tuneLength = 12)
svm_Rad
plot(svm_Rad)

test_pred_grid <- predict(svm_Rad, newdata = testing_balanced)
test_pred_grid 

confusionMatrix(table(test_pred_grid, testing_balanced$Hinselmann),dnn = c("Prediction", "Reference"))

################################################KNN ALGORITHM#######################################################
model_knn <- train(Hinselmann ~ 
                     Biopsy+
                     Schiller+
                     Citology+
                     STDs+
                     Smokes+
                     Hormonal.Contraceptives+
                     IUD+
                     Dx+
                     Smokes..years.+ 
                     Hormonal.Contraceptives..years.+
                     IUD..years., data = training_balanced, method = "knn",preProcess = "pca", tuneLength=13)
model_knn
predict_knn <- predict(model_knn, newdata = testing_balanced)

confusionMatrix(table(predict_knn, testing_balanced$Hinselmann,dnn = c("Prediction", "Reference")))

#####################################################LOGISTIC REGRESSION############################################################
logistic_1= train(
  form = Hinselmann ~ Age+Smokes+STDs+Number.of.sexual.partners+Num.of.pregnancies+IUD+Dx+Hormonal.Contraceptives+Schiller+Biopsy+Citology,
  data = training_balanced,
  trControl = trainControl(method = "repeatedcv", number = 16,repeats = 4),
  method = "glm",
  family = "binomial"
)
logistic_1$results

test_pred_grid <- predict(logistic_1, newdata = testing_balanced)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing_balanced$Hinselmann,dnn = c("Prediction", "Reference")))

############################################### RANDOM FOREST ALGORITHM ##########################################
mtry <- sqrt(ncol(training_balanced))
ntree <- 5
control <- trainControl(method='repeatedcv', 
                        number=16, 
                        repeats=4,
                        search = 'random')
set.seed(123)
#Number randomly variable selected is mtry
tunegrid <- expand.grid(.mtry = mtry)
rf1 <- train(Hinselmann~
               Schiller+
               Biopsy+
               Citology+
            Number.of.sexual.partners+
             First.sexual.intercourse+
             Num.of.pregnancies+
             STDs..number.+
             Smokes+
             Hormonal.Contraceptives+
             IUD+
             STDs+
             Dx, 
                    data=cervical_no_na, 
                    method='rf', 
                    metric='Accuracy', 
                    #tuneGrid=tunegrid, 
                    trControl=control
            )
print(rf1)
plot(rf1)

test_pred_grid <- predict(rf1, newdata = testing_balanced)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing_balanced$Hinselmann,dnn = c("Prediction", "Reference")))
#############################################GNB################################################
gnb1<-train(Hinselmann~
               Age+
               Smokes+
               STDs+
               Number.of.sexual.partners+
               Num.of.pregnancies+
               IUD+
               Dx+
               Hormonal.Contraceptives+
               Schiller+
               Biopsy+
               Citology, data=training_balanced, method="nb", trControl=trctrl)
gnb1
test_pred_grid <- predict(gnb1, newdata = testing_balanced)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing_balanced$Hinselmann,dnn = c("Prediction", "Reference")))

################################################ROC#############################################

model <- train(Hinselmann ~
                 Num.of.pregnancies+
                 First.sexual.intercourse+
                 Biopsy+
                 Schiller+
                 Citology+
                 STDs+
                 Smokes+
                 Hormonal.Contraceptives+
                 IUD+
                 Dx+
                 Smokes..years.+ 
                 Hormonal.Contraceptives..years.+
                 IUD..years., data=training_balanced, method="lvq", preProcess="scale", trControl=trctrl)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)
