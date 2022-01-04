#load the packages
options(warn=-1)
library('caret')
library('lattice')
library('ggplot2')
library('naniar')
library('ROSE')
library('rpart')
##########################################IMPORTANCE###################
model <- train(Citology~
                 Schiller+
                 Hinselmann+
                 Biopsy+
                 Number.of.sexual.partners+
                 First.sexual.intercourse+
                 Num.of.pregnancies+
                 Smokes+
                 Hormonal.Contraceptives+
                 IUD+IUD..years.+Smokes..years.+
                 STDs+
                 Dx, data=training_balanced3, method="lvq", preProcess="scale", trControl=trctrl)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)
################################################SVM LINEAR#########################################
set.seed(3033)
intrain3<-createDataPartition(y=cervical_no_na$Citology,p=0.8,list=FALSE)
training3<-cervical_no_na[intrain3,]
testing3<-cervical_no_na[-intrain3,]

dim(training3)
dim(testing3)

#check for class imbalance
table(training3$Citology)
table(testing3$Citology)

#fix the class imbalance by both oversampling and undersampling
training_balanced3 <- ovun.sample(Citology ~ ., data = training3, method = "both", p=0.5, seed = 1)$data
testing_balanced3 <- ovun.sample(Citology ~ ., data = testing3, method = "both", p=0.5, seed = 1)$data
table(training_balanced3$Citology)
table(testing_balanced3$Citology)

#Training the model 
trctrl<-trainControl(method="repeatedcv",number=16,repeats=4)

svm_Linear3<-train(Citology~
                     Age+
                     STDs+
                     Number.of.sexual.partners+
                     Num.of.pregnancies+
                     IUD+
                     Dx+
                     Hormonal.Contraceptives+
                     Hinselmann+
                     Biopsy+
                     Schiller, data=training_balanced3, method="svmLinear", tuneGrid = grid,trControl=trctrl, preProcess=c("center","scale"),tuneLength=15)
svm_Linear3

test_pred3<-predict(svm_Linear3,newdata=testing_balanced3)
test_pred3

confusionMatrix(table(test_pred3,testing_balanced3$Citology,dnn = c("Prediction", "Reference")))

################################## svm radial algorithm ################################
svm_Rad3<- train(Citology~
                   Biopsy+
                   Schiller+
                   Hinselmann+
                   STDs+
                   Smokes+
                   Hormonal.Contraceptives+
                   IUD+
                   Dx+
                   Smokes..years.+ 
                   Hormonal.Contraceptives..years.+
                   IUD..years., data = training_balanced3, method = "svmRadial",
                 trControl=trctrl,
                 preProcess = "pca",
                 tuneLength = 12)
svm_Rad3
plot(svm_Rad3)

test_pred_grid3 <- predict(svm_Rad3, newdata = testing_balanced3)
test_pred_grid3

confusionMatrix(table(test_pred_grid3, testing_balanced3$Citology,dnn = c("Prediction", "Reference")))

#######################################KNN ALGORITHM#########################################
model_knn3 <- train(Citology~
                      Schiller+
                      Hinselmann+
                      Biopsy+
                      Number.of.sexual.partners+
                      First.sexual.intercourse+
                      Num.of.pregnancies+
                      Smokes+
                      Hormonal.Contraceptives+
                      IUD+
                      STDs+
                      Dx,  data = training_balanced3, method = "knn",preProcess = "pca", tuneLength=15)
model_knn3
predict_knn3 <- predict(model_knn3, newdata = testing_balanced3)

confusionMatrix(table(predict_knn3, testing_balanced3$Citology,dnn = c("Prediction", "Reference")))

################################LOGISTIC REGRESSION##########################################

logistic_3= train(
  form = Citology ~ Age+Smokes+STDs+Number.of.sexual.partners+Num.of.pregnancies+IUD+Dx+Hormonal.Contraceptives+Hinselmann+Biopsy+Schiller,
  data = training_balanced3,
  trControl = trainControl(method = "cv", number = 6),
  method = "glm",
  family = "binomial"
)
logistic_3$results

test_pred_grid3<- predict(logistic_3, newdata = testing_balanced3)
test_pred_grid3

confusionMatrix(table(test_pred_grid3, testing_balanced3$Citology,dnn = c("Prediction", "Reference")))
########################################## RANDOM FOREST #################################
mtry <- sqrt(ncol(training_balanced3))
ntree <- 7
control <- trainControl(method='repeatedcv', 
                        number=12, 
                        repeats=3,
                        search = 'random')
set.seed(123)
#Number randomly variable selected is mtry
tunegrid <- expand.grid(.mtry = mtry)
rf3 <- train(Citology~
               Schiller+
               Hinselmann+
               Biopsy+
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
             tuneGrid=tunegrid, 
             trControl=control
)
print(rf3)
plot(rf3)

test_pred_grid3 <- predict(rf3, newdata = testing_balanced3)
test_pred_grid3

confusionMatrix(table(test_pred_grid3, testing_balanced3$Citology,dnn = c("Prediction", "Reference")))

####################################GNB###############################################
gnb3<-train(Citology~
               Age+
               Smokes+
               STDs+
               Number.of.sexual.partners+
               Num.of.pregnancies+
               IUD+
               Dx+
               Hormonal.Contraceptives+
               Hinselmann+
               Biopsy+
               Schiller, data=training_balanced3, method="nb", trControl=trctrl)
gnb3
test_pred_grid3 <- predict(gnb3, newdata = testing_balanced3)
test_pred_grid3
confusionMatrix(table(test_pred_grid3, testing_balanced3$Citology,dnn = c("Prediction", "Reference")))

