#load the packages
options(warn=-1)
library('caret')
library('lattice')
library('ggplot2')
library('naniar')
library('ROSE')
library('rpart')
###############################SVM LINEAR############################################################
set.seed(3033)
intrain2<-createDataPartition(y=cervical_no_na$Biopsy,p=0.75,list=FALSE)
training2<-cervical_no_na[intrain2,]
testing2<-cervical_no_na[-intrain2,]

dim(training2)
dim(testing2)

#check for class imbalance
table(training2$Biopsy)
table(testing2$Biopsy)

#fix the class imbalance by both oversampling and undersampling
training_balanced2 <- ovun.sample(Biopsy ~ ., data = training2, method = "both", p=0.5,N=950, seed = 1)$data
testing_balanced2 <- ovun.sample(Biopsy ~ ., data = testing2, method = "both", p=0.5,N=300, seed = 1)$data
table(training_balanced2$Biopsy)
table(testing_balanced2$Biopsy)

#Training the model 
trctrl<-trainControl(method="repeatedcv",number=12,repeats=3)
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear2<-train(Biopsy~
                     Age+
                     STDs+
                     Number.of.sexual.partners+
                     Num.of.pregnancies+
                     IUD+
                     Dx+
                     Hormonal.Contraceptives+
                    Hinselmann+
                    Citology+
                    Schiller, data=training_balanced2, method="svmLinear", tuneGrid=grid , trControl=trctrl, preProcess=c("center","scale"),tuneLength=10)
svm_Linear2
plot(svm_Linear2)

test_pred2<-predict(svm_Linear2,newdata=testing_balanced2)
test_pred2

confusionMatrix(table(test_pred2,testing_balanced2$Biopsy,dnn = c("Prediction", "Reference")))

#########################################SVMRADIAL#############################################
svm_Rad2<- train(Biopsy~
                  Age+
                  Smokes+
                  Smokes..years.+
                  STDs+
                  Num.of.pregnancies+
                  IUD+
                  Dx+
                  Hormonal.Contraceptives+
                  Hormonal.Contraceptives..years.+
                  Hinselmann+
                  Citology+
                  Schiller, data = training_balanced2, method = "svmRadial",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 8)
svm_Rad2
plot(svm_Rad2)

test_pred_grid2 <- predict(svm_Rad2, newdata = testing_balanced2)
test_pred_grid2 

confusionMatrix(table(test_pred_grid2, testing_balanced2$Biopsy,dnn = c("Prediction", "Reference")))

############################################KNN ALGORITHM############################################
model_knn2 <- train(Biopsy~
                      Age+
                      Smokes+
                      Smokes..years.+
                      STDs+
                      Num.of.pregnancies+
                      IUD+
                      Dx+
                      Hormonal.Contraceptives+
                      Hormonal.Contraceptives..years.+
                      Hinselmann+
                      Citology+
                      Schiller, data = training_balanced2, method = "knn",preProcess = c("center","scale"), tuneLength=13)
model_knn2
predict_knn2 <- predict(model_knn2, newdata = testing_balanced2)

confusionMatrix(table(predict_knn2, testing_balanced2$Biopsy,dnn = c("Prediction", "Reference")))
#####################################################LOGISTIC REGRESSION############################################################
logistic_2= train(
  form = Biopsy ~ IUD..years. + Smokes..years. +Age+Smokes+STDs+Number.of.sexual.partners+Num.of.pregnancies+IUD+Dx+Hormonal.Contraceptives+Schiller+Hinselmann+Citology,
  data = training_balanced,
  trControl = trainControl(method = "repeatedcv", number = 12,repeats = 3),
  method = "glm",
  family = "binomial"
)
logistic_2$results

test_pred_grid2 <- predict(logistic_2, newdata = testing_balanced2)
test_pred_grid2

confusionMatrix(table(test_pred_grid2, testing_balanced2$Biopsy,dnn = c("Prediction", "Reference")))

########################################## RANDOM FOREST #################################
mtry <- sqrt(ncol(training_balanced2))
#ntree: Number of trees to grow.
ntree <- 5
control <- trainControl(method='repeatedcv', 
                        number=12, 
                        repeats=3,
                        search = 'random')
set.seed(123)
#Number randomly variable selected is mtry
tunegrid <- expand.grid(.mtry = mtry)
rf2 <- train(Biopsy~
               Schiller+
               Hinselmann+
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
print(rf2)
plot(rf2)

test_pred_grid2 <- predict(rf2, newdata = testing_balanced2)
test_pred_grid2

confusionMatrix(table(test_pred_grid2, testing_balanced2$Biopsy,dnn = c("Prediction", "Reference")))

##############################################GNB########################################
gnb2<-train(Biopsy~
              Age+
              Smokes+
              STDs+
              Number.of.sexual.partners+
              Num.of.pregnancies+
              IUD+
              Dx+
              Hormonal.Contraceptives+
              Schiller+
              Hinselmann+
              Citology, data=training_balanced2, method="nb", trControl=trctrl)
gnb2
test_pred_grid2 <- predict(gnb2, newdata = testing_balanced2)
test_pred_grid2
confusionMatrix(table(test_pred_grid2, testing_balanced2$Biopsy,dnn = c("Prediction", "Reference")))









model <- train(Biopsy ~ IUD..years. + Smokes..years. +Age+Smokes+STDs+Number.of.sexual.partners+Num.of.pregnancies+IUD+Dx+Hormonal.Contraceptives+Schiller+Hinselmann+Citology, data=training_balanced, method="lvq", preProcess="scale", trControl=trctrl)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

