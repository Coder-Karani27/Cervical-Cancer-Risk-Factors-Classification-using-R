#load the packages
options(warn=-1)
library('caret')
library('lattice')
library('ggplot2')
library('naniar')
library('ROSE')
library('rpart')

##########################################SVM LINEAR############################################
set.seed(3033)
intrain4<-createDataPartition(y=cervical_no_na$Schiller,p=0.75,list=FALSE)
training4<-cervical_no_na[intrain4,]
testing4<-cervical_no_na[-intrain4,]

dim(training4)
dim(testing4)

#check for class imbalance
table(training4$Schiller)
table(testing4$Schiller)

#fix the class imbalance by both oversampling and undersampling
training_balanced4 <- ovun.sample(Schiller ~ ., data = training4, method = "both", p=0.5, seed = 1)$data
testing_balanced4 <- ovun.sample(Schiller ~ ., data = testing4, method = "both", p=0.5, seed = 1)$data
table(training_balanced4$Schiller)
table(testing_balanced4$Schiller)

#Training the model 
trctrl<-trainControl(method="boot",number=12)
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1))

svm_Linear4<-train(Schiller~
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
                     Citology, data=training_balanced4, method="svmLinear", trControl=trctrl, tuneGrid= grid ,preProcess=c("center","scale"),tuneLength=15)
svm_Linear4

test_pred4<-predict(svm_Linear4,newdata=testing_balanced4)
test_pred4

confusionMatrix(table(test_pred4,testing_balanced4$Schiller,dnn = c("Prediction", "Reference")))
##############################################SVM RADIAL###########################################
trctrl<-trainControl(method="repeatedcv",number=12,repeats=4)
svm_Rad4<- train(Schiller~
                   Smokes+
                   STDs+
                   Num.of.pregnancies+
                   IUD+
                   Dx+
                   Hormonal.Contraceptives+
                   Hinselmann+
                   Biopsy+
                   Citology
                   #IUD..years.
                 , data = training_balanced4, method = "svmRadial",
                 trControl=trctrl,
                 preProcess = c("center","scale"),
                 tuneLength = 15)
svm_Rad4
plot(svm_Rad4)

test_pred_grid4 <- predict(svm_Rad4, newdata = testing_balanced4)
test_pred_grid4

confusionMatrix(table(test_pred_grid4, testing_balanced4$Schiller,dnn = c("Prediction", "Reference")))


####################################KNN MODEL##################################
model_knn4 <- train(Schiller~
                      Citology+
                      Hinselmann+
                      Biopsy+
                      Number.of.sexual.partners+
                      First.sexual.intercourse+
                      Num.of.pregnancies+
                      Smokes+
                      Hormonal.Contraceptives+
                      IUD+
                      STDs+
                      Dx, data = training_balanced4, method = "knn",preProcess = c("center","scale"), tuneLength=15)
predict_knn4 <- predict(model_knn4, newdata = testing_balanced4)
print(model_knn4)
# Measure Model Performance: Hold-Out Method
confusionMatrix(table(predict_knn4, testing_balanced4$Schiller,dnn = c("Prediction", "Reference")))

################################LOGISTIC REGRESSION##########################################

logistic_4= train(
  form = Schiller ~ Age+Smokes+STDs+Number.of.sexual.partners+Num.of.pregnancies+IUD+Dx+Hormonal.Contraceptives+Hinselmann+Biopsy+Citology,
  data = training_balanced4,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
logistic_4$results

test_pred_grid4 <- predict(logistic_4, newdata = testing_balanced4)
test_pred_grid4

confusionMatrix(table(test_pred_grid4, testing_balanced4$Schiller,dnn = c("Prediction", "Reference")))

########################################## RANDOM FOREST #################################
mtry <- sqrt(ncol(training_balanced4))

ntree <- 5
control <- trainControl(method='repeatedcv', 
                        number=12, 
                        repeats=3,
                        search = 'random')
set.seed(123)
#Number randomly variable selected is mtry
tunegrid <- expand.grid(.mtry = mtry)
rf4 <- train(Schiller~
               Citology+
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
             trControl=control)
print(rf4)
test_pred_grid4 <- predict(rf4, newdata = testing_balanced4)
test_pred_grid4

confusionMatrix(table(test_pred_grid4, testing_balanced4$Schiller,dnn = c("Prediction", "Reference")))

###################################GNB#############################
gnb4<-train(Schiller~
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
                     Citology, data=training_balanced4, method="nb", trControl=trctrl)
gnb4
test_pred_grid4 <- predict(gnb4, newdata = testing_balanced4)
test_pred_grid4
confusionMatrix(table(test_pred_grid4, testing_balanced4$Schiller,dnn = c("Prediction", "Reference")))

