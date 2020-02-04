#loading the dataset
heart<-read.csv('C:/Users/91977/Desktop/heart.csv')
summary(heart)
#structure of the data
str(heart)
head(heart)

#install the carat package
install.packages('caret')
library('caret')
#for splitting the data
intrain<-createDataPartition(y=heart$target,p=0.7,list=FALSE)
heart_training<-heart[intrain,]
heart_testing<-heart[-intrain,]

#checking for the structure
dim(heart_training)
dim(heart_testing)

#checking for the missing values
anyNA(heart)

summary(heart)

#converting this target variable in factor
heart$target<-as.factor(heart$target)
heart$target
heart_training$target<-as.factor(heart_training$target)

#all to train the  model
trctrl<-trainControl(method = 'repeatedcv',number = 10,repeats = 3)
trctrl

svm_Linear <- train(target ~., data = heart_training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear

#ready to predict 

pred<-predict(svm_Linear,heart_testing)
pred
confusionMatrix(table(pred,heart_testing$target))
