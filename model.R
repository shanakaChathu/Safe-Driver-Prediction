load("train.RData")

#Dividing Sample 
indexes=sample(1:nrow(train),size=0.6*nrow(train))
train_train=train[indexes,]
train_test=train[-indexes,]

#Fitting decision tree
library(rpart) 
test_target=train_test[,1]
train_test=train_test[,2:54]
train_tree=rpart(target~.,minsplit=1,cp=0,data = train_train)

#Predict class
pred=predict(train_tree,train_test,type="class")

#Evaluate
confusion.matrix(pred,test_target)


#Predict Probability
pred=predict(train_tree,train_test,type="prob")

#Evaluate using giniIndex
a=as.integer(test_target)
p=pred[,2]
normalizedGini(b,a)


#SMOTE tha sample 
train_smote=SMOTE(target~.,data=train_train,perc.over =100,k=5,perc.under = 200)

#Fitting model for smote sample
smote_tree=rpart(target~.,minsplit=1,cp=0,data = train_smote)
pred_smote=predict(smote_tree,train_test,type="class")

#Evaluate
q=confusion.matrix(pred_smote,test_target)

#Predict Probability
pred_smote=predict(train_tree,train_test,type="prob")

#Evaluate using giniIndex
a=as.integer(test_target)
p=pred_smote[,2]
normalizedGini(b,a)




#Randomforest

load("train.RData")

indexes=sample(1:nrow(train),size=0.6*nrow(train))
train_train=train[indexes,]
train_test=train[-indexes,]

test_target=train_test[,1]
train_test=train_test[,2:54]

library(randomForest)
library(ROSE)
#train_smote=SMOTE(target~.,data=train_train,perc.over =200,k=5,perc.under = 100)
train_smote=ROSE(target~ ., data = train_train, seed = 1)$data


#dt
smote_tree=rpart(target~.,minsplit=1,cp=0,data = train_smote)
pred_smote=predict(smote_tree,train_test,type="prob")
p=pred_smote[,2]
normalizedGini(a,p)

#randomforest
rf_model=randomForest(target~.,data=train_smote,ntree=300,mtry=8)
pred_smote=predict(rf_model,train_test,type="prob")

save(rf_model,file="rf_model")

a=as.integer(test_target)
p=pred_smote[,2]
normalizedGini(a,p)


roc.curve(test_target, pred_smote[,2])
confusion.matrix(pred_smote,test_target)
 
