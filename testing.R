load("rf_model_100_7.RData")

rf_model$importance

rf_model$importanceSD
importanceOrder=order(-rf_model$importance)
importanceOrder
names=rownames(rf_model$importance)[importanceOrder][1:20]


load("train.RData")
index_cat_bin
index_int

train_train=train_train[,index_int]
train_test=train_test[,index_int]
test_target=train_test[,1]

library(ROSE)
library(randomForest)

train_rose=ROSE(target~ ., data = train_train, seed = 1)$data

#Random forest model
rf_model=randomForest(target~.,data=train_rose,ntree=100,mtry=8)
pred_rose=predict(rf_model,train_test,type="prob")

save(rf_model,file="rf_model")
a=as.integer(test_target)
p=pred_rose[,2]
normalizedGini(a,p)




