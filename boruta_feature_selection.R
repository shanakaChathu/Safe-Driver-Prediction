library(Boruta)

#Boruto for original training set
boruta.train = Boruta(target~., data = train_train, doTrace = 2,holdHistory=FALSE)
print(boruta.train)

#Boruta for sampled dataset
train_smote=ROSE(target~ ., data = train_train, seed = 1)$data
boruta.train = Boruta(target~., data = train_smote, doTrace = 2,holdHistory=FALSE)
print(boruta.train)
?Boruta


load("rf_model_100_7.RData")
importanceOrder=order(-rf_model$importance)
importanceOrder


rf_model$terms

imp_names=rownames(rf_model$importance)[importanceOrder]
imp_names

imp_index=match(imp_names,names(train))
imp_index
str(train)

load("train.RData")
imp_index1=imp_index
str(train)

nrow(train)

train1=train[,c(1,imp_index)]












