load("train.RData")
str(train)
attach(train)

library(randomForest)
library(ROSE)

#Dividing Sample 
indexes=sample(1:nrow(train1),size=0.6*nrow(train1))
train_train=train1[indexes,]
train_test=train1[-indexes,]
test_target=train_test[,1]


#Resamplong using rose 

train_rose=ROSE(target~ ., data = train_train, seed = 1)$data

#Undersampling
data_rose= ovun.sample(target ~ ., data = train_train, method = "under", N = 40000,
                                 seed = 1)$data

#Random forest model
rf_model1=randomForest(target~.,data=data_rose,ntree=2500,mtry=15)
pred_rose=predict(rf_model1,train_test,type="prob")

save(rf_model,file="rf_model")
a=as.integer(test_target)
p=pred_rose[,2]
normalizedGini(a,p)


save(rf_model,file="rf_model_100_7.RData")

save(rf_model,file="rf_model_under.RData")





data_rose= ovun.sample(target ~ ., data = train_train, method = "under", N = 30000,
                       ,seed = 1)$data
gini=c()
tree=c(100:2000)


for(i in 1:length(tree))
{
     rf_model=randomForest(target~.,data=data_rose,ntree=tree[i],mtry=12)
     pred_rose=predict(rf_model,train_test,type="prob")
     a=as.integer(test_target)
     p=pred_rose[,2]
     g=normalizedGini(a,p)  
     gini=c(gini,g)
}

df1=data.frame(tree,gini)
df1

gini



data_balanced_under= ovun.sample(target ~ ., data = train, method = "under", N = 21694, 
                                 seed = 1)$data




str(train[train$target==1,])


