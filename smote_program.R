over=seq(100,500,by=100)
over
under=seq(100,500,by=100)
under
train

#Dividing Sample 
indexes=sample(1:nrow(train),size=0.6*nrow(train))
train_train=train[indexes,]
train_test=train[-indexes,]

test_target=train_test[,1]
train_test=train_test[,2:54]
a=as.integer(test_target)

gini=c()
vecTT=c()
vecFF=c()
vecTF=c()
vecFT=c()
for (i in 1:length(over))
{
   for ( j in 1:length(under))
   {
     #Smoting
     train_smote=SMOTE(target~.,data=train_train,perc.over =i,k=5,perc.under = j)
     
     #Fitting model for smote sample
     smote_tree=rpart(target~.,minsplit=1,cp=0,data = train_smote)
     
     #Evaluate usinng Confusion matrix
     pred_smote_class=predict(smote_tree,train_test,type="class")
     cm=confusion.matrix(pred_smote_class,test_target)
     TT=cm[2,2]/(cm[2,1]+cm[2,2])
     TF=cm[2,1]/(cm[2,1]+cm[2,2])
     FF=cm[1,1]/(cm[1,1]+cm[1,2])
     FT=cm[1,2]/(cm[1,1]+cm[1,2])
     
     vecTT=c(vecTT,TT)
     vecFF=c(vecFF,FF)
     vecTF=c(vecTF,TF)
     vecFT=c(vecFT,FT)
     
     #Evaluate using giniIndex
     pred_smote=predict(smote_tree,train_test,type="prob")
     p=pred_smote[,2]
     g=normalizedGini(a,p)
     gini=c(gini,g)
     
   }
}

