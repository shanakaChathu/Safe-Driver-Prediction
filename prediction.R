load("test.RData")
t=read.csv("test.csv")
id=t[,1]

pred_test=predict(rf_model,test,type="prob")
target=pred_test[,2]


rf=data.frame(id,target)
write.csv(rf, file = "target.csv")
str(test)
