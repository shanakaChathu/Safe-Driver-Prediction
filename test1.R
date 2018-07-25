test=read.csv("test.csv")
str(test1)

train1=read.csv("train.csv")
str(train1)

test=test[,-c(1,25,26,27,29)]


cols_cat=colnames(((test[,grepl("cat",names(test))])))
cols_cat
cols_bin=colnames(((test[,grepl("bin",names(test))])))
cols_bin
cols_cat_bin=c(cols_cat,cols_bin)
cols_cat_bin

#Extracting indexex according to the type 
index_cat_bin=match(cols_cat_bin,names(test))
index_cat_bin

cols_int=colnames(test[,-index_cat_bin])
index_int=match(cols_int,names(test))

test[test==-1]=NA
str(test)

#Mode function 
Mode <- function(y) 
{
  ux <- unique(y)
  ux[which.max(tabulate(match(y, ux)))]
}

#Missing value analysis for numerical variables 
#index_int

#str(test)
#test

#Missing value analysis for  categorical and binary variables 
#index_cat_bin


repalceNAsWithMean = function(x) 
{
  replace(x, is.na(x), mean(x[!is.na(x)]))
}

test[,index_int]=repalceNAsWithMean(test[,index_int])

#--------------------------
repalceNAsWithMode = function(x) 
{
  
  replace(x,is.na(x),Mode(x[!is.na(x)]))
}


for (i in 1:length(index_cat_bin))
{
  v=index_cat_bin[i]
  test[,v]=repalceNAsWithMode(test[,v])
}




#Convert cat variables into the factor type from int type 
cols=colnames(((test[,grepl("cat",names(test))])))
cols
test[cols] =lapply(test[cols], factor)


#Convert bin variables into the factor type from int type 
cols1=colnames(((test[,grepl("bin",names(test))])))
cols1
test[cols1] =lapply(test[cols1], factor)
#target



#Converting some cat variables to the int variable 
test["ps_car_11_cat"]=as.integer(test[,"ps_car_11_cat"])
test["ps_car_01_cat"]=as.integer(test[,"ps_car_01_cat"])
test["ps_car_06_cat"]=as.integer(test[,"ps_car_06_cat"])

save(test,file="test.RData")


pred_test=predict(rf_model,test,type="prob")
target=pred_test[,2]
rf=data.frame(id,target)

write.csv(rf, file = "under_30k_2000_12.csv")

save(test,file="test.RData")

t=read.csv("test.csv")
id=t[,1]

load("test.RData")
