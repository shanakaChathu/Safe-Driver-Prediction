
#Data loading 
train=read.csv("train.csv")
str(train)
attach(train)

#Check the proportions of 1 and 0
len1=length(target[target==1])
len1
len0=length(target[target==0])
len0
perc1 = (len1/length(target))*100
perc0 =(len0/length(target))*100
perc0
perc1

#Extracting categorical,binary and numerical coloumns
cols_cat=colnames(((train[,grepl("cat",names(train))])))
cols_cat
cols_bin=colnames(((train[,grepl("bin",names(train))])))
cols_bin
cols_cat_bin=c(cols_cat,cols_bin)



#Extracting indexex according to the type 
index_cat_bin=match(cols_cat_bin,names(train))
index_cat_bin

cols_int=colnames(train[,-index_cat_bin])
index_int=match(cols_int,names(train))

#Missing value analysis- Proportion of missing value in each variable 
vec=c()
for(var in 1:ncol(train) )
{
         x=train[,var]
         len=length(x[is.na(x)])
         perc=round((len/nrow(train))*100,digits = 10)
         vec=c(vec,perc)
}

df=data.frame(colnames(train),vec)
df

# -1 codes as missing value (NA)
train[train==-1]=NA
str(train)

#Mode function 
Mode <- function(y) 
{
  ux <- unique(y)
  ux[which.max(tabulate(match(y, ux)))]
}

#Missing value analysis for numerical variables 
index_int
for (var in 1:length(index_int))
{     
  for(j in 1:nrow(train))
 {
   if(is.na(train[j,index_int[var]])) 
   {
    train[j,index_int[var]]=median(train[,index_int[var]],na.rm = TRUE) 
   }
 }  
}
str(train)
train

#Missing value analysis for  categorical and binary variables 
index_cat_bin
for (var in 1:length(index_cat_bin))
{     
  for(j in 1:nrow(train))
  {
    if(is.na(train[j,index_cat_bin[var]])) 
    {
      train[j,index_cat_bin[var]]=Mode(train[,index_cat_bin[var]]) 
    }
  }  
}
str(train)
train

#Convert cat variables into the factor type from int type 
cols=colnames(((train[,grepl("cat",names(train))])))
cols
train[cols] =lapply(train[cols], factor)


#Convert bin variables into the factor type from int type 
cols1=colnames(((train[,grepl("bin",names(train))])))
cols1
train[cols1] =lapply(train[cols1], factor)
#target
train[1]=lapply(train[1],factor)

#Converting some cat variables to the int variable 
train["ps_car_11_cat"]=as.integer(train[,"ps_car_11_cat"])
train["ps_car_01_cat"]=as.integer(train[,"ps_car_01_cat"])
train["ps_car_06_cat"]=as.integer(train[,"ps_car_06_cat"])
str(train)

train=train[,c(1:59)]
str(train)

#Extracting diffent grouping
ind=train[,grepl("ind",names(train))]
str(ind)
reg=train[,grepl("reg",names(train))]
str(reg)
car=train[,grepl("car",names(train))]
str(car)
calc=train[,grepl("calc",names(train))]
str(calc)

str(train)

install.packages("DMwR")


#Removing unwanted variabels
str(train)
train=train[,-c(1,26,28)]
save(train,file="train.RData")


#ROC curve thing 
library(pROC)
auc = roc(test_target,pred[,2])
print(auc)


plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)




smote_tree$cptable

opt=which.min(smote_tree$cptable[,"xerror"])
opt
cp=smote_tree$cptable[opt,"CP"]
cp

smote_tree_pruned=prune(smote_tree,cp=cp)
pred=predict(smote_tree_pruned,train_test,type="class")
pred
confusion.matrix(pred,test_target)


a=as.integer(test_target)
p=pred[,2]

normalizedGini(a,p)


2*((precision*recall)/(precision+recall))