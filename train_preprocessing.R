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

#Removing variables
train=train[,-c(1,26,28)]

#Extracting categorical,binary and numerical coloumns
cols_cat=colnames(((train[,grepl("cat",names(train))])))
cols_cat
cols_bin=colnames(((train[,grepl("bin",names(train))])))
cols_bin
cols_cat_bin=c(cols_cat,cols_bin)
cols_cat_bin


#Extracting indexex according to the type 
index_cat_bin=match(cols_cat_bin,names(train))
index_cat_bin

cols_int=colnames(train[,-index_cat_bin])
index_int=match(cols_int,names(train))
index_int

save(index_cat_bin,index_int,file="index_int_cat")

#Missing value analysis 
# -1 codes as missing value (NA)
train[train==-1]=NA
str(train)

#Replace mean for continuous variables 
repalceNAsWithMean = function(x) 
{
  replace(x, is.na(x), mean(x[!is.na(x)]))
}

train[,index_int]=repalceNAsWithMean(train[,index_int])


#Mode function 
Mode <- function(y) 
{
  ux <- unique(y)
  ux[which.max(tabulate(match(y, ux)))]
}

repalceNAsWithMode = function(x) 
{
  
  replace(x,is.na(x),Mode(x[!is.na(x)]))
}


for (i in 1:length(index_cat_bin))
{
  v=index_cat_bin[i]
  train[,v]=repalceNAsWithMode(train[,v])
}

index_cat_bin

#Checking missing value available in the set 

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

#Convert cat variables into the factor type from int type 
cols=colnames(((train[,grepl("cat",names(train))])))
cols
train[cols] =lapply(train[cols], factor)


#Convert bin variables into the factor type from int type 
cols1=colnames(((train[,grepl("bin",names(train))])))
cols1
train[cols1] =lapply(train[cols1], factor)

#convert target variable from into factor 
train[1]=lapply(train[1],factor)

#Converting some cat variables to the int variable 
train["ps_car_11_cat"]=as.integer(train[,"ps_car_11_cat"])
train["ps_car_01_cat"]=as.integer(train[,"ps_car_01_cat"])
train["ps_car_06_cat"]=as.integer(train[,"ps_car_06_cat"])
str(train)

train["ps_car_11_cat"]=as.factor(train[,"ps_car_11_cat"])
train["ps_car_01_cat"]=as.factor(train[,"ps_car_01_cat"])
train["ps_car_06_cat"]=as.factor(train[,"ps_car_06_cat"])


#save the train object 

save(train,file="train.RData")
