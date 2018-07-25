#Data loading 
test=read.csv("test.csv")
str(test)
attach(test)

#Removing variables
test=test[,-c(1,25,27)]

#Extracting categorical,binary and numerical coloumns
cols_cat=colnames(((test[,grepl("cat",names(test))])))
cols_cat
cols_bin=colnames(((test[,grepl("bin",names(test))])))
cols_bin
cols_cat_bin=c(cols_cat,cols_bin)
cols_cat_bin


#Extracting indexes according to the type 
index_cat_bin=match(cols_cat_bin,names(test))
index_cat_bin

cols_int=colnames(test[,-index_cat_bin])
index_int=match(cols_int,names(test))
index_int

#Missing value analysis 
# -1 codes as missing value (NA)
test[test==-1]=NA
str(test)

#Replace mean for continuous variables 
repalceNAsWithMean = function(x) 
{
  replace(x, is.na(x), mean(x[!is.na(x)]))
}

test[,index_int]=repalceNAsWithMean(test[,index_int])


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
  test[,v]=repalceNAsWithMode(test[,v])
}


#Checking missing value available in the set 

vec=c()
for(var in 1:ncol(test) )
{
  x=test[,var]
  len=length(x[is.na(x)])
  perc=round((len/nrow(test))*100,digits = 10)
  vec=c(vec,perc)
}

df=data.frame(colnames(test),vec)
df

#Convert cat variables into the factor type from int type 
cols=colnames(((test[,grepl("cat",names(test))])))
cols
test[cols] =lapply(test[cols], factor)


#Convert bin variables into the factor type from int type 
cols1=colnames(((test[,grepl("bin",names(test))])))
cols1
test[cols1] =lapply(test[cols1], factor)


#Converting some cat variables to the int variable 
test["ps_car_11_cat"]=as.integer(test[,"ps_car_11_cat"])
test["ps_car_01_cat"]=as.integer(test[,"ps_car_01_cat"])
test["ps_car_06_cat"]=as.integer(test[,"ps_car_06_cat"])
str(test)

train["ps_car_11_cat"]=as.factor(train[,"ps_car_11_cat"])
train["ps_car_01_cat"]=as.factor(train[,"ps_car_01_cat"])
train["ps_car_06_cat"]=as.factor(train[,"ps_car_06_cat"])

#Save test object 
save(test,file='test.RData')






