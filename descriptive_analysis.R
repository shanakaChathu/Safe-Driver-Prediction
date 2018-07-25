str(train)
head(train)

l0=train[train$target==0,]
l1=train[train$target==1,]


#Numerical variable analysis 



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
cols_int=cols_int[-1]
index_int=match(cols_int,names(train))


#Integer coloumn comparision 
l0=train[train$target==0,index_int]
str(l0)
l1=train[train$target==1,index_int]
str(l1)

vec=cols_int
vec0=c()
vec1=c()

for (i in 1:length(index_int))

{
     z0=mean(l0[,i]) 
     z1=mean(l1[,i])
     vec1=c(vec1,z1)
     vec0=c(vec0,z0)
     
}

df_int=data.frame(vec,vec0,vec1)
df_int

#Categorical coloumn comparision 

le0=train[train$target==0,index_cat_bin]
str(le0)
le1=train[train$target==1,index_cat_bin]
str(le1)

vect=cols_cat_bin
vect0=c()
vect1=c()

Mode <- function(y) 
{
  ux <- unique(y)
  ux[which.max(tabulate(match(y, ux)))]
}

for (i in 1:length(index_cat_bin))
  
{
  z0=Mode(le0[,i]) 
  z1=Mode(le1[,i])
  vect1=c(vect1,z1)
  vect0=c(vect0,z0)
  
}

df_cat=data.frame(vect,vect0,vect1)
df_cat







