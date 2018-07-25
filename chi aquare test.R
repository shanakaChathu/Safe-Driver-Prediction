str(train)

tbl=table(train$target,train$ps_ind_02_cat)
tbl
chisq.test(tbl) 


index_cat_bin
vec=c()

for(i in 1:length(index_cat_bin))
{
        z=index_cat_bin[i]
        v=train[,z]
        tbl=table(train$target,v)
        q=chisq.test(tbl)
        p=q$p.value
        
        if(p<0.05)
        {
             a="Dependent" 
             vec=c(vec,a)
        }
        else
        {
            a="Independent"
            vec=c(vec,a)
        }
}

vec1=colnames(train[,index_cat_bin])
vec
df=data.frame(vec1,vec)
df


#Extracting dependent variables 

dep_var=df[df$vec=="Dependent",1]
str(dep_var)
dep_var


#Model building using variables 

dep_var=as.character(dep_var)
var=c(dep_var,cols_int)

var_index=match(var,names(train))
var_index

train=train[,var_index]






