

a1=c(2,-1,3,4,2,4,3,-1)
a=c(1,2,3,2,-1,3,-1,5)
b=c(0,1,-1,0,1,0,-1,1)
d=c(1,0,-1,1,-1,0,1,1)
x=data.frame(a1,b,a,d)
str(x)
x
x[x==-1]=NA
x

Mode <- function(y) 
{
  ux <- unique(y)
  ux[which.max(tabulate(match(y, ux)))]
}


i1=c(1,3)
for (var in 1:length(i1))
{     for(j in 1:nrow(x))
{
  if(is.na(x[j,i1[var]])) 
  {
    x[j,i1[var]]=median(x[,i1[var]],na.rm = TRUE) 
  }
}  
}
str(x)
x

i2=c(2,4)
for (var in 1:length(i2))
{     
  for(j in 1:nrow(x))
  {
    if(is.na(x[j,i2[var]])) 
    {
      x[j,i2[var]]=Mode(x[,i2[var]])
    }
  }  
}
str(x)
x



#Imbalance problem handling 
require(DMwR)

ncols<-ncol(dm)
dm<-cbind(dm[2:ncols],dm[1])
dmSmote<-SMOTE(target ~ . , dm,k=5,perc.over = 1400,perc.under=140)
dm<-cbind(dmSmote[ncols],dmSmote[1:ncols-1])


Over = ( (0.6 * COMMON_NO) - RARE_NO ) / RARE_NO
Under = (0.4 * COMMON_NO) / (RARE_NO * Over)

Over_Perc = round(Over, 1) * 100
Under_Perc = round(Under, 1) * 100
