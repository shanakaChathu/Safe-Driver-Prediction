str(train)
library(FactoMineR)

cols_cat_bin
index_cat=match(cols_cat_bin,names(train[,-1]))

res=PCA(train[,-1],ncp=10,scale.unit=TRUE,quali.sup=index_cat,graph=TRUE)
library(FactoInvestigate)
Investigate(res,document="pdf_document")


str(train)
str(train[,-1])

?PCA

str(train)

