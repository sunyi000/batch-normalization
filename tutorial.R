library(sva)
library(bladderbatch)
data(bladderdata)
library(pamr)
library(limma)

pheno = pData(bladderEset)
edata = exprs(bladderEset)

print (pheno)

print (edata)
