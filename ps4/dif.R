read.table(file="https://raw.githubusercontent.com/ben-domingue/252L/master/data/emp-3pl-math-reading.txt",header=TRUE)->x
dim(x)
x[rowSums(is.na(x))==0,]->x
dim(x)

grep("r.mc",names(x))->index
rowSums(x[,index])->rs
as.numeric(rs<mean(rs))-> gr #this can be treated as the confounding variable in the DIF analysis
grep("m.mc",names(x))->index
rowSums(x[,index])->th #this can be treated as ability in DIF analysis
