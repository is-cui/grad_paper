dt=read.csv("D:\\毕业设计\\SVM_GSE35809.csv",row.names = 1)
std_dt=apply(dt, 2, scale)

colnames(std_dt)=colnames(dt)
std_dt=as.data.frame(std_dt)
row.names(std_dt)=row.names(dt)
write.csv(std_dt,"D:\\毕业设计\\SVM_std_GSE35809.csv")
