library(ggplot2)
library(reshape2)
data=read.csv("D:\\毕业设计\\GSE35809.csv",row.names = 1)
ID=read.csv("D:\\毕业设计\\ID.csv")

##-----标准化数据-----
std_data=apply(data, 1, scale)
std_data=t(std_data)
colnames(std_data)=colnames(data)
std_data=as.data.frame(std_data)
write.csv(std_data,"D:\\毕业设计\\std_GSE35809.csv")

##-----绘制箱线图-----
box_dt=melt(data)
box_dt$class=rep(as.array(ID$锘縄D),rep(54675,70))
colnames(box_dt)=c("sample","value","class")


ggplot(box_dt,aes(x=sample,y=value,color=class))+
  geom_boxplot(alpha=0.7,outlier.shape = "o",outlier.size = 0.8,outlier.color="black")+
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 0.5),
        legend.key.height = unit(15,"pt"),legend.key.width = unit(10,"pt"))+
  labs(title = "Gene expression in samples",y="Gene expression level",x="samples")


##-----基因的均值-方差图-----
mean_dt=data
mean_dt$mean=apply(mean_dt,1,mean)
mean_dt=mean_dt[order(mean_dt$mean),]
mean_dt$variance=apply(mean_dt,1,sd)
dtt=mean_dt[,c(71,72)]
dtt=melt(dtt)
dtt$id=rep(1:54675,2)
ggplot(dtt)+
  labs(title = "Mean-Variance in samples",x="Gene",y="Mean-Variance")+
  theme_grey()+
  geom_point(aes(x=id,y=value,color=variable),alpha=0.5,shape=17,size=0.6)+
  theme(legend.position = c(0.15,0.8),legend.title=element_blank())+
  guides(fill=guide_legend(title = NULL))+
  theme(plot.title = element_text(hjust = 0.5)) #标题居中


