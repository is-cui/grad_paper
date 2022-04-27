##-----信噪比-----
library(reshape2)
data=as.data.frame(t(read.csv("D:\\毕业设计\\sort_GSE35809.csv",row.names = 1)))
data$class=rep(c("proliferative","metabolic","mesenchymal"),c(29,15,26))
dt=melt(data)
mean=aggregate(dt$value,by=list(dt$class,dt$variable),FUN = mean)
sd=aggregate(dt$value,by=list(dt$class,dt$variable),FUN = sd)
snr_dt=mean
snr_dt$y=sd$x
colnames(snr_dt)=c("class","gene","mean","sd")
a=aggregate(snr_dt$sd,by=list(snr_dt$gene),FUN = sum)
snr_dt$sd_sum=rep(a$x,rep(3,54675))

for (i in seq(1,by=3,length.out = 54675)) {
  snr_dt$mean_sum[i]=abs(snr_dt$mean[i]-snr_dt$mean[i+1])+abs(snr_dt$mean[i+1]-snr_dt$mean[i+2])+abs(snr_dt$mean[i]-snr_dt$mean[i+2])
  snr_dt$mean_sum[i+1]=snr_dt$mean_sum[i]
  snr_dt$mean_sum[i+2]=snr_dt$mean_sum[i]
}

##------SNR指标值-----
SNR=as.data.frame(snr_dt$gene[!duplicated(snr_dt$gene)])
SNR[,2]=((snr_dt$mean_sum/snr_dt$sd_sum)[!duplicated(snr_dt$mean_sum/snr_dt$sd_sum)])
colnames(SNR)=c("gene","SNR")

table(cut(SNR$SNR,breaks = seq(0,by=0.7,length.out = 5)))

##-----绘图-----
library(ggplot2)
library(gcookbook)
ggplot(SNR)+
  labs(title = "SNR of genes",hjust=0.5,x="Gene",y="SNR")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(aes(x=1:54675,y=sort(SNR),color=rgb(0.9,0.5,0.1)),size=0.9,show.legend = FALSE)+
  geom_hline(yintercept = 0.7,color="blue",linetype="dashed",size=1)+
  annotate("text", x=10000, y=0.9, label="SNR=0.7",fontface="italic",size=4.5) 
  
##-----过滤后的基因数据-----
SVM_dt=SNR[SNR$SNR>0.7,]
gene=SVM_dt$gene
SVM_dt=data[,gene]
write.csv(SVM_dt,"D:\\毕业设计\\SVM_GSE35809.csv")
