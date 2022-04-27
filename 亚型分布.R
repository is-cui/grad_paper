
#查看探针对应的基因
library(BiocManager)
BiocManager::install("hgu133plus2.db")
library(hgu133plus2.db)
ids=toTable(hgu133plus2SYMBOL)

#三种亚型的分布图
dt=as.data.frame(c("proliferative","mesenchymal","metabolic"))
dt$id=c(1,2,3)
dt$var=c(29,26,15)
colnames(dt)=c("var","id","n")
library(ggplot2)

ggplot(dt,aes(x=factor(1),n,fill=factor(var)))+
  
  geom_bar(stat="identity",position="fill")+
  
  coord_polar(theta="y")+ # 按Y轴极坐标转换
  
  labs(title="饼图")


library(ggplot2)
dt = data.frame(A = c(29,26,15), B = c("增殖型","间质型","代谢型"))

dt = dt[order(dt$A, decreasing = TRUE),]
myLabel = as.vector(dt$B)   
myLabel = paste(myLabel, " ", round(dt$A / sum(dt$A) * 100, 2), "%", sep = "")   
showtext_auto()
p = ggplot(dt, aes(x = "", y = A, fill = B)) +
  geom_bar(stat = "identity", width = 19) +    
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme_bw()+
  theme(axis.ticks = element_blank()) + 
  theme(legend.title = element_blank(), legend.position = "top") + 
  scale_fill_discrete(breaks = dt$B, labels = myLabel) + 
  theme(axis.text.x = element_blank()) + 
  geom_text(aes(y = A/2 + c(0, cumsum(A)[-length(A)]), x = sum(A)/20, label = myLabel), size = 3)   ## 在图中加上百分比：x 调节标签到圆心的距离, y 调节标签的左右位置
p

library(showtext)
