# @Dr. Kurban, July 2020
# For any questions, please contact hakurban@gmai.com
##########################################################################################
#                          ORDER PARAMETER FIGURES
##########################################################################################
# install packages
require("ggplot2")
library("reshape2")
library("ggpubr")
###########################################################################################anatese
#anatase data
data1 <- read.table("./anatase_all_geometries/anatase.txt",header = T)
data1 <- data1[,c(2,3,5)]
order.data1 <- read.table("./anatase_all_geometries/orderParameter1.txt",header = T)
data1 <- cbind(data1,order.data1[,2:3])
data1<- melt(data1, id = c("O","Ti"))
#plots
anatase.O<- ggplot(data=data1, aes(x=value, y = O, colour = variable, group = variable)) +
  geom_point()+ xlab("Energy (eV)")+
  ylab("Order Parameter") + theme_bw() + 
  theme(legend.title=element_blank(), panel.grid.major = element_blank(),
        axis.text=element_text(size=8),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        ) +  theme(legend.position = "none")

anatase.Ti<- ggplot(data=data1, aes(x=value, y = Ti, colour = variable, group = variable)) +
  geom_point()+ xlab("Energy (eV)")+
  ylab("Order Parameter") + theme_bw() + 
  theme(legend.title=element_blank(), panel.grid.major = element_blank(),
        axis.text=element_text(size=8),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        ,legend.position = c(0.29, 0.39)) +  annotate("text", x = -7, y = 1.641, label = "Anatase",fontface=2)
##########################################################################################
#brookite
data2 <- read.table("./brookite_all_geometries/brookite.txt",header = T)
data2 <- data2[,c(2,3,5)]
order.data2 <- read.table("./brookite_all_geometries/orderParameter2.txt",header = T)
data2 <- cbind(data2,order.data2[,2:3])
data2<- melt(data2, id = c("O","Ti"))

brookite.O<- ggplot(data=data2, aes(x=value, y = O, colour = variable, group = variable)) +
  geom_point()+ xlab("Energy (eV)")+
  ylab("Order Parameter") + theme_bw() + 
  theme(legend.title=element_blank(), panel.grid.major = element_blank(),
        axis.text=element_text(size=8),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        ) + theme(legend.position = "none") 

brookite.Ti<- ggplot(data=data2, aes(x=value, y = Ti, colour = variable, group = variable)) +
  geom_point()+ xlab("Energy (eV)")+  
  ylab("Order Parameter") + theme_bw() + 
  theme(legend.title=element_blank(), panel.grid.major = element_blank(),
        axis.text=element_text(size=8),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
      ) +  annotate("text", x = -7, y = 1.6315, label = "Brookite",fontface=2)+  theme(legend.position = "none")
##########################################################################################
#anatase
data3 <- read.table("./rutile_all_geometries/rutile.txt",header = T)
data3 <- data3[,c(2,3,5)]
order.data3 <- read.table("./rutile_all_geometries/orderParameter3.txt",header = T)
data3 <- cbind(data3,order.data3[,2:3])
data3<- melt(data3, id = c("O","Ti"))

rutile.O<- ggplot(data=data3, aes(x=value, y = O, colour = variable, group = variable)) +
  geom_point()+ xlab("Energy (eV)")+
  ylab("Order Parameter") + theme_bw() + 
  theme(legend.title=element_blank(), panel.grid.major = element_blank(),
        axis.text=element_text(size=8),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        ) +  theme(legend.position = "none")

rutile.Ti<- ggplot(data=data3, aes(x=value, y = Ti, colour = variable, group = variable)) +
  geom_point()+ xlab("Energy (eV)")+
  ylab("Order Parameter") + theme_bw() + 
  theme(legend.title=element_blank(), panel.grid.major = element_blank(),
        axis.text=element_text(size=8),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        )+ annotate("text", x = -6.7, y = 1.612, label = "Rutile",fontface=2)+   theme(legend.position = "none")
##########################################################################################
#Combine Figurers
figure <- ggarrange(anatase.Ti,anatase.O, brookite.Ti,brookite.O,rutile.Ti,rutile.O,
                    ncol = 2, nrow = 3)
figure
##########################################################################################
#                            CORRELATION FIGURES
###########################################################################################
#anatase
library(corrplot)
library(RColorBrewer)
par(mfrow=c(3,1))
data.anatase<- read.table("./anatase_all_geometries/anatase.txt",header = T)
order.anatase <- read.table("./anatase_all_geometries/orderParameter1.txt",header = T)
nn.anatase <-  read.table("./anatase_all_geometries/NN.anateseData.txt",header = T)
data1 <- cbind(data.anatase[,c(2,3,4,5)], order.anatase[,2:3],nn.anatase[,1:3])
colnames(data1)[5] <-"$R[O]"
colnames(data1)[6] <-"$R[Ti]"
colnames(data1)[7] <-"$n[O-O]"
colnames(data1)[8] <-"$n[Ti-Ti]"
colnames(data1)[9] <-"$n[O-Ti]"
cor.data1 <- cor(data1)
corrplot(cor.data1, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(cor.data1, method="number")
###########################################################################################
#brookite
data.brookite<- read.table("./brookite_all_geometries/brookite.txt",header = T)
order.brookite <- read.table("./brookite_all_geometries/orderParameter2.txt",header = T)
nn.brookite <-  read.table("./brookite_all_geometries/NN.brookite.txt",header = T)
data2<- cbind(data.brookite[,c(2,3,4,5)], order.brookite[,2:3],nn.brookite[,1:3])
colnames(data2)[5] <-"$R[O]"
colnames(data2)[6] <-"$R[Ti]"
colnames(data2)[7] <-"$n[O-O]"
colnames(data2)[8] <-"$n[Ti-Ti]"
colnames(data2)[9] <-"$n[O-Ti]"
cor.data2 <- cor(data2)
 corrplot(cor.data2, type="upper", order="hclust",
                        col=brewer.pal(n=8, name="RdYlBu"))
corrplot(cor.data2, method="number")
###########################################################################################
#rutile
data.rutile<- read.table("./rutile_all_geometries/rutile.txt",header = T)
order.rutile <- read.table("./rutile_all_geometries/orderParameter3.txt",header = T)
nn.rutile <-  read.table("./rutile_all_geometries/NN.rutile.txt",header = T)
data3 <- cbind(data.rutile[,c(2,3,4,5)], order.rutile[,2:3],nn.rutile[,1:3])
colnames(data3)[5] <-"$R[O]"
colnames(data3)[6] <-"$R[Ti]"
colnames(data3)[7] <-"$n[O-O]"
colnames(data3)[8] <-"$n[Ti-Ti]"
colnames(data3)[9] <-"$n[O-Ti]"
cor.data3 <- cor(data3)
corrplot(cor.data3, type="upper", order="hclust",
                        col=brewer.pal(n=8, name="RdYlBu"))
corrplot(cor.data3, method="number")
###########################################################################################
#                       DATA SUMMARY VISUALIZATION
###########################################################################################
library(GGally)
library(ggplot2)
###########################################################################################
#anatase
anatase.small.data <- read.table("./anatase_all_geometries/0K.txt",header = F)
anatase.fig.small <- ggpairs(anatase.small.data, aes(color = V1))+ theme_bw()
for(i in 1:anatase.fig.small$nrow) {
  for(j in 1:anatase.fig.small$ncol){
    anatase.fig.small[i,j] <- anatase.fig.small[i,j]  
  }
}
anatase.fig.small

anatase.big.data <- read.table("./anatase_all_geometries/1000K.txt",header = F)
anatase.fig.big <- ggpairs(anatase.big.data, aes(color = V1))+ theme_bw()
for(i in 1:anatase.fig.big$nrow) {
  for(j in 1:anatase.fig.big$ncol){
    anatase.fig.big[i,j] <- anatase.fig.big[i,j]  
  }
}
anatase.fig.big
###########################################################################################
#brookite
brookite.small.data <- read.table("./brookite_all_geometries/0K.txt",header = F)
brookite.fig.small <- ggpairs(brookite.small.data, aes(color = V1))+ theme_bw()
for(i in 1:brookite.fig.small$nrow) {
  for(j in 1:brookite.fig.small$ncol){
    brookite.fig.small[i,j] <- brookite.fig.small[i,j]  
  }
}
brookite.fig.small 

brookite.big.data <- read.table("./brookite_all_geometries/1000K.txt",header = F)
brookite.fig.big <- ggpairs(brookite.big.data, aes(color = V1))+ theme_bw()
for(i in 1:brookite.fig.big$nrow) {
  for(j in 1:brookite.fig.big$ncol){
    brookite.fig.big[i,j] <- brookite.fig.big[i,j]  
  }
}
brookite.fig.big
###########################################################################################
#rutile
rutile.small.data <- read.table("./rutile_all_geometries/0K.txt",header = F)
rutile.fig.small <- ggpairs(rutile.small.data, aes(color = V1))+ theme_bw()
for(i in 1:rutile.fig.small$nrow) {
  for(j in 1:rutile.fig.small$ncol){
   c[i,j] <- rutile.fig.small[i,j]  
  }
}
rutile.fig.small

rutile.big.data <- read.table("./rutile_all_geometries/1000K.txt",header = F)
rutile.fig.big <- ggpairs(rutile.big.data, aes(color = V1))+ theme_bw()
for(i in 1:rutile.fig.big$nrow) {
  for(j in 1:rutile.fig.big$ncol){
    rutile.fig.big[i,j] <- rutile.fig.big[i,j]  
  }
}
rutile.fig.big 
###########################################################################################s