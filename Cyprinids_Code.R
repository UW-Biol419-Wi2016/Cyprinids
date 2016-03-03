######Cyprinids Morphology- Performance########
######### Feb 25 2016 ########################

##Set working directory
setwd('/Users/darwin/Desktop/BOPL519/BIOL519_Project/Cyprinids')

###Install packages
library("vegan")
install.packages("pastecs")
library("pastecs")
source('/Users/darwin/Desktop/BOPL519/BIOL519_Project/biostats.R')

##plot packages
install.packages("devtools")
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
install.packages("rgl")
library(rgl)

##Read files
Perf<-read.csv('Cypr_Perf.csv')
Morph<-read.csv('Cypr_Morpho.csv')

##Subset only numerical data
Perf_Num<-Perf[,3:5]
Ratios<- Morph[,4:9]
LinearM<-Morph[,c(10:19)]
Linear2<-Morph[,c(10, 13:19)] 

##Examine data, make sure species is a factor and the rest numeric
str(Ratios)
str(LinearM)
str(Perf)

####Clean data

#remove individual without performance data
empty<-(which(Perf_Num$vmax=='0')) #list of sepecies without data

#remove from each matrix
Perf_Num<- Perf_Num[-empty,]
Ratios<- Morph[-empty,4:9]
LinearM<-Morph[-empty,c(10:19)]
Linear2<-Morph[-empty,c(10, 13:19)] 

##Tables with final number of individuals to extract the names
Original_sp<-Perf[-empty,]
Perf_Sp<-Perf[-empty,]
Morph_Sp<-Morph[-empty,]

## Pca for the performance, select only the numeric columns
Perfpca<-prcomp(Perf_Num)
Ratiopca <-prcomp(Ratios)
Linearpca <- prcomp(LinearM)
Linear2pca<- prcomp(Linear2)

##you can check the summary for each pca
summary(Perfpca)

#Calculate eigenvalues for each PCA and Broken stick values
#Round the numeric values to have two decimals

eigen1<-as.data.frame(round(pca.eigenval(Perfpca), 2))
eigen2<-as.data.frame(round(pca.eigenval(Ratiopca), 2))
eigen3<-as.data.frame(round(pca.eigenval(Linearpca), 2))
eigen4<-as.data.frame(round(pca.eigenval(Linear2pca), 2))

#export eigenvalues and variance tables
write.csv(c(eigen1,eigen2,eigen3, eigen4), file = "PCAVar.csv")

##What variables explain the most amount of variation
#you can indicate the number of PCs that you want to see in the table by adding dim=n
trait1<-pca.structure(Perfpca,Perf_Num)
trait2<-pca.structure(Ratiopca,Ratios)
trait3<-pca.structure(Linearpca,LinearM)
trait4<-pca.structure(Linear2pca,Linear2)

#export tables, thay can not be exported in the same file 
#because do not have the same numebr of variables

write.csv(trait1, file = "Perf_Trait_Explain_Var.csv")
write.csv(trait2, file = "Ratio_Trait_Explain_Var.csv")
write.csv(trait3, file = "Linear_Trait_Explain_Var.csv")
write.csv(trait4, file = "Linear2_Trait_Explain_Var.csv")

#Broken Stick plot for each pca
#eigen values definetly higher than brooken stick, explain the variance significativelly
#export the figure as pdf

pdf("Cyprinids_Brokenstick.pdf")
par(mfrow=c(3,1))
screeplot(Perfpca, bstick=TRUE, main = 'Performance') 
screeplot(Ratiopca, bstick=TRUE, main = 'Body ratios') 
screeplot(Linearpca, bstick=TRUE, main = 'Linear measurements') 
dev.off()

##Regular Plot just to see how the data loos like
biplot(Perfpca)
biplot(Ratiopca)
biplot(Linearpca)

ordiplot(Perfpca)

#####Plot results of pca
#change species names to numbers to visualize in a plot

library(ggplot2)

levels(Perf_Sp$species)<-c(1:19)
p1<-ggbiplot(Perfpca, choices=1:2, groups = Perf_Sp$species, 
         labels= Perf_Sp$species, scale=1, ellipse = T,
         legend.text=levels(Original_sp$species))

levels(Morph_Sp$species)<-c(1:19)
p2<-ggbiplot(Ratiopca, choices=1:2, groups = Morph_Sp$species, 
         labels= Morph_Sp$species, scale=1, ellipse = T)

p3<-ggbiplot(Linearpca, choices=1:2, groups = Morph_Sp$species, 
         labels= Morph_Sp$species, scale=1, ellipse = T, col='black')+
theme(panel.background = element_rect(fill = 'white'))

##package to multiplot ggplot 2
install.packages('gridExtra')
library(gridExtra)
# indicate number of rows and columns
grid.arrange(p1, p2, p3, ncol=1)
###element_blank() #draws nothing

##maybe leyend
+geom_line(aes(color="Important line"))+
  geom_point(aes(color="My points"))

##Correlation analysis

####try one
corr(Perf_Num, Ratios)
cov(Perf_Num, Ratios)
corr(Perf_Num, LinearM)
corr(Ratios, LinearM)

##### try two
cor(Perf_Num, Ratios, use = "everything",
    method = c("pearson", "kendall", "spearman"))


#####try three

#correlation plot package
install.packages('corrplot')
library(corrplot)

# Pie plots for correlation test

##plot the three correlation plots in a column
par(mfrow=c(3,1))
Perf_Ratios <- cor(Perf_Num, Ratios)
corrplot(Perf_Ratios, method = "pie", tl.col = "black", 
         tl.srt = 45) 
#other methods "square", "ellipse", "number","shade", color, pie

Perf_LinearM <- cor(Perf_Num, LinearM)
corrplot(Perf_LinearM, method = "pie", tl.col = "black", 
         tl.srt = 45)

colCypr<- c('#d8b365', '#5ab4ac')
Ratios_LinearM <- cor(Ratios, LinearM)
corrplot(Ratios_LinearM, method = "pie", tl.col = "black", 
         tl.srt = 45, col = colCypr, rect.col = F, tl.cex = 1)
           

####linear regressions 
cor(Perf_Num)
pairs(Perf_Num)

cor(LinearM)
pairs(LinearM)

cor(Ratios)
pairs(Ratios)

lm.out= lm(vmax~amax, data = Perf_Num)
summary(lm.out)
plot(vmax~amax, data = Perf_Num)
abline(lm.out, col='red')


## fail attempt to run the correlations per trait in a loop
for (i in 1:ncol(Perf_Num))
  {
  for(j in 1:nrow(Perf_Num))
  {
    lm.out= lm(i~j, data = Perf_Num)
    plot(i~j, data = Perf_Num)
  }
}

####Plots to improve, 3 pca, 3 corr, 3 clust