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

#to include legend
  #theme(legend.position = "right")+
  #scale_color_discrete(name="species")+
#to remove the legend
  # scale_colour_discrete(guide = FALSE) 
library(ggplot2)

levels(Perf_Sp$species)<-c(1:19)
p1<- ggbiplot(Perfpca, choices=1:2, groups = Original_sp$species, 
         labels= Perf_Sp$species, scale=1, ellipse = T)+
    scale_colour_discrete(guide = FALSE)+ 
        ggtitle('Performance')

levels(Morph_Sp$species)<-c(1:19)
p2<-ggbiplot(Ratiopca, choices=1:2, groups = Original_sp$species, 
         labels= Morph_Sp$species, scale=1, ellipse = T)+
          scale_colour_discrete(guide = FALSE)+
          ggtitle('Body Ratios')

p3<-ggbiplot(Linearpca, choices=1:2, groups = Morph_Sp$species, 
         labels= Morph_Sp$species, scale=1, ellipse = T, col='black')+
          scale_colour_discrete(guide = FALSE)+
          ggtitle('Linear Measurements')

p4<-ggbiplot(Linear2pca, choices=1:2, groups = Morph_Sp$species, 
             labels= Morph_Sp$species, scale=1, ellipse = T, col='black')+
              scale_colour_discrete(guide = FALSE)+
              ggtitle('Linear Measurements 2')

#change background to white
#theme(panel.background = element_rect(fill = 'white'))

##package to multiplot ggplot 2
install.packages('gridExtra')
library(gridExtra)

# indicate number of rows and columns
grid.arrange(p1, p4)#, p3, p2, ncol=2)

###element_blank() #draws nothing


#######Correlation analysis
###########################

####try one
corr(Perf_Num, Ratios)
cov(Perf_Num, Ratios)
corr(Perf_Num, LinearM)
corr(Ratios, LinearM)

##### try two
cor(Perf_Num, Ratios, use = "everything",
    method = c("pearson", "kendall", "spearman"))


#####try three

### package to keep name rows as columns
install.packages('data.table')
library(data.table)
#function setDT(data, keep.rownames = TRUE)[]
#function to melt tables using the same variable
require(reshape2)

##correlation analyses
Perf_Ratios <- cor(Perf_Num, Ratios)
              setDT(Perf_Ratios, keep.rownames = TRUE)[]
Perf_LinearM <- cor(Perf_Num, LinearM)
              setDT(Perf_LinearM, keep.rownames = TRUE)[]


testtt <- merge(Perf_Ratios,Perf_LinearM,by="rn")
      
corrplot(testtt, use,names=T, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/Body Ratios", 
         tl.cex=1, mar=c(0,0,1,0), cl.cex=1, diag = FALSE)
######

Ratios_LinearM <- cor(Ratios, LinearM)

Perf_Linear2 <- cor(Perf_Num, Linear2)

##correlation plots
# Pie plots for correlation test

#correlation plot package
install.packages('corrplot')
library(corrplot)

##plot the three correlation plots in a column

par(mfrow=c(2,2))
corrplot(Perf_Ratios, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/Body Ratios", 
         tl.cex=1, mar=c(0,0,1,0), cl.cex=1, diag = FALSE)
corrplot(Perf_Linear2, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/LinearM 2", 
         mar=c(0,0,1,0))   
corrplot(Perf_LinearM, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/LinearM", mar=c(0,0,1,0))
corrplot(Ratios_LinearM, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Ratios/LinearM", mar=c(0,0,1,0))
dev.off()
#other methods "square", "ellipse", "number","shade", color, pie
##to change color
#colCypr<- c('#d8b365', '#5ab4ac')#create a vector
#add col = colCypr


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