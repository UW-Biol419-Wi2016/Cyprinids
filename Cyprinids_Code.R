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

##Tables with final number of individuals to extract the names
Original_sp<-Perf[-empty,]
Perf_Sp<-Perf[-empty,]
Morph_Sp<-Morph[-empty,]

## Pca for the performance, select only the numeric columns
Perfpca<-prcomp(Perf_Num)
Ratiopca <-prcomp(Ratios)
Linearpca <- prcomp(LinearM)

summary(Perfpca)

#Calculate eigenvalues
VarNum.eigenva<-Perfpca$sdev^2#calculate variance in each PCA
VarNum.eigenva
pca.eigenval(Perfpca)

#Broken Stick plot
screeplot(Perfpca, bstick=TRUE) #eigen values definetly higher than brooken stick, explain the variance significativelly

##Regular Plot just to see how the data loos like
biplot(Perfpca)
biplot(Ratiopca)
biplot(Linearpca)

ordiplot(Perfpca)

#####Plot results of pca
#change species names to numbers to visualize in a plot
levels(Perf_Sp$species)<-c(1:19)
ggbiplot(Perfpca, choices=1:2, groups = Perf_Sp$species, 
         labels= Perf_Sp$species, scale=1, ellipse = T,
         legend.text=levels(Original_sp$species))

levels(Morph_Sp$species)<-c(1:19)

ggbiplot(Ratiopca, choices=1:2, groups = Morph_Sp$species, 
         labels= Morph_Sp$species, scale=1, ellipse = T)

ggbiplot(Linearpca, choices=1:2, groups = Morph_Sp$species, 
         labels= Morph_Sp$species, scale=1, ellipse = T)

##Correlation analysis

####try one
corr(Perf_Num, Ratios)
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


