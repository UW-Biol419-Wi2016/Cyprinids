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
         labels= Perf_Sp$species, scale=1, ellipse = T)

levels(Morph_Sp$species)<-c(1:19)

ggbiplot(Ratiopca, choices=1:2, groups = Morph_Sp$species, 
         labels= Morph_Sp$species, scale=1, ellipse = T)

ggbiplot(Linearpca, choices=1:2, groups = Perf$species, 
         labels= Perf$species, scale=1, ellipse = T)



