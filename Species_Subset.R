
#table to be use
Perf<-read.csv('Cypr_Perf.csv')
#Table with numbers instead species

Perf_Num<-Perf[,3:5]#only numeric values
empty<-(which(Perf_Num$vmax=='0')) #list of sepecies without data
Perf_Sp<-Perf[-empty,] #remove missing data
levels(Perf_Sp$species)<-c(1:19)#change species for numbers
#base dataset
Perf_Sp
#change levels for morphology
Morph_Sp<-Morph[-empty,]

Fish_size1<-rbind(Morph_Sp[grep('^3$',Perf_Sp$species),c(3,10)],
                 Morph_Sp[grep('^17$',Perf_Sp$species),c(3,10)],
                 Morph_Sp[grep('^5$',Perf_Sp$species),c(3,10)])


# Select the number of rows of the desired species
#grep the exact value that you want '^i$'
#use rbind to bombine the tables per species

#1. Different Morphology, same Microhabitat, same clade
SUBset1<-rbind(Perf_Sp[grep('^3$',Perf_Sp$species),],
        Perf_Sp[grep('^17$',Perf_Sp$species),],
        Perf_Sp[grep('^5$',Perf_Sp$species),])
names1<-as.factor(c(rep(3,7), rep(17,6), rep(5,6))) #assign species as factor
michab1<-as.factor(c(rep('still', 19)))
size1<-Fish_size1

SUBset1a<-cbind(names1,michab1, size1,SUBset1[,3:5]) #cbind names and performance
                
#2. Different Morphology, different Microhabitat, same clade
SUBset2<-rbind(Perf_Sp[grep('^1$',Perf_Sp$species),],
              Perf_Sp[grep('^4$',Perf_Sp$species),],
              Perf_Sp[grep('^6$',Perf_Sp$species),])
names2<-as.factor(c(rep(1,8), rep(4,6), rep(6,7))) #assign species as factor
SUBset2a<-cbind(names2, SUBset2[,3:5]) 


#3. Same Morphology, different Microhabitat, same clade
SUBset3<-rbind(Perf_Sp[grep('^7$',Perf_Sp$species),],
              Perf_Sp[grep('^8$',Perf_Sp$species),])
names3<-as.factor(c(rep('Common carp' ,8), rep('Crucian carp',8))) #assign species as factor
SUBset3a<-cbind(names3, SUBset3[,3:5]) #cbind names and performance

#4. Same Morphology, same Microhabitat, same clade
SUBset4<-rbind(Perf_Sp[grep('^3$',Perf_Sp$species),],
              Perf_Sp[grep('^14$',Perf_Sp$species),])
names4<-as.factor(c(rep('Chinese bitterling',7), rep('Rose bitterling',6))) #assign species as factor
SUBset4a<-cbind(names4, SUBset4[,3:5]) #cbind names and performance


##PCA Analysis
PerfpcaSUB1<-prcomp(SUBset1a[,5:7])
PerfpcaSUB2<-prcomp(SUBset2a[,2:4])
PerfpcaSUB3<-prcomp(SUBset3a[,2:4])
PerfpcaSUB4<-prcomp(SUBset4a[,2:4])

biplot(PerfpcaSUB1)

########prettyplot try one
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, group=wine.class,
         varname.size = 8, labels.size=10, 
         ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +  
  geom_point(aes(colour=wine.class), size = 8) +
  theme(legend.direction ='horizontal', 
        legend.position = 'top')
######

pSub1<- 
  ggbiplot(PerfpcaSUB1,choices=1:2, obs.scale = 1, groups = SUBset1a$names1,
           labels.size=10, ellipse = TRUE)+
            geom_point(aes(colour=SUBset1a$names1, size = SUBset1a$total.L))+ #add points according to total lenght
            scale_size_continuous(range = c(0,9))+ #change scale of the points
            theme(panel.background = element_rect(fill = 'white'))


##########
pSub2<- ggbiplot(PerfpcaSUB2, choices=1:2, groups = SUBset2a$names2, 
                 labels= SUBset2a$names2, scale=1, ellipse = T)+
  ggtitle('Performance')

pSub3<- ggbiplot(PerfpcaSUB3, choices=1:2, groups = SUBset3a$names3, 
                 labels= SUBset3a$names3, scale=1, ellipse = T)

pSub4<- ggbiplot(PerfpcaSUB4, choices=1:2, groups = SUBset4a$names4, 
                 labels= SUBset4a$names4, scale=1, ellipse = T)

grid.arrange(pSub1,pSub2,pSub3,pSub4, ncol=2)


#######

nc1 <- NbClust(SUBset1a[,2:4], min.nc=2, max.nc=15, method="kmeans")
kclus1<-ggbiplot(PerfpcaSUB1, choices=1:2, groups = nc1$Best.partition, 
         labels= SUBset1a$names1, scale=1, ellipse = T,
         legend.text=nc1$Best.partition)+
  ggtitle('Best Number of Clusters')

grid.arrange(pSub1,kclus1,ncol=2)

###Figure iit our...how to loop to subset??
SUB<-c('3','5','17')
for (i in seq(SUB)){
  SUBset<-Perf_Sp[grep('^i$',Perf_Sp$species),]
  print(SUBset)
}