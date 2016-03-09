
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

Full_Morph1<-rbind(Morph_Sp[grep('^3$',Morph_Sp$species),c(3,10, 13:19)],
                   Morph_Sp[grep('^17$',Morph_Sp$species),c(3,10,13:19)],
                   Morph_Sp[grep('^5$',Morph_Sp$species),c(3,10,13:19)])

# Select the number of rows of the desired species
#grep the exact value that you want '^i$'
#use rbind to bombine the tables per species

#1. Different Morphology, same Microhabitat, same clade
SUBset1<-rbind(Perf_Sp[grep('^3$',Perf_Sp$species),],
        Perf_Sp[grep('^17$',Perf_Sp$species),],
        Perf_Sp[grep('^5$',Perf_Sp$species),])

names1<-as.factor(c(rep('Chinese bitterling',7), rep('Small fat minnow',6), rep('Chinese false gudgeon',6))) #assign species as factor
michab1<-as.factor(c(rep('still', 19)))
size1<-Full_Morph1[,2]
underfin1<-Full_Morph1[,4]
headW<-Full_Morph1[,5]

SUBset1a<-cbind(names1,michab1, size1, underfin1,headW, SUBset1[,3:5]) #cbind names and performance
                

##PCA Analysis
PerfpcaSUB1<-prcomp(SUBset1a[,8:10])

#traits explaining variation

MorphpcaSUB1<-prcomp(Full_Morph1[,2:9])
traitperSUB1<-pca.structure(PerfpcaSUB1, SUBset1a[,8:10])

########prettyplot try one

pSub1<- ggbiplot(PerfpcaSUB1,choices=1:2, obs.scale = 1, groups = SUBset1a$names1,
                 labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset1a$names1, size = SUBset1a$total.L))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  theme(panel.background = element_rect(fill = 'white'))

under<-ggbiplot(PerfpcaSUB1,choices=1:2, obs.scale = 1, groups = SUBset1a$names1,
           labels.size=10, ellipse = TRUE)+
            geom_point(aes(colour=SUBset1a$names1, size = SUBset1a$underfinW))+ #add points according to total lenght
            scale_size_continuous(range = c(0,9))+ #change scale of the points
            theme(panel.background = element_rect(fill = 'white'))

head<-ggbiplot(PerfpcaSUB1,choices=1:2, obs.scale = 1, groups = SUBset1a$names1,
                labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset1a$names1, size = SUBset1a$headW))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  theme(panel.background = element_rect(fill = 'white'))

####PCA for subset 1  morphology
MorphologyPCA<- ggbiplot(MorphpcaSUB1,choices=1:2, obs.scale = 1, groups = Full_Morph1$species,
                 labels.size=10, ellipse = TRUE, var.axes=T)+
  theme(panel.background = element_rect(fill = 'white'))

traitmorSUB1<-pca.structure(MorphpcaSUB1,Full_Morph1[,2:9])

##correlation analysis subset1
corrMPSUB1<-cor(SUBset1a[,8:10],Full_Morph1[,2:9])

corrplot(corrMPSUB1, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/LinearM", 
         tl.cex=1, mar=c(0,0,1,0), cl.cex=1, diag = FALSE)


grid.arrange(MorphologyPCA,pSub1, under, head, ncol=1)


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