############change levels for morphology

Full_Morph3<-rbind(Morph_Sp[grep('^7$',Morph_Sp$species),c(3,10, 13:19)],
                   Morph_Sp[grep('^8$',Morph_Sp$species),c(3,10,13:19)])

# Select the number of rows of the desired species
#grep the exact value that you want '^i$'
#use rbind to bombine the tables per species

#1. Different Morphology, same Microhabitat, same clade
SUBset3<-rbind(Perf_Sp[grep('^7$',Perf_Sp$species),],
               Perf_Sp[grep('^8$',Perf_Sp$species),])
names3<-as.factor(c(rep('Common carp',8), rep('Crucian carp',8))) #assign species as factor
michab3<-as.factor(c(rep('still', 8),rep('intermediate',8)))
size3<-Full_Morph3[,2]
ventral3<-Full_Morph3[,4]
underfin3<-Full_Morph3[,9]

SUBset3a<-cbind(names3,michab3, size3, ventral3,underfin3, SUBset3[,3:5]) #cbind names and performance

##PCA Analysis
PerfpcaSUB3<-prcomp(SUBset3a[,6:8])
traitperSUB3<-pca.structure(PerfpcaSUB3, SUBset3a[,6:8])

####PCA for subset 1  morphology

MorphpcaSUB3<-prcomp(Full_Morph3[,2:9])
traitmorSUB3<-pca.structure(MorphpcaSUB3,Full_Morph3[,2:9])

MorphologyPCA3<- ggbiplot(MorphpcaSUB3,choices=1:2, obs.scale = 1, groups = Full_Morph3$species,
                          labels.size=10, ellipse = TRUE, var.axes=T)+
  scale_colour_manual(values=c("#c2a5cf", "#a6dba0"))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(legend.position="none")
########prettyplot try one

pSub3<- ggbiplot(PerfpcaSUB3,choices=1:2, obs.scale = 1, groups = SUBset3a$names3,
                 labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset3a$names3, size = SUBset3a$size3))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  scale_colour_manual(values=c("#c2a5cf", "#a6dba0"))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))

vent3<-ggbiplot(PerfpcaSUB3,choices=1:2, obs.scale = 1, groups = SUBset3a$names3,
                labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset3a$names3, size = SUBset3a$ventral3))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  theme(panel.background = element_rect(fill = 'white'))+
  scale_colour_manual(values=c("#c2a5cf", "#a6dba0"))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))

underf3<-ggbiplot(PerfpcaSUB3,choices=1:2, obs.scale = 1, groups = SUBset3a$names3,
                    labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset3a$names3, size = SUBset3a$underfin3))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  theme(panel.background = element_rect(fill = 'white'))+
  scale_colour_manual(values=c("#c2a5cf", "#a6dba0"))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))



##correlation analysis subset1
corrMPSUB3<-cor(SUBset3a[,6:8],Full_Morph3[,2:9])

corrplot(corrMPSUB3, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/LinearM", 
         tl.cex=1, mar=c(0,0,1,0), cl.cex=1, diag = FALSE, cl.pos='n')


grid.arrange(MorphologyPCA3,pSub3,vent3, underf3, ncol=2)

grid.arrange(MorphologyPCA,head, MorphologyPCA2,pSub2, MorphologyPCA3,underf3, ncol=2)


############change levels for morphology

Full_Morph4<-rbind(Morph_Sp[grep('^12$',Morph_Sp$species),c(3,10, 13:19)],
                   Morph_Sp[grep('^15$',Morph_Sp$species),c(3,10,13:19)])
Full_Morph4<-Full_Morph4[-8,]
# Select the number of rows of the desired species
#grep the exact value that you want '^i$'
#use rbind to bombine the tables per species

#1. Different Morphology, same Microhabitat, same clade
SUBset4<-rbind(Perf_Sp[grep('^12$',Perf_Sp$species),],
               Perf_Sp[grep('^15$',Perf_Sp$species),])
names4<-as.factor(c(rep('Qingbo',7), rep('Sharp-jaw barbel',7))) #assign species as factor
michab4<-as.factor(c(rep('riptide', 14)))
size4<-Full_Morph4[,2]
headW4<-Full_Morph4[,5]
caudal.baseW4<-Full_Morph4[,6]

SUBset4a<-cbind(names4,michab4, size4, headW4,caudal.baseW4, SUBset4[-8,3:5]) #cbind names and performance

##PCA Analysis
PerfpcaSUB4<-prcomp(SUBset4a[,6:8])
traitperSUB4<-pca.structure(PerfpcaSUB4, SUBset4a[,6:8])

####PCA for subset 1  morphology

MorphpcaSUB4<-prcomp(Full_Morph4[,2:9])
traitmorSUB4<-pca.structure(MorphpcaSUB4,Full_Morph4[,2:9])

MorphologyPCA4<- ggbiplot(MorphpcaSUB4,choices=1:2, obs.scale = 1, groups = Full_Morph4$species,
                          labels.size=10, ellipse = TRUE, var.axes=T)+
  scale_colour_manual(values=c("#d01c8b", "#fdb863"))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(legend.position="none")
########prettyplot try one

pSub4<- ggbiplot(PerfpcaSUB4,choices=1:2, obs.scale = 1, groups = SUBset4a$names4,
                 labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset4a$names4, size = SUBset4a$size4))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  scale_colour_manual(values=c("#d01c8b", "#fdb863"))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))


##correlation analysis subset1
corrMPSUB4<-cor(SUBset4a[,6:8],Full_Morph4[,2:9])

corrplot(corrMPSUB4, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/LinearM", 
         tl.cex=1, mar=c(0,0,1,0), cl.cex=1, diag = FALSE, cl.pos='n')


grid.arrange(MorphologyPCA,pSub1, ncol=2)

###Final plotting




grid.arrange(MorphologyPCA,pSub1,ncol=2)
grid.arrange(MorphologyPCA2,pSub2, ncol=2)
grid.arrange(MorphologyPCA4,pSub4, ncol=2)
grid.arrange(MorphologyPCA3,pSub3, ncol=2)


