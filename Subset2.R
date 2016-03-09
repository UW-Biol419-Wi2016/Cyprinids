############change levels for morphology

Full_Morph2<-rbind(Morph_Sp[grep('^1$',Morph_Sp$species),c(3,10, 13:19)],
                   Morph_Sp[grep('^4$',Morph_Sp$species),c(3,10,13:19)],
                   Morph_Sp[grep('^6$',Morph_Sp$species),c(3,10,13:19)])

# Select the number of rows of the desired species
#grep the exact value that you want '^i$'
#use rbind to bombine the tables per species

#1. Different Morphology, same Microhabitat, same clade
SUBset2<-rbind(Perf_Sp[grep('^1$',Perf_Sp$species),],
               Perf_Sp[grep('^4$',Perf_Sp$species),],
               Perf_Sp[grep('^6$',Perf_Sp$species),])
names2<-as.factor(c(rep('Big head arp',8), rep('Chinise bream',6), rep('Chinese hooksnout carp',7))) #assign species as factor
michab2<-as.factor(c(rep('still', 8),rep('intermediate',6), rep('riptide',7)))
size2<-Full_Morph2[,2]
headW2<-Full_Morph2[,5]
caudal.baseW2<-Full_Morph2[,6]

SUBset2a<-cbind(names2,michab2, size2, headW2,caudal.baseW2, SUBset2[,3:5]) #cbind names and performance

##PCA Analysis
PerfpcaSUB2<-prcomp(SUBset2a[,6:8])
traitperSUB2<-pca.structure(PerfpcaSUB2, SUBset2a[,6:8])

####PCA for subset 1  morphology

MorphpcaSUB2<-prcomp(Full_Morph2[,2:9])
traitmorSUB2<-pca.structure(MorphpcaSUB2,Full_Morph2[,2:9])

MorphologyPCA2<- ggbiplot(MorphpcaSUB2,choices=1:2, obs.scale = 1, groups = Full_Morph2$species,
                         labels.size=10, ellipse = TRUE, var.axes=T)+
  scale_colour_manual(values=c("#f4a582", "#92c5de", '#0571b0'))+
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

pSub2<- ggbiplot(PerfpcaSUB2,choices=1:2, obs.scale = 1, groups = SUBset2a$names2,
                 labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset2a$names2, size = SUBset2a$size2))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  scale_colour_manual(values=c("#f4a582", "#92c5de", '#0571b0'))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))

head2<-ggbiplot(PerfpcaSUB2,choices=1:2, obs.scale = 1, groups = SUBset2a$names2,
                labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset2a$names2, size = SUBset2a$headW2))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  theme(panel.background = element_rect(fill = 'white'))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))

caudalBW2<-ggbiplot(PerfpcaSUB2,choices=1:2, obs.scale = 1, groups = SUBset2a$names2,
               labels.size=10, ellipse = TRUE)+
  geom_point(aes(colour=SUBset2a$names2, size = SUBset2a$caudal.baseW2))+ #add points according to total lenght
  scale_size_continuous(range = c(0,9))+ #change scale of the points
  theme(panel.background = element_rect(fill = 'white'))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x	= element_text(size=14))+
  theme(axis.text.y	= element_text(size=14))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.line.x = element_line(size = 0.5, colour = "grey", linetype = 1))+
  theme(axis.line.y = element_line(size = 0.5, colour = "grey", linetype = 1))



##correlation analysis subset1
corrMPSUB2<-cor(SUBset2a[,6:8],Full_Morph2[,2:9])

corrplot(corrMPSUB2, method = "square", tl.col = "black", 
         tl.srt = 45, title = "Performance/LinearM", 
         tl.cex=1, mar=c(0,0,1,0), cl.cex=1, diag = FALSE, cl.pos='n')


grid.arrange(MorphologyPCA2,pSub2,head2, caudalBW2, ncol=2)

grid.arrange(MorphologyPCA,head, MorphologyPCA2,pSub2, ncol=2)
             

