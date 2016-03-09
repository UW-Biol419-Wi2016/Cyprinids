
#table to be use
Perf<-read.csv('Cypr_Perf.csv')


# Select the number of rows of the desired species
#grep the exact value that you want '^i$'
#use rbind to bombine the tables per species

#1. Different Morphology, same Microhabitat, same clade
SUBset1<-rbind(Perf_Sp[grep('^3$',Perf_Sp$species),],
        Perf_Sp[grep('^17$',Perf_Sp$species),],
        Perf_Sp[grep('^5$',Perf_Sp$species),])

#2. Different Morphology, different Microhabitat, same clade
SUBset2<-rbind(Perf_Sp[grep('^1$',Perf_Sp$species),],
              Perf_Sp[grep('^4$',Perf_Sp$species),],
              Perf_Sp[grep('^6$',Perf_Sp$species),])

#3. Same Morphology, different Microhabitat, same clade
SUBset3<-rbind(Perf_Sp[grep('^7$',Perf_Sp$species),],
              Perf_Sp[grep('^8$',Perf_Sp$species),])

#4. Same Morphology, same Microhabitat, same clade
SUBset4<-rbind(Perf_Sp[grep('^3$',Perf_Sp$species),],
              Perf_Sp[grep('^14$',Perf_Sp$species),])

###Figure iit our...how to loop to subset??
SUB<-c('3','5','17')
for (i in seq(SUB)){
  SUBset<-Perf_Sp[grep('^i$',Perf_Sp$species),]
  print(SUBset)
}