########Clustering analysis############
#source http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/

#### function to test what is the best number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data))*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

###Run the function on your own data
wssplot(Perf_Num)

### Packages needed for NbClust
install.packages('NbClust')
library(NbClust)

##Set a random seed
set.seed(1234)
#calculate the best number of clusters using dif index  and plot
nc <- NbClust(Perf_Num, min.nc=2, max.nc=15, method="kmeans")
## next table indicate how many indices support each number of clusters
table(nc$Best.n[1,])

##asignation of each sample to a cluster (using the best number of clusters)
nc$Best.partition

#bar plot to clarified the best number of clusters
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

##Run kmeans with the suggested number of clusters
set.seed(1234)
fit.km <- kmeans(Perf_Num, 2, nstart=25) 
##number of samples included in each cluster
fit.km$size
###cluster id for each sample
fit.km$cluster
#### you can select the info that you want to know about the clusters for example the center
fit.km$centers

# agregatte the cluster id to the main table ###I have not try this one yet.. 
##you can also only use the vector fit.km$cluster
aggregate(Perf_Num[-1], by=list(cluster=fit.km$cluster), mean)

#plot the clusterin in a pca to visualize in 2 dimensions
ggbiplot(Perfpca, choices=1:2, groups = nc$Best.partition, 
         labels= Perf_Sp$species, scale=1, ellipse = T,
         legend.text=nc$Best.partition)
