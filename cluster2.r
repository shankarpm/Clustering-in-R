library(dplyr)   
library(NbClust)
library(magrittr)
library(caret)
library(e1071)
library(mlbench)
library(caret) 

setwd("C:/Users/Shankar/Documents/R Docs/Projects/RealEstateCluster")
data<-read.csv("data_sales.csv",head=T,sep=",")
data<-na.omit(data) 
nrow(data) 

features <- c('TOTAL_ITEM','ORIGIN_ITEM_IMP','ORIGIN_ITEM_DOM','CAT_ITEM_WINE','CAT_ITEM_SPIRITS',
              'CAT_ITEM_BEER','CAT_ITEM_REFRESHMENT_BEVERAGE','MONTHLY_ITEM_1','MONTHLY_ITEM_2',
              'MONTHLY_ITEM_3','MONTHLY_ITEM_4','MONTHLY_ITEM_5','MONTHLY_ITEM_6','MONTHLY_ITEM_7',
              'MONTHLY_ITEM_8','MONTHLY_ITEM_9','MONTHLY_ITEM_10','MONTHLY_ITEM_11','MONTHLY_ITEM_12',
              'RECLS_ITEM_POPULAR_WINE','RECLS_ITEM_DELUXE_BEER','RECLS_ITEM_PREMIUM_WINE','RECLS_ITEM_PREMIUM_SPIRIT',
              'RECLS_ITEM_DELUXE_SPIRIT','RECLS_ITEM_DELUXE_WINE','RECLS_ITEM_ULTRA_PREMIUM_WINE','RECLS_ITEM_PREMIUM_BEER',
              'RECLS_ITEM_ECONOMY_WINE','RECLS_ITEM_ECONOMY_SPIRIT','RECLS_ITEM_PREMIUM_REF_BEV','RECLS_ITEM_SUPER_PREMIUM_WINE',
              'RECLS_ITEM_ECONOMY_BEER','RECLS_ITEM_ECONOMY_REF_BEV','RECLS_ITEM_DELUXE_REF_BEV','RECLS_ITEM_SUPER_DELUXE_WINE',
              'AGE')


data_filtered<- data[,features]
summary(data_filtered)
head(data_filtered)  
################ find the number of clusters#######################
# df.data <- scale(data[,1:14])
# set.seed(1234)
# nc <- NbClust(df.data, min.nc=2, max.nc=15, method="kmeans")
# 
# barplot(table(nc$Best.n[1,]), 
#         xlab="Numer of Clusters", ylab="Number of Criteria",
#         main="Number of Clusters Chosen by 26 Criteria")
# 
# table(nc$Best.n[1,])
# 
#  for(i in  c(1:25)) 
#  {
#    km<-kmeans(data_new,i,nstart=25,iter.max=30);
#    print(km$tot.withinss)
#    print(i)
#  }
# warnings()
################ find the number of clusters#######################
#library(cluster)   
#Silhouette analysis for determining the number of clusters
# library(fpc)
# asw <- numeric(20)
# for (k in 2:20)
#   asw[[k]] <- pam(head(data_new,60000), k) $ silinfo $ avg.width
# k.best <- which.max(asw)
# 
# cat("silhouette-optimal number of clusters:", k.best, "\n")
# plot(pam(d, k.best))


########## Hierarchical  clustering    ########## 
hd <- dist(data_filtered[1:10000,], method = "euclidean") 
H.fit <- hclust(hd, method="ward.D")  
hcluster <- cutree(H.fit, k=5)
u = union(data[,'Cluster'], hcluster) 
hcm <- confusionMatrix(table(factor(hcluster, u), factor(data[1:10000,'Cluster'], u))) 
hierar_output<-cbind(data[1:10000,],hcluster) 

print(paste("Hierarchical Accuracy -" ,  hcm$overall[1])) 
write.csv(hierar_output,'data_sales_hcluster.csv' ,row.names = FALSE)
########## kmeans Clustering ###############
km<-kmeans(data_filtered,15)
km$size


adj_rquare <- km$betweenss / km$totss
km$size
km$cluster 
km$
confusionMatrix ( table(km$cluster,data[,'Cluster'])  )
kmeans_cluster <- km$cluster
kmeans_output<-cbind(data,kmeans_cluster) 
cm <- confusionMatrix(table(factor(km$cluster),factor(data[,'Cluster'])))

print(paste("Kmeans Accuracy -" ,  cm$overall[1]))
write.csv(kmeans_output,'data_sales_kmeans.csv' ,row.names = FALSE)

head(kmeans_output)
###dendo gram for  Hierarchical  clustering    
clusters <- hclust(dist(data_filtered[1:100,]), method = 'euclidean')
plot(clusters) 
 

hc <- hclust(hd) 
plot(hc)
#group by buyers
clusterMeanData <-  kmeans_output  %>% group_by(kmeans_cluster) %>% summarise(Buyers = length(kmeans_cluster), 
                                                                              MeanMonthlyTrx_1 = round(mean(MONTHLY_TRX_1),1), MeanMonthlySales_1 = round(mean(MONTHLY_SALES_1),1),
                                                                              MeanTOTAL_TRX =  round(mean(TOTAL_TRX),1), MeanTOTAL_ITEM =  round(mean(TOTAL_ITEM),1),
                                                                              MeanTotalSales =  round(mean(TOTAL_SALES),1), MeanORIGIN_SALES_IMP =  round(mean(ORIGIN_SALES_IMP),1),
                                                                              MeanCAT_TRX_WINE =  round(mean(CAT_TRX_WINE),1), MeanCAT_TRX_SPIRITS =  round(mean(CAT_TRX_SPIRITS),1),
                                                                              MeanCAT_TRX_BEER =  round(mean(CAT_TRX_BEER),1), MeanCAT_TRX_REFRESHMENT_BEVERAGE =  round(mean(CAT_TRX_REFRESHMENT_BEVERAGE),1),
                                                                              MeanCAT_ITEM_WINE =  round(mean(CAT_ITEM_WINE),1), MeanCAT_ITEM_SPIRITS =  round(mean(CAT_ITEM_SPIRITS),1),
                                                                              MeanCAT_ITEM_BEER =  round(mean(CAT_ITEM_BEER),1), MeanCAT_ITEM_REFRESHMENT_BEVERAGE =  round(mean(CAT_ITEM_REFRESHMENT_BEVERAGE),1),
                                                                              MeanCAT_SALES_WINE =  round(mean(CAT_SALES_WINE),1), MeanCAT_SALES_WINE =  round(mean(CAT_SALES_WINE),1),
                                                                              MeanCAT_SALES_SPIRITS =  round(mean(CAT_SALES_SPIRITS),1), MeanCAT_SALES_BEER =  round(mean(CAT_SALES_BEER),1),
                                                                              MeanCAT_SALES_REFRESHMENT_BEVERAGE =   round(mean(CAT_SALES_REFRESHMENT_BEVERAGE),1)
)   %>% as.data.frame()

#################radar chart#######################
library(fmsb)
cmd <- clusterMeanData[,c(1:6)] %>% as.data.frame()
 
COL<-colorRampPalette(c("red", "blue"))(nrow(cmd)-2) 

radarchart(cmd, pcol = COL, cglcol = "grey80", seg = 10, title = "sun spots") 
legend(2, 1, legend = levels(as.factor(cmd$kmeans_cluster)), title = "Sales", col = COL, seg.len = 2,
       border = "transparent", pch = 16, lty = 1)


radarchart(cmd, axistype=1, seg=4, plty=1,   
           title="(axis=1, 5 segments, with specified vlabels)", vlcex=0.5)
radarchart(cmd, axistype=3, pty=32, plty=1, axislabcol="grey", na.itp=FALSE,
           title="(no points, axis=3, na.itp=FALSE)")
radarchart(cmd, axistype=2, pcol=topo.colors(3), plty=1, pdensity=c(5, 10, 30), pangle=c(15, 45, 120),
           pfcol=topo.colors(3), title="(topo.colors, fill, axis=2)")

#################radar chart#######################