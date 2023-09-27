getwd()
#data=read.csv("D:/MSA2-Sem 3/Assignment-2_OG_data.csv")
#View(data)
# Scale variables
#dt = scale(data) #complete datasets

dt=read.csv("measure1_smartwatch_sens_clean.csv")
dt=dt[,-1]
View(dt)
#Libraries
library(dplyr)
library(purrrlyr)
library(purrr)
library(factoextra)

#````````````````````````````````````````````````````````````````````````````````````````````````
#1#####
df=dt %>% as.data.frame()
head(df)
## elbow method
wss <- function(k) {
  kmeans(df, k, nstart = 10 ,iter.max = 1000)$tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#``````````````````````````````````````````````````````````````````````````
#2#####
set.seed(1234)
#For different optimal of k we will choose 
k3<-kmeans(df,3,nstart=200)
c1<-k3$cluster 
table(c1)

k4<-kmeans(df,4,nstart=200)
c2<-k4$cluster 
table(c2)

k5<-kmeans(df,5,nstart=200)
k5
c3<-k5$cluster 
table(c3)

k6<-kmeans(df,6,nstart=200)
c4<-k6$cluster 
table(c4)

#plot
fviz_cluster(k3, data = df)
fviz_cluster(k4, data = df)
fviz_cluster(k5, data = df)
fviz_cluster(k6, data = df)

#length(c[which(c==1)])
#length(c[which(c==2)])
#length(c[which(c==3)])
#length(c[which(c==4)])
#````````````````````````````````````````````````````````````````````````````
#3#####                                                                                                                         
#for cluster size 5

a1=aov(dt$timestamp~ k5$cluster)
a2=aov(dt$AccelerationX~ k5$cluster)
a3=aov(dt$AccelerationY~ k5$cluster)
a4=aov(dt$AccelerationZ~ k5$cluster)
a5=aov(dt$MagneticFieldX~ k5$cluster)
a6=aov(dt$MagneticFieldY~ k5$cluster)
a7=aov(dt$MagneticFieldZ~ k5$cluster)
a8=aov(dt$Z.AxisAgle.Azimuth.~ k5$cluster)
a9=aov(dt$X.AxisAngle.Pitch.~ k5$cluster)
a10=aov(dt$Y.AxisAngle.Roll.~ k5$cluster)
a11=aov(dt$GyroX~ k5$cluster)
a12=aov(dt$GyroY~ k5$cluster)
a13=aov(dt$GyroZ~ k5$cluster)
summary(a1)
summary(a2)
summary(a3)
summary(a4)
summary(a5)
summary(a6)
summary(a7)
summary(a8)
summary(a9)
summary(a10)
summary(a11)
summary(a12)
summary(a13)



#now we will remove the variables that are insignificant (GyroY,GyroZ)
new_dt=dt[,-c(11,13)]
View(new_dt)

## elbow method
wss <- function(k) {
  kmeans(new_dt, k, nstart = 10 ,iter.max = 1000)$tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(1234)
k3_new<-kmeans(new_dt,3,nstart=200)
c1<-k3_new$cluster 
table(c1)

k4_new<-kmeans(new_dt,4,nstart=200)
c2<-k4_new$cluster 
table(c2)

k5_new<-kmeans(new_dt,5,nstart=200)
k5_new
c3<-k5_new$cluster 
table(c3)


#length(c[which(c==1)])
#length(c[which(c==2)])
#length(c[which(c==3)])
#length(c[which(c==4)])

#fviz_cluster(k3_new, data = df)
#fviz_cluster(k4_new, data = df)
fviz_cluster(k5_new, data = df)


#Q5#####
d=cor(dt,use="complete.obs")
E=eigen(d)
pca=prcomp(d,center = T,scale. = T)
summary(pca)
screeplot(pca, type = "lines")
vec=E$vectors
vec
which(E$values>1)
val=E$values
val
val_1=val[c(1:4)]
val_1
vec_1=vec[,1:4]
vec_1
pca1=as.matrix(dt)%*%vec_1
summary(pca1)
View(pca1)
head(pca1)
#PCA dataset=Pca1

#6####
wss_1 <- function(k) {
  kmeans(pca1, k, nstart = 10 ,iter.max = 1000)$tot.withinss
}

k.values <- 1:10

wss_values1 <- map_dbl(k.values, wss_1)

plot(k.values, wss_values1,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




#7#####
set.seed(1234)
k5_pca=kmeans(pca1,5,nstart=200)
k5_pca
c<-k5_pca$cluster
table(c)

fviz_cluster(k5_pca, data = pca1)

#for cluster size 3
#set.seed(1234)
k3_pca=kmeans(pca1,3,nstart=200)

c<-k3_pca$cluster
table(c)
#fviz_cluster(k3_pca, data = pca1)


#Q8(I)#####


View(new_dt)
ddt=cor(new_dt,use="complete.obs")
E=eigen(ddt)
pca2=prcomp(ddt,center = T,scale. = T)
summary(pca2)
screeplot(pca2, type = "lines")
vec1=E$vectors
vec1
which(E$values>1)
val=E$values
val
val_1=val[c(1:3)]
val_1
vec_1=vec[,1:3]
vec_1
pca_new=as.matrix(dt)%*%vec_1
summary(pca_new)
View(pca_new)
pca_new=pca_new%>%as.data.frame()
head(pca_new)
#pca_new is pca dataset with significant variables

wss_1 <- function(k) {
  kmeans(pca_new, k, nstart = 10 ,iter.max = 1000)$tot.withinss
}

k.values <- 1:10

wss_values1 <- map_dbl(k.values, wss_1)

plot(k.values, wss_values1,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(1234)
k5_pca_new=kmeans(pca_new,5,nstart=200)
k5_pca_new
c<-k5_pca_new$cluster
table(c)

fviz_cluster(k5_pca_new, data = pca_new)






#Q8(ii)#####
#anova on pca dataset
pca1=pca1%>% as.data.frame()
b1=aov(pca1$V1~k5_pca$cluster)
b2=aov(pca1$V2~k5_pca$cluster)
b3=aov(pca1$V3~k5_pca$cluster)
b4=aov(pca1$V4~k5_pca$cluster)

summary(b1)
summary(b2)
summary(b3)
summary(b4)



