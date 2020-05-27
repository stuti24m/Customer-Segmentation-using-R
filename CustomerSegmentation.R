# ------------------------Data Exploration

data = read.csv("C:\\Users\\subroto mittra\\Desktop\\STUTI\\R\\customer-segmentation-dataset\\Mall_customers.csv")
str(data)
names(data)

head(data)

# summary of Age column
summary(data$Age)

# standard deviation
sd(data$Age)

summary(data$Annual.Income..k..)
sd(data$Annual.Income..k..)
summary(data$Age)

sd(data$Spending.Score..1.100.)


#------------------------- Customer Gender Visualization

a = table(data$Gender)
barplot(a, main = "Barplot for Gender Comparison",
        legend = rownames(a), col = rainbow(2),
        xlab = "Gender", ylab = "Count")
# insight = number of females is mor ethan number of males

# Pie chart to observe the raio of male against females


#install.packages(plotrix)
pct = round(a/sum(a)*100)
lbs = paste(c("Female","Male")," ",pct,"%",sep = " ")
library(plotrix)
pie3D(a, labels = lbs, main = "Pie chart depicting Ratio of Females vs Males")


# Visualization of Age distribution
summary(data$Age)

hist(data$Age, xlab = "Age Class", ylab = "Frequency", col = "blue", labels = TRUE, 
     main = "Histogram to show the count of age class")

boxplot(data$Age, main = "Boxplot for Age", col = "#ff0066")

# Analysis of Annual Income of the Customers

summary(data$Annual.Income..k..)
hist(data$Annual.Income..k.., main = "Histogram for annual Income",
     col = "#660033", xlab = "Annual Income Class", ylab = "Frequency",
     labels = TRUE)

# Density plot of the same

plot(density(data$Annual.Income..k..), col = "blue",
     main = "Density plot for annual income", xlab = "Annual Income Class",
     ylab = "Density")
polygon(density(data$Annual.Income..k..), col = "#ccff66")


# Analyzing Spending Score of the Customers

summary(data$Spending.Score..1.100.)
boxplot(data$Spending.Score..1.100., 
        main = "Boxplot for descriptive analysis for spending income",
        horizontal = TRUE, col = "#ff0654")
hist(data$Spending.Score..1.100., 
     main = "Histogram for descriptive analysis for spending income",
     xlab = "Spending Score Class ", ylab = "Frequency",
     labels = TRUE, col ="#f09865")


# ----------------------------------K-means Algorithm

# To determine optimal clusters we would use 3 methods for the same
# 1. Elbow Method 
# 2. Silhouette Method
# 3. Gap Statistic

library(purrr)
# It takes a vector as input and applies a function to each 
# element of the vector. 

set.seed(123)

# function to generate total intra-cluster sum of square
iss <- function(k){
  kmeans(data[,3:5],k,iter.max = 100, nstart = 100, 
         algorithm = "Lloyd")$tots.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)
plot(k.values,iss_values,frame=FALSE, pch = 20, type = "b",
     xlab = "number of clusters k", 
     ylab = "Total intra-clusters sum of squares")

# Average Silhouette Method
library(cluster)
library(gridExtra)
library(grid)

k2 <-  kmeans(data[,3:5],2,iter.max = 100, nstart = 50, 
              algorithm = "Lloyd")
s2 <- plot(silhouette(k2$cluster,dist(data[,3:5],"euclidean")))


k3 <- kmeans(data[,3:5],3,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s2 <- plot(silhouette(k3$cluster,dist(data[,3:5],"euclidean")))


k4 <- kmeans(data[,3:5],4,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s4 <- plot(silhouette(k4$cluster,dist(data[,3:5],"euclidean")))

# iter.max is the number of times the algorithm will repeat 
# the cluster assignment and moving of centroids.
# nstart is the number of times the initial starting 
# points are re-sampled.
k5 <- kmeans(data[,3:5],5,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s5 <- plot(silhouette(k5$cluster,dist(data[,3:5],"euclidean")))


k6 <- kmeans(data[,3:5],6,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s6 <- plot(silhouette(k6$cluster,dist(data[,3:5],"euclidean")))


k7 <- kmeans(data[,3:5],7,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s7 <- plot(silhouette(k7$cluster,dist(data[,3:5],"euclidean")))


k8 <- kmeans(data[,3:5],8,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s8 <- plot(silhouette(k8$cluster,dist(data[,3:5],"euclidean")))


k9 <- kmeans(data[,3:5],9,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s9 <- plot(silhouette(k9$cluster,dist(data[,3:5],"euclidean")))


k10 <- kmeans(data[,3:5],10,iter.max = 100, nstart = 50, 
             algorithm = "Lloyd")
s10 <- plot(silhouette(k10$cluster,dist(data[,3:5],"euclidean")))

# Now we use fviz_nbclust() to determine and visualize the optimal 
# number of clusters


library(NbClust)
library(factoextra)
library(cluster)
library(cluster.datasets)
# species the number of clustetrs to be generated
fviz_nbclust(data[,3:5], kmeans, method = "silhouette")



# Gap Statistic Method

# compute gap statistic method
set.seed(125)

stat_gap <- clusGap(data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

# taking k = 6 as it fits most appropriate

# compute gap statistics
k6 <- kmeans(data[,3:5],6, nstart = 50, iter.max = 100, algorithm = "Lloyd")
k6


# Visualizing the clustering results using 
# the first two principle components


pcclust = prcomp(data[,3:5],scale = FALSE)
# principle component analysis

summary(pcclust)
pcclust$rotation[,1:2]

# Visualize the cluster
set.seed(1)
ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.))+
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster)))+
  scale_color_discrete(name = " ",breaks = c("1","2","3","4","5","6"), 
                       labels= c("Cluster1","Cluster2","Cluster3",
                       "Cluster4","Cluster5","Cluster6"))+
  ggtitle("Segements of Mall customer", 
          subtitle = "Using K-means clustering")


ggplot(data, aes(x = Spending.Score..1.100., y = Age))+
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster)))+
  scale_color_discrete(name = " ",breaks = c("1","2","3","4","5","6"), 
                       labels= c("Cluster1","Cluster2","Cluster3",
                                 "Cluster4","Cluster5","Cluster6"))+
  ggtitle("Segements of Mall customer", 
          subtitle = "Using K-means clustering")

kCols <- function(vec){
  
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

digCluster <- k6$cluster; 
dignm <- as.character(digCluster)
# k-means cluster

plot(pcclust$x[,1:2], col = kCols(digCluster), pch = 19, xlab = "Kmeans",
     ylab = "Classes")
legend("bottomleft", unique(dignm), fill = unique(kCols(digCluster)))

