library(factoextra)
library(cluster)
data("USArrests")
head(USArrests)
USArrests <- scale(USArrests)
head(USArrests)
set.seed(123)
crime <- sample(1:50,10)

crime_1 <- USArrests[crime,]
head(crime_1)

euc_dist <- dist(crime_1, method = "euclidean")
head(euc_dist)
wss <- sapply(1:crime, 
              function(k){kmeans(USArrests, k, nstart=20,iter.max = 15)$tot.withinss})

fviz_nbclust(USArrests, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype=5, col= "darkred")

km.res <- kmeans(USArrests, 4, nstart = 20)
km.res
df_member <- cbind(USArrests, cluster = km.res$cluster)
head(df_member,10)

fviz_cluster(km.res, data = USArrests,
            palette=c("red", "blue", "black", "darkgreen"),
            ellipse.type = "euclid",
            star.plot = T,
            repel = T,
            ggtheme = theme())
