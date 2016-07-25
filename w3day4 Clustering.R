#Clustering Practice

library("readr")
library("cluster")
library("pvclust")
library("fpc")
library("datasets")
library("mixtools")
library("mclust")

# Load the data, separate the features and scale them
df = read_delim("protein.csv", delim = "\t")
df_scale = data.frame(scale(df[,colnames(df) != "Country"]))
rownames(df_scale) = df[["Country"]]

# Create a distance matrix on the scaled features
dist_matrix = dist(df_scale, method="euclidean")

?hclust
ward_obj = hclust(dist_matrix, method = "ward.D2")
plot(ward_obj)
str(ward_obj)

# Broken function, could be made to display groups clustered by foods
print_clusters = function(df,hclust_object_labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(df[hclust_object_labels == i,c("RedMeat","Fish", "Fr.Veg")])
  }
}

test_obj = print_clusters(df_scale, ward_obj$labels, k=4)


# Cut the dendrogram into set number of groups (k)
ward_obj_cut = cutree(ward_obj, k=4)
sort(ward_obj_cut)


# Clusplot for use on the hclust object
clusplot(df_scale, ward_obj_cut, color = TRUE, shade = TRUE, labels = 2, lines = 0)

hclust_plot = function(df_scaled, method, k) {
  d = dist(df_scaled, method = "euclidean")
  hclust_obj = hclust(d, method)
  hclust_obj_cut = cutree(hclust_obj, k)
  clusplot(df_scaled, hclust_obj_cut, color = TRUE, shade = TRUE, labels = 2, lines = 0)
}

hclust_plot(df_scale, "ward.D2", 4)

pvclust_obj = pvclust(t(df_scale), method.hclust = "ward.D2", method.dist = "euclidean")
pvclust_obj
plot(pvclust_obj)
pvrect(pvclust_obj, alpha = 0.985)

?pvrect
?pvclust


# K-means clustering
kmean_obj = kmeans(df_scale, centers=4)
sort(kmean_obj$cluster)

clusplot(df_scale, kmean_obj$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

outlier_vec = c(0, rep(1, nrow(df_scale)-1))
fake_outlier_df = cbind(df_scale, outlier_vec)
single_outlier_df = df_scale
single_outlier_df[1,1] = 9
kmean_outlier = kmeans(fake_outlier_df, centers = 4)
clusplot(fake_outlier_df, kmean_outlier$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
kmean_outlier2 = kmeans(single_outlier_df, centers = 4)
clusplot(single_outlier_df, kmean_outlier2$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# Validating k-means choice of K
kmeansruns_obj = kmeansruns(df_scale, krange = 1:10, criterion = "asw", critout = TRUE)
kmeansruns_obj2 = kmeansruns(df_scale, krange = 1:10, criterion = "ch", critout = TRUE)
kmeansruns_obj$crit
kmeansruns_df = data.frame(cbind(kmeansruns_obj$crit, kmeansruns_obj2$crit))
ggplot(data = kmeansruns_df) + geom_point(aes(x=1:10,kmeansruns_df[1]), color= "blue")

ggplot()+ geom_point(data = kmeansruns_df,aes(x=1:10,kmeansruns_df[2]), color = "red")

# Clusterboot to resample with replacement (bootstrapping kmeans)
clus_obj = clusterboot(df_scale, clustermethod = kmeansCBI, runs = 100, iter.max = 100, krange=5)

clus_obj$bootmean
clus_obj$bootbrd
str(clus_obj)

# Mixture Models
df_faithful = faithful
str(df_faithful)
qplot(df_faithful$waiting)
qplot(df_faithful$eruptions)
mix_obj = normalmixEM(df_faithful$waiting, k=2)
plot(mix_obj, density = TRUE, which = 2)
summary(mix_obj)

?faithful

mix_obj2 = normalmixEM(df_faithful$eruptions, k=2)
plot(mix_obj2, density = TRUE, which = 2)

# semiparametric fitting
semipara_obj = spEMsymloc(df_faithful$waiting, mu0 = 2, bw = 2)
plot(semipara_obj)

df_faithful_fake = df_faithful
df_faithful_fake[1,2] = 120

semipara_obj2 = spEMsymloc(df_faithful_fake$waiting, mu0 = 2, bw = 2)
plot(semipara_obj2)

mix_obj2 = normalmixEM(df_faithful_fake$waiting, k=2)
plot(mix_obj2, density = TRUE, which = 2)


qplot(df_faithful$eruptions, df_faithful$waiting)

# McLust (mClust)
# fitting mixture of multivariate normal distributions
mclust_obj = Mclust(scale(df_faithful))
plot(mclust_obj)

mclust_protein = Mclust(df_scale)
plot(mclust_protein)
sort(mclust_protein$classification)

# Nonparametric
np_obj = npEM(df_faithful, mu0 = 2, bw = 2)
plot(np_obj)

np_obj2 = npEM(df_scale, mu0 = 4, bw = 3)
plot(np_obj2)

