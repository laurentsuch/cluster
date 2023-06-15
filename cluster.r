data <- testdat
daisy_dist <- daisy(data, metric="gower", weights=c(1:28))
view(daisy_dist)
Showing the first 1000 rows.
> fviz_nbclust(data, kmeans, method="silhouette")
Error in do_one(nmeth) : NA/NaN/Inf in foreign function call (arg 1)
In addition: Warning messages:
1: In stats::dist(x) : NAs introduced by coercion
2: In storage.mode(x) <- "double" : NAs introduced by coercion
> data <- na.omit(data)
> fviz_nbclust(data, kmeans, method="silhouette")
Error in do_one(nmeth) : NA/NaN/Inf in foreign function call (arg 1)
In addition: Warning messages:
1: In stats::dist(x) : NAs introduced by coercion
2: In storage.mode(x) <- "double" : NAs introduced by coercion
> fviz_nbclust(daisy_dist, method="silhouette")
Error in fviz_nbclust(daisy_dist, method = "silhouette") : 
  x should be an object of class matrix/data.frame or an object created by the function NbClust() [NbClust package].
> dist <- daisy_dist
> pamx <- pam(dist, 3)
> sil <- silhouette(pamx$clustering, dist)
> plot(sil)
> pamx <-pam(dist, 4)
> sil <- silhouette(pamx$clustering, dist)
> plot(sil)
> pamx <- pam(dist, 5)
> sil <- silhouette(pamx$clustering, dist)
> plot(sil)
> pamx <- pam(dist, 6)
> sil <- silhouette(pamx$clustering, dist)
> plot(sil)
> pamx <- pam(dist, 7)
> sil <- silhouette(pamx$clustering, dist)
> plot(sil)
> pam_fit <- pam(dist, 6)
> pam_results <- testdat %>%
+ mutate(cluster = pam_fit$clustering) %>%
+ group_by(cluster) %>%
+ do(the_summary = summary(.))
> pam_results$the_summary

> tsne_obj <- Rtsne(dist, perplexity=28, is_distance=TRUE)


