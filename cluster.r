data <- testdat
daisy_dist <- daisy(data, metric="gower", weights=c(1:28))
# obtains the dissimilarity matrix for gower distances between both numerical and categorical values 
view(daisy_dist)

dist <- daisy_dist
pamx <-pam(dist, 4)
sil <- silhouette(pamx$clustering, dist)
plot(sil)
pamx <- pam(dist, 5)
sil <- silhouette(pamx$clustering, dist)
plot(sil)
pamx <- pam(dist, 6)
sil <- silhouette(pamx$clustering, dist)
# using sihouette plots, find best amount of clusters 
# average silhouette width: closer to one the best fit

> pam_fit <- pam(dist, 6)
> pam_results <- testdat %>%
+ mutate(cluster = pam_fit$clustering) %>%
+ group_by(cluster) %>%
+ do(the_summary = summary(.))
> pam_results$the_summary

> tsne_obj <- Rtsne(dist, perplexity=28, is_distance=TRUE)

# perplexity is either N^(1/2) or R-1/3
