# CLUSTERING ALGORITHM 

# obtains the dissimilarity matrix for gower distances between both numerical and categorical values 
gower_dist <- daisy(data, metric="gower", weights=c(1:28))

# uses PAM clustering method to find optimal number of k clusters 
for(i in 2:10) {
   pam_fit <- pam(gower_dist, diss=TRUE, k=i) 
   sil_width[i] <- pam_fit$silinfo$avg.width
}
# give results of average silhouette width per k clusters in a plot
plot(1:10, sil_width, xlab = "Number of clusters", ylab = "Silhouette width")
lines(1:10, sil_width)

# visualizing clusters that we calculated with gower_dist, PAM)
dist <- gower_dist
pam_fit <- pam(gower_dist, diss=TRUE, k = 2) # using optimal num k
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE) # perplexity is either N^(1/2) or R-1/3
tsne_data <- tsne_obj$Y %>%
   data.frame() %>%
   setNames(c("X", "Y")) %>%
   mutate(cluster=factor(pam_fit$clustering))
ggplot(aes(x=X, y=Y), data=tsne_data) + geom_point(aes(color=cluster))

# PREDICTIVE MODELING COMPONENT

