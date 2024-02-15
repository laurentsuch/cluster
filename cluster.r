# helper code for clustering algorithm and predictive piece to shorten our survey


library(h2o)
library(cluster)
library(Rtsne)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(FactoMineR)
library(dplyr)
library(NbClust)
library(openxlsx)
library(nnet)
library(caret)


# DATA CLEANING 
# switched from char to factors 
# ordered factors below 
df <- df %>% mutate_if(is.character, as.factor)

# CLEANING DATA
df <- df %>% mutate_if(is.character, as.factor) # converts all characters to factors
ncol(df) # finds number of columns 
df <- df %>% mutate(across(where(is.numeric), scale)) # scaling only numerical values in data frame
# SAVE AN EXCEL SHEET WITH FINALIZED DATA MEMBERS AFTER CLEANING 


# CLUSTERING ALGORITHM 
# obtains the dissimilarity matrix for gower distances between both numerical and categorical values 
gower_dist <- daisy(data, metric="gower")

# uses PAM clustering method to find optimal number of k clusters 
sil_width <- c(NA)
for(i in 2:10) {
   pam_fit <- pam(gower_dist, diss=TRUE, k=i) 
   sil_width[i] <- pam_fit$silinfo$avg.width
}
# give results of average silhouette width per k clusters in a plot
plot(1:10, sil_width, xlab = "Number of clusters", ylab = "Silhouette width")
lines(1:10, sil_width)

# OR we can plot each average silhouette width depending on k clusters and compare average silhouette width 
pam.res <- pam(gower_dist, k)
sil.res <- silhouette(pam.res$clustering, gower_dist)
plot(sil.res) #fviz_silhouette(sil.res) 

# OR we could find optimal number of k clusters with gap statistic or WSS
fviz_nbclust(as.matrix(gower_dist), pam, method = "wss")
fviz_nbclust(as.matrix(gower_dist), pam, method ="gap_stat")

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
# famd 
res.famd <- FAMD(df) # FAMD takes mixed data 
print(res.famd)
fviz_screeplot(res.famd)
var <- get_famd_var(res.famd)
fviz_contrib(res.famd, "var", axes = 1)
fviz_contrib(res.famd, "var", axes = 2) # finds important variables that contribute 

# multinomial linear regression model 
total <- cbind(cluster.df, df)
multinom.fit <- multinom(cluster.df ~ ., data = total) # first will all variables to find important variables / p vals
summary(multinom.fit)
z <- (summary(test)$coefficients)/(summary(test)$standard.errors)
p <- (1 - pnorm(abs(z), 0, 1)) * 2
impVar <- varImp(multinom.fit)
predictions <- predict(multinom.fit, df) # find predictive and accuracy %


# OPTIONAL HIERARCHIAL CLUSTERING
div.clust <- diana(as.matrix(gower_dist), diss = TRUE, keep.diss = TRUE)
plot(div.clust, main = "Divisive") # divisive clustering model; top-bottom approach
aggl.clust <- hclust(gower_dist, method = "complete")
plot(aggl.clust, main = "Agglomerative") # agglomerative clustering model; bottom-top approach


# ANALYZING RESULTS OF THE CLUSTER ANALYSIS 
cluster_num <- tsne_data$cluster
cluster_num <- as.matrix(cluster_num)
cluster_num <- data.frame(cluster_num)
write.xlsx(cluster_num, file="clusterAssignment") # writes an excel file to list all cluster assignments 


# to find cluster summaries: 
# add a cluster num column to data set combine the two using cbind 
library(compareGroups)
comparegroups.main = compareGroups(formula=Group ~ ., data=total.c)
comparegroups.main.table = createTable(x = comparegroups.main, show.all=T) 
comparegroups.html = suppressWarnings(export2md(x=comparegroups.main.table, caption=""))

# cluster FAMD analysis 
princomp <- FAMD(total.total, graph = FALSE) # if missing data, imputeFAMD(total.total) 
fviz_contrib(princomp, choice="var", axes=1, top=10, sort.val=c("desc"))

# cluster analysis and stats 
library(fpc)
cluster.stats(gowdist, clusternum) # clusternum must be numeric - cannot be factors 

# to plot one variable 
ggplot(total) + aes(x=clustnum, fill=total$i) + geom_bar()

# classification model 
rfmodel <- randomForest(clustnumdf ~ ., data=imputed.df, importance=TRUE)
randomForest::varImpPlot(rfmodel, sort=TRUE)
