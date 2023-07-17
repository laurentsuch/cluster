# BELOW is the helper code for clustering algorithm and predictive piece to shorten our survey
# Not all steps are necessary for analysis; read comments for necessary implementation 
# This is NOT complete 

library(h2o)
library(cluster)
library(Rtsne)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(FactoMineR)
library(dpylr)
library(NbClust)
library(openxlsx)
library(nnet)
library(caret)


# FOLLoW THIS METHOD:
# switch column "Approx how many hours of.." to column Z
df <- df %>% mutate_if(is.character, as.factor)
df[1] <- lapply(df[1], ordered, levels=c("One of my favorite NHL teams", "Second favorite NHL team", "Favorite NHL Team"))
df[3] <- lapply(df[3], ordered, levels=c("18 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "75 or older"))
# column 4 = change none to 0 and more than 5 to 6
# convert 6-10 to 0/1
df[11] <- lapply(df[11], ordered, levels=c("I started following during the 2023 Stanley Cup Playoffs", "I only recently started to follow the NHL (fewer than 6 years ago)",
                                           "I started following when the Golden Knights came to Vegas", "I started following many years ago (more than 6 years ago)", "I have been a fan my whole life"))
df[12] <- lapply(df[12], ordered, levels=c("I do not follow the NHL, just my favorite team", "I follow the NHL somewhat, but I couldn't name more than some teams and players", "I follow the NHL closely, and could 
                                           talk about teams and players having a stong season", "I follow the NHL very closely, and could recount the standings as well as major events happening around the league"))
df[13:25] <- lapply(df[13:25], ordered, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
df[26] <- lapply(df[26], ordered, levels=c("<5 hours", "6-10 hours", "11-15 hours", "16-20 hours", ">20 hours"))
df[27] <- lapply(df[27], ordered, levels=c("I did not follow them at all", "I casually followed them as part of the local news", "I actively followed them and can discuss team news in conversation", 
                                           "I followed them closely, they are my favorite team", "I followed them very closely, I am emotionally invested in them"))
df[28:38] <- lapply(df[28:38], ordered, levels=c("Not important at all", "Not important", "Neutral", "Important", "Extremely important"))
df[41] <- lapply(df[41], ordered, levels=c("Fair Weather - My support for the team varies greatly depending on team performance.", "Somewhat Fair Weather - My support for the team varies somewhat depending on team performance.", 
                                           "Neutral", "Somewhat Die-hard - My support for the team will remain strong, win or lose.", "Die-hard - My support for the team will remain very strong, win or lose."))
df[42] <- lapply(df[42], ordered, levels=c("I do not live in the Las Vegas area", "Less than 2 years", "2-5 years", "5-10 years", ">10 years", "Born and raised"))
# col 43 - change to numerical - maybe NAs to 0?
df[45:54] <- lapply(df[45:54], ordered, levels=c("No impact", "Slightly more interested", "A lot more interested"))
df[56:62] <- lapply(df[56:62], ordered, levels=c("Not important at all", "Not very important", "Slightly important", "Extremely important"))
df[68:72] <- lapply(df[68:72], ordered, levels=c("Never", "Seldom", "Sometimes", "Often", "Almost Always"))
# renames columns 64-67


# CLEANING DATA
df <- df %>% mutate_if(is.character, as.factor) # converts all characters to factors
ncol(df) # finds number of columns 
df <- df %>% mutate(across(where(is.numeric), scale)) # scaling only numerical values in data frame
# SAVE AN EXCEL SHEET WITH FINALIZED DATA MEMBERS AFTER CLEANING (MUST DO!!!)


# CLUSTERING ALGORITHM 
# obtains the dissimilarity matrix for gower distances between both numerical and categorical values 
gower_dist <- daisy(data, metric="gower", weights=c(1:28))

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
var
head(var$coord)
fviz_famd_var(res.famd, repel=TRUE)
fviz_contrib(res.famd, "var", axes = 1)
fviz_contrib(res.famd, "var", axes = 2)

# multinomial linear regression model 
total <- cbind(cluster.df, df)
multinom.fit <- multinom(cluster.df ~ ., data = df)
summary(multinom.fit)
z <- (summary(test)$coefficients)/(summary(test)$standard.errors)
p <- (1 - pnorm(abs(z), 0, 1)) * 2
exp(coef(test))
impVar <- varImp(multinom.fit)


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
comparegroups.main
comparegroups.main.table = createTable(x = comparegroups.main, show.all=T)
comparegroups.main.table 
comparegroups.html = supressWarnings(export2md(x=comparegroups.main.table, caption=""))

#cluster FAMD analysis 
princomp <- FAMD(total.total, graph = FALSE) # if missing data, imputeFAMD(total.total) 
fviz_contrib(princomp, choice="var", axes=1, top=10, sort.val=c("desc"))

# cluster analysis and stats 
library(fpc)
cluster.stats(gowdist, clusternum) # clusternum must be numeric - cannot be factors 
