setwd("C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Presentation 1 - Hierarchical Clustering")

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


#################################################################
#### Step 1: Data preparation
top10 <- read.csv("top10s.csv")   # Read in the datafile
top10 %>%                         # Summarise frequencies by year
  group_by(year)%>%
  summarise(n=n())

top10_drop <- top10[-443,]          # Remove Adele, obs. 443 as it is invalid
top10_adj <- scale(top10_drop[,6:14])

# Check for double titles
top10_adj <- top10_adj[!duplicated(top10_drop$title),]
#> found 18 duplicates drop them!

########################################################
#### Step 2: Determining the number of clusters
## Method 1: within cluster sums of squares
fviz_nbclust(top10_adj, FUN = hcut, method = "wss")

# After k=5, the line levels off (the elbow).

#---------------------------------------------

## Method 2: Average Silhouette Method
fviz_nbclust(top10_adj, FUN = hcut, method = "silhouette")

# k=2 provides the highest average silhouette width, but as we're interested in more clusters, k=6 provides the second highest average silhouette width.

#---------------------------------------------

## Method 3: Gap statistic method
no_of_Clusters <- fviz_nbclust(top10_adj, FUN = hcut, method = "gap_stat",k.max = 20,nboot=20)
no_of_Clusters
library("NbClust")
no_of_Clusters <- NbClust(top10_adj,method="ward.D")
fviz_nbclust(no_of_Clusters)

no_of_Clusters2 <- NbClust(top10_adj,method="ward.D2")
fviz_nbclust(no_of_Clusters)

#---------------------------------------------

## Create Dendrograms
NbClust(top10_adj,method="single")

d <- dist(top10_adj, method = "euclidean")
hc5 <- hclust(d, method = "ward.D2" )
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 6, border = 2:5)

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute all coefficients and check optimal method.
ac <- function(x) {
  agnes(top10_adj, method = x)$ac
}

map_dbl(m, ac)

par(mfrow=c(2,2))
hc1 <- agnes(top10_adj, method = "ward")
pltree(hc1, cex = 0.6, hang = -1, main = "Dendrogram of agnes Ward") 

hc2 <- agnes(top10_adj, method = "average")
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes Average") 

hc3 <- agnes(top10_adj, method = "complete")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes Complete") 

hc4 <- agnes(top10_adj, method = "single")
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of agnes Single") 


hc5 <- diana(top10_adj)

# Divise coefficient; amount of clustering structure found
hc5$dc

# plot dendrogram
pltree(hc5, cex = 0.6, hang = -1, main = "Dendrogram of diana")
pltree(hc1, cex = 0.6, hang = -1, main = "Dendrogram of agnes Ward") 


#######################################################################
#### Step 3. Perform cluster analysis
library(dendextend)
set.seed(1)
# EXCLUDE/INCLUDE GENRE ??? 
sc_top10 <- top10_adj

# Dissimilarity matrix
d <- dist(sc_top10, method = "euclidean")
set.seed(5)
hc_final <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc_final, k = 6)

# Number of members in each cluster
table(sub_grp)

pltree(hc1, cex = 0.6)
plot(rev(hc_final), cex = 0.6)
pltree(hc5, cex = 0.6)
rect.hclust(hc_final, k = 6, border = 2:5)
fviz_nbclust(sc_top10, FUN = hcut, method = "wss")
fviz_nbclust(sc_top10, FUN = hcut, method = "gap_stat")
?fviz_nbclust

gap_stat <- clusGap(sc_top10, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

fviz_cluster(list(data = sc_top10, cluster = sub_grp))

#---------------------------------------------

#### Compare Dendrograms with each other
all.equal(hc1, hc_final, use.edge.length = TRUE)

library(dendextend)

# Create multiple dendrograms by chaining
dend1 <- top10_adj %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- top10_adj %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- top10_adj %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- top10_adj %>% dist %>% hclust("centroid") %>% as.dendrogram
dend5 <- top10_adj %>% dist %>% hclust("ward.D") %>% as.dendrogram
dend6 <- top10_adj %>% dist %>% hclust("ward.D2") %>% as.dendrogram

plot(dend1)
plot(dend2)
plot(dend3)
plot(dend6)
plot(dend1)

d <- dist(top10_adj, method = "euclidean")
hc1 <- hclust(d^2, method = "single" )
plot(hc1, cex = 0.6)
hc2 <- hclust(d^2, method = "complete" )
plot(hc2, cex = 0.6)
hc3 <- hclust(d^2, method = "average" )
plot(hc3, cex = 0.6)
hc4 <- hclust(d^2, method = "complete" )
plot(hc4, cex = 0.6)
hc5 <- hclust(d, method = "ward.D2" )
plot(hc5, cex = 0.6)

#---------------------------------------------

# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4,
                      "Ward.D" = dend5, "Ward.D2" = dend6)
cors.cophenetic <- cor.dendlist(dend_list, method="cophenetic")
cors.baker <- cor.dendlist(dend_list, method="baker")
# Print correlation matrix
round(cors.cophenetic, 2)
round(cors.baker, 2)

library(corrplot)
corrplot(cors.cophenetic, "pie", "lower")
corrplot(cors.baker, "pie", "lower")


# Get some colors
col <- colorRampPalette(c("darkblue", "white", "darkorange"))(6)
M <- cor(top10_adj)
heatmap(x = M, col = col, symm = TRUE)

dend1 <- top10_adj %>% scale %>% dist %>% 
  hclust("ward.D2") %>% as.dendrogram %>%
  color_branches(h = 6) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2))
# plot the dend in usual "base" plotting engine:
plot(dend1)
color_branches(avg_dend_obj, h = 6)

dend2 <- top10_adj %>% scale %>% dist %>% 
  hclust("ward.D") %>% as.dendrogram %>%
  set("branches_k_color", k=6) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2))
# plot the dend in usual "base" plotting engine:
plot(dend2)

avg_dend_obj <- as.dendrogram(dend6)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)
dend6


######################################################################
#### Compare Cluster Statistics
library(reshape2)
library(ggplot2)
library(RColorBrewer)
sc_top10$Cluster <- sub_grp

top10_drop <- top10_drop[!duplicated(top10_drop$title),]
top10_drop$Cluster <- sub_grp

top10_drop[,6:14] <- scale(top10_drop[,6:14])

subset <- top10_drop %>%
  group_by(Cluster, artist) %>%
  summarize(n = n())

# Calculate the column mean for each Cluster 
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

table <- seg.summ(top10_adj, sub_grp)
colnames(table) <- c("Cluster","Bpm", "Energy", "Dance", "dB", "Live", "Positivity", "Duration", "Acoustic", "Speechness")


# Reshape the results table from wide to long to have categorical data for plotting purposes
table.long <- melt(table,id.vars=c("Cluster"))
# Change column names
colnames(table.long) <- c("Cluster", "Legend", "Value")

# Plot the results
ggplot(table.long, aes(x=Cluster, y=Value, fill=Legend)) +                # Clusters on the x-axis, grouped by Question 16
  geom_bar(stat="identity", position="dodge", col="black", size=0.5) +      # plot bars next to each other and add a thin black line
  scale_fill_manual(values = brewer.pal(9, "Greens")) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6), labels=c("1", "2", "3", "4", "5", "6")) +
  theme_minimal()

# Cluster 1: High Positivity (val) AND low likelihood to be live  <- Chill'd out POP
# Cluster 2: High Positivity (val) AND high likelihood to be live <- Bring the party home
# Cluster 3: Average Song                                         <- Average Radio-tunes
# Cluster 4: Low in Energy AND High in acoustic [plus very quiet] <- Laidback sunday afternoon
# Cluster 5: High in Speech (many words spoken)                   <- Rap-Workouts
# Cluster 6: High Bpm (fast music) AND very low danceability      <- Run Forest,run!

#############################
### Plot Spiderplots
library(fmsb)
minmax <- function(x){(x-min(x))/(max(x)-min(x))}
table[2:10] <- minmax(table[2:10])
data <- rbind(rep(1,10) , rep(0,10) , table)
colnames(data) <- c("Cluster","Bpm", "Energy", "Dance", "dB", "Live", "Positivity", "Duration", "Acoustic", "Speechness")
data[,1] <- c("Sample", "Sample", "Chill'd Out Pop", "Bring the Party Home!", "Average Radio-Hits", "Lazy Sunday", "Rap-Workouts", "Shake it off!") 


# Set graphic colors
colors_border <- "#1DB954"
library(scales)
colors_in <- alpha("#1DB954",0.4)

for (i in 3:8){
  # If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
  radarchart(data[c(1:2,i),2:10], axistype=0, maxmin=T,
             #custom polygon
             pcol=colors_border , pfcol=colors_in , plwd=7 , plty=1,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", cglwd=1, 
             #custom labels
             vlcex=0.9,
             title = data[i,1])
}

###################################################
#### Create playlists tp create them in Spotify!

Playlist_1 <- top10_drop %>%
  filter(Cluster == 1) %>%
  select(title, artist, top.genre, year)

write.csv(Playlist_1, "C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Presentation 1 - Hierarchical Clustering/Playlist_1.csv")

Playlist_2 <- top10_drop %>%
  filter(Cluster == 2) %>%
  select(title, artist, top.genre, year)

write.csv(Playlist_2, "C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Presentation 1 - Hierarchical Clustering/Playlist_2.csv")

Playlist_3 <- top10_drop %>%
  filter(Cluster == 3) %>%
  select(title, artist, top.genre, year)

write.csv(Playlist_3, "C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Presentation 1 - Hierarchical Clustering/Playlist_3.csv")

Playlist_4 <- top10_drop %>%
  filter(Cluster == 4) %>%
  select(title, artist, top.genre, year)

write.csv(Playlist_4, "C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Presentation 1 - Hierarchical Clustering/Playlist_4.csv")

Playlist_5 <- top10_drop %>%
  filter(Cluster == 5) %>%
  select(title, artist, top.genre, year)

write.csv(Playlist_5, "C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Presentation 1 - Hierarchical Clustering/Playlist_5.csv")

Playlist_6 <- top10_drop %>%
  filter(Cluster == 6) %>%
  select(title, artist, top.genre, year)

write.csv(Playlist_6, "C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Presentation 1 - Hierarchical Clustering/Playlist_6.csv")

############################################
#### Appendix (Alternative Clusterizes)
set.seed(1)
# EXCLUDE/INCLUDE GENRE ??? 
sc_top10 <- top10_adj
sc_top10 <- as.data.frame(sc_top10)

# Dissimilarity matrix
d <- dist(sc_top10, method = "euclidean")
set.seed(5)
hc1 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp4 <- cutree(hc1, k = 4)

# Number of members in each cluster
table(sub_grp4)

plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 4, border = 2:5)
fviz_nbclust(sc_top10, FUN = hcut, method = "wss")
fviz_nbclust(sc_top10, FUN = hcut, method = "gap_stat")
?fviz_nbclust

gap_stat <- clusGap(sc_top10, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

fviz_cluster(list(data = sc_top10, cluster = sub_grp4))

table4 <- seg.summ(top10_adj, sub_grp4)

# Reshape the results table from wide to long to have categorical data for plotting purposes
table4.long <- melt(table4,id.vars=c("Group.1"))
# Change column names
colnames(table4.long) <- c("Cluster", "Legend", "Value")

# Plot the results
ggplot(table4.long, aes(x=Cluster, y=Value, fill=Legend)) +                # Clusters on the x-axis, grouped by Question 16
  geom_bar(stat="identity", position="dodge", col="black", size=0.5) +      # plot bars next to each other and add a thin black line
  scale_fill_manual(values = brewer.pal(10, "Greens")) +
  theme_minimal()

# Cluster 5+6 merged in 4
# Cluster 2+3 merged in 2

##################################
##### Plots Spider alternative clusters
library(fmsb)
minmax <- function(x){(x-min(x))/(max(x)-min(x))}
table4[2:10] <- minmax(table4[2:10])
data <- rbind(rep(1,10) , rep(0,10) , table4)
colnames(data) <- c("Cluster","Bpm", "Energy", "Dance", "dB", "Live", "Positivity", "Duration", "Acoustic", "Speechness")
data[,1] <- c("Sample", "Sample", "Cluster 1", "Cluster 2 + 3", "Cluster 4", "Cluster 5 + 6") 

# Set graphic colors
colors_border <- "#1DB954"
library(scales)
colors_in <- alpha("#1DB954",0.4)

for (i in 3:6){
  # If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
  radarchart(data[c(1:2,i),2:10], axistype=0, maxmin=T,
             #custom polygon
             pcol=colors_border , pfcol=colors_in , plwd=7 , plty=1,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", cglwd=1, 
             #custom labels
             vlcex=0.9,
             title = data[i,1])
}

subset <- top10_drop %>%
  group_by(Cluster, year) %>%
  summarize(n = n())

#################################################
### Plot Heatmap Appendix

some_col_func <- function(n) rev(colorspace::sequential_hcl(n, "Greens", l = c(30, 90), power = c(1/5, 1.5)))
library(gplots)
heatmap.2(as.matrix(top10_adj), 
          main = "Dissimilarity Matrix Ward",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend6,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",     
          col = some_col_func
)

heatmap.2(as.matrix(top10_adj), 
          main = "Dissimilarity Matrix Complete",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend2,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",     
          col = some_col_func
)

d2=color_branches(as.dendrogram(hc1),k=6) %>%
  set("labels_cex", 0.001)
plot(d2)
abline(h=20, col="black", lty="dashed")

d2=color_branches(as.dendrogram(hc1),k=4) %>%
  set("labels_cex", 0.001)
plot(d2)
abline(h=23, col="black", lty="dashed")
