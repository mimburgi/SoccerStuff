##packages####
library(tidyverse)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)
## preproc ####
raw<-read.csv("PL-Whoscored.csv")
raw<-raw[,2:ncol(raw)]
library(dplyr)
teams<-raw$Team
raw<-select(raw, -(contains("Rating")))
raw<-select(raw, -(contains("R.")))
raw<-select(raw, -(contains("Team.")))
colnames(raw)<-c("Team", "TotalTackles", "DribbledPast", "TotalAttemptedTackles", "R", "Interceptions", "Fouled", "Fouls", 
                 "Yellow", "Red",  "CaughtOffside", "Clearances", "ShotsBlocked", "CrossesBlocked", "PassesBlocked", 
                 "TotalSaves",  "SixYardBoxSaves",   "PenaltyAreaSaves",  "OutOfBoxSaves",  "TotalShots",  "OutOfBoxShots",   
                 "SixYardBoxShots", "PenaltyAreaShots", "TotalGoals", "SixYardBoxGoals", "PenaltyAreaGoals", "OutOfBoxGoals",   
                 "Unsuccessful", "Successful", "TotalDribbles", "UnsuccessfulTouches", "Dispossessed", "TotalAerial",
                 "AerialWon",   "AerialLost", "TotalPasses","AccLB", "InAccLB",   "AccSP", "InAccSP","TotalKeyPasses","LongKeyPasses",  "ShortKeyPasses", 
                 "CrossAss", "CornerAss","ThroughballAss",  "FreekickAss","ThrowinAss","OtherAss", "TotalAss")
raw<-select(raw, -"R")
raw<-select(raw, -"ThrowinAss")

rownames(raw)<-raw$Team
raw<-select(raw, -"Team")
scaled<-scale(raw)
offense<-raw[,c("OutOfBoxShots" ,        "SixYardBoxShots"  ,     "PenaltyAreaShots" ,    
                "TotalGoals"     ,       "SixYardBoxGoals"   ,    "PenaltyAreaGoals"  ,   
                "OutOfBoxGoals"   ,      "Unsuccessful"       ,   "Successful"         ,  
                "TotalDribbles"    ,     "UnsuccessfulTouches" ,  "Dispossessed"        , 
                "TotalAerial"       ,    "AerialWon"          ,   "AerialLost"           ,
                "TotalPasses"        ,   "AccLB"          ,       "InAccLB"              ,
                "AccSP"               ,  "InAccSP"         ,      "TotalKeyPasses"       ,
                "LongKeyPasses"     ,    "ShortKeyPasses"   ,     "CrossAss"             ,
                "CornerAss"          ,   "ThroughballAss"    ,    "FreekickAss"          ,
                 "TotalAss")]

## pca ####
res.pca <- PCA(scale(offense),  graph = FALSE)
# Visualize eigenvalues/variances
library(factoextra)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
# Extract the results for variables
var <- get_pca_var(res.pca)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")

## kmeans ####
kmean_calc <- function(df, ...){
  kmeans(df, scale(offense) = ..., nstart = 30)
}
km2 <- kmean_calc(scale(offense), 2)
km3 <- kmean_calc(scale(offense), 3)
km4 <- kmeans(scale(offense), 4)
km5 <- kmeans(scale(offense), 5)
km6 <- kmeans(scale(offense), 6)
km7 <- kmeans(scale(offense), 7)
km8 <- kmeans(scale(offense), 8)
km9 <- kmeans(scale(offense), 9)
km10 <- kmeans(scale(offense), 10)
km11 <- kmeans(scale(offense), 11)
p1 <- fviz_cluster(km2, data = scale(offense), frame.type = "convex") + theme_minimal() + ggtitle("k = 2") 
p2 <- fviz_cluster(km3, data = scale(offense), frame.type = "convex") + theme_minimal() + ggtitle("k = 3")
p3 <- fviz_cluster(km4, data = scale(offense), frame.type = "convex") + theme_minimal() + ggtitle("k = 4")
p4 <- fviz_cluster(km5, data = scale(offense), frame.type = "convex") + theme_minimal() + ggtitle("k = 5")
p5 <- fviz_cluster(km6, data = scale(offense), frame.type = "convex") + theme_minimal() + ggtitle("k = 6")
p6 <- fviz_cluster(km7, data = scale(offense), frame.type = "convex") + theme_minimal() + ggtitle("k = 7")
plot_grid(p1, p2, p3, p4, p5, p6, labels = c("k2", "k3", "k4", "k5", "k6", "k7"))

# dendrogam ###
# Compute dissimilarity matrix with euclidean distances
d <- dist(scale(offense), method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Cut tree into 5 groups
grp <- cutree(res.hc, k = 5)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 5, border = 2:5) # add rectangle

#dendrogram features ####
final <- kmeans(scale(offense), 5, nstart = 30)
fviz_cluster(final, data = scale(offense)) + theme_minimal() + ggtitle("k = 5")
as.data.frame(scale(offense)) %>% mutate(Cluster = final$cluster) %>% group_by(Cluster) %>% summarise_all("mean")
