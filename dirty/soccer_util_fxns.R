rsd=function(vector){
  m=mean(vector)
  sd=sd(vector)
  return((sd*100)/m)
}


plot_cluster=function(data, var_cluster, title="")
{
  library(RColorBrewer)
  palette<- colorRampPalette(brewer.pal(8, "Set2"))(length(levels(var_cluster)))
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=2) +
    scale_color_manual(values = palette) +
    xlab("") + ylab("") +
    ggtitle(title) +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") 
}

topProbs<-function(team, knnfit=fit, topnum=3){
  teamindex<-which(mlscomp$Team==team)
  allprobs<-fit$prob[teamindex,,drop=F] %>% as.data.frame %>% gather()
  topprobs<-allprobs[order(allprobs$value, decreasing = T),]%>% {(.)[1:topnum,]}
  return(topprobs)
}


compareTeams<-function(team1, team2, knndat=knnset){
  require(fmsb)
  require(dplyr)
  compmetrics<-colnames(select_if(knndat, is.numeric))
  scaledraw<-select_if(raw_extra, is.numeric) %>% scale()
  team1df<-scaledraw[raw_extra$Team==team1, compmetrics] 
  team2df<-scaledraw[raw_extra$Team==team2, compmetrics]
  gginput<-rbind(rep(3, length(compmetrics)),
                 rep(-3, length(compmetrics)),
                 team1df, team2df)
  rownames(gginput)<-c("max", "min", team1, team2)
  gginput<-as.data.frame(gginput)
  return(radarchart(gginput))
}

compareMLSTeam<-function(mlsdata, mlsteam, otherdata, otherteam,
                         maxnum=3, minnum=-3, segments=4,
                         linealpha=.9, fillalpha=.2,
                         color1="tomato", color2="skyblue"){
  require(fmsb)
  require(dplyr)
  df1<- mlsdata[rownames(mlsdata)==mlsteam,c(1:(ncol(mlsdata)-1))]
  df2<- otherdata[rownames(otherdata)==otherteam,]
  gginput<-rbind(rep(maxnum, ncol(df1)),
                 rep(minnum, ncol(df1)),
                 df1, df2)
  rownames(gginput)<-c("max", "min", mlsteam, otherteam)
  colors_fill <- c(scales::alpha(color1, fillalpha),
                   scales::alpha(color2, fillalpha))
  
  # Define line colors
  colors_line <- c(scales::alpha(color1, linealpha),
                   scales::alpha(color2, linealpha))
  radarchart(gginput, seg=segments, pcol=colors_line, pfcol=colors_fill, plwd=2)
  legend(x=0.6, 
         y=1.35, 
         legend = rownames(gginput[-c(1,2),]), 
         bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
}


kmeans2radar<-function(rawdata, kmeans){
  rawdata_df <- as.data.frame(rawdata) %>% rownames_to_column()
  cluster_pos <- as.data.frame(kmeans$cluster) %>% rownames_to_column()
  colnames(cluster_pos) <- c("rowname", "cluster")
  output <- inner_join(cluster_pos, rawdata_df)
  output$cluster<-as.factor(output$cluster)
  ggRadar(output[-1], aes(group = cluster), rescale = TRUE, legend.position = "none", size = 1, interactive = FALSE, use.label = TRUE) + facet_wrap(~cluster) + scale_y_discrete(breaks = NULL) + # don't show ticks
    theme(axis.text.x = element_text(size = 10)) + scale_fill_manual(values = rep("#1c6193", nrow(output))) +
    scale_color_manual(values = rep("#1c6193", nrow(output)))
}

predict.kmeans <- function(object, newdata, method = c("centers", "classes")) {
  method <- match.arg(method)
  
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }
}

raw2radardata<-function(rawdata){
  tmpdata<-rawdata
  rmcols <- rev(seq(1,ncol(tmpdata))[!as.logical(sapply(tmpdata, is.numeric))])
  for (i in rmcols) tmpdata[[i]] <- NULL
  tmpdata[nrow(tmpdata)+1,]<-colMeans(tmpdata)
  return(tmpdata[nrow(tmpdata),])
}


