rsd=function(vector){
  m=mean(vector)
  sd=sd(vector)
  return((sd/m)*100)
} #relative SD


secondmax=function(vector){
  vector<-as.numeric(vector)
  maxval<-max(vector)
  tmp<-vector[vector != maxval]
  return(max(tmp))
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


nearestcenter=function(clusternum, kminput, res.km, labels){
  tmp<-kminput
  numVars<-length(kminput)
  centers<-res.km$centers
  #add centers
  for (var in (1:numVars)){
    tmp[[paste0('center', as.character(var))]]<-centers[clusternum,var]
  }
  #calculate distances
  for(var in 1:numVars){
    tmp[[paste0('distance', as.character(var))]]<-(tmp[,var] - tmp[,(var + numVars)])^2
  }
  #sum distances
  tmp[['totaldistance']]<-rowSums(tmp[,(1+(2*numVars)):(3*numVars)])
  closest<-which(tmp$totaldistance==min(tmp$totaldistance))
  labels[closest]
  
} #find player closest to cluster center


plotclusters=function(dataframe, clusters, clustervar){
  library(reshape2)
  library(ggplot2)
  if (missing(clusters)){
    if (is.numeric(dataframe[[clustervar]])){
      dataframe[[clustervar]]<-as.factor(dataframe[[clustervar]])
    }
    clusters<-dataframe[[clustervar]]
  }
  tmp<-select_if(dataframe, is.numeric)
  tmp$cluster<-as.factor(clusters)
  tmp<-melt(tmp, id.vars="cluster")
  plot(ggplot(data = tmp, aes(x=variable, y=value)) + geom_boxplot(aes(fill=cluster)))
}