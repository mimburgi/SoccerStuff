plot_radars=function(clusters, df){
  require(fmsb)
  require(scales)
  require(ggplot2)
  
  clusts<-as.factor(clusters)
  
  #makegroups
  for (i in levels(clusts)){
    assign(paste0('group', i), df[clusts==i,])
  }
  
  #makeaggs
  for (i in levels(clusts)){
    assign(paste0('groupaggs', i),
           get(paste0('group', i)) %>% summarise_all(median))
  }
  
  
  #graph parameters
  means<-rep(0, length(df))
  names(means)<-colnames(df)
  col <- c('#3f2199')
  colors_border <- c('black', col)
  colors_in <- c(scales::alpha('white', 0), scales::alpha(col,0.3))
  
  #make dfs for radars
  for (i in levels(clusts)){
    assign(paste0('radardf', i),
           rbind(rep(3, length(df)),
                 rep(-3, length(df)),
                 means,
                 get(paste0('groupaggs',i))))
  }
  
  for (i in levels(clusts)){
    radarchart(get(paste0('radardf', i)),
               pty=32,
               pcol=colors_border, pfcol=colors_in,plwd=c(2,4),plty=1,
               cglcol='grey', cglty=1, axislabcol = 'black', cglwd=1.5,
               centerzero = T,
               title=paste('Group', i)
    )
  }
}

plot_clust_tendency=function(clustrange=2:3, df, title=""){
  require(ggplot2)
  require(factoextra)
  hopkinsdf<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
  for (i in clustrange){
    hopstat<-get_clust_tendency(df, i, graph = F)$hopkins_stat
    hopkinsdf[nrow(hopkinsdf) + 1,]<-c(i, hopstat)
  }
  hopkinsdf<-hopkinsdf[-1,]
  
  ggplot(hopkinsdf, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
    ggtitle(title)
}

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

plot_cluster=function(data, var_cluster, title=""){
  library(RColorBrewer)
  library(ggplot2)
  
  var_cluster<-as.factor(var_cluster)
  
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
  }else{
    clusters<-as.factor(clusters)
  }
  tmp<-select_if(dataframe, is.numeric)
  tmp$cluster<-as.factor(clusters)
  tmp<-melt(tmp, id.vars="cluster")
  plot(ggplot(data = tmp, aes(x=variable, y=value)) + geom_boxplot(aes(fill=cluster)))
}

basic_clusts=function(dataframe, centers){
  require(cluster)
  require(factoextra)
  assign(paste0("km", as.character(centers)),
         kmeans(dataframe, centers, nstart = 10),
         envir = .GlobalEnv)
  assign(paste0("clar", as.character(centers)),
         clara(dataframe, centers),
         envir = .GlobalEnv)
  assign(paste0("pam", as.character(centers)),
         pam(dataframe, centers),
         envir = .GlobalEnv)
  assign(paste0("hkward", as.character(centers)),
         hkmeans(dataframe, centers, hc.method='ward.D2'),
         envir = .GlobalEnv)
  assign(paste0("hkcomplete", as.character(centers)),
         hkmeans(dataframe, centers, hc.method='complete'),
         envir = .GlobalEnv)
  assign(paste0("hkavg", as.character(centers)),
         hkmeans(dataframe, centers, hc.method='average'),
         envir = .GlobalEnv)
}