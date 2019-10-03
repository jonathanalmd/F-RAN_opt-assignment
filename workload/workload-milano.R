# based on: https://rstudio-pubs-static.s3.amazonaws.com/343521_19d968f5a54141a49f6db3eb7b6ff099.html

###################################################
# Libraries
library(ggplot2)
library(reshape)
library(dplyr)
library(tidyr)
library(kernlab)
###################################################
# Basic functions

#Set dir
set_wdir <- function(){
  library(rstudioapi) 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

# Normalize
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

###################################################



cleanData <- function(data_type, file_name, file_ext){
  # Get data
  cdr_input <- read.csv(paste('data/',data_type,'/',file_name,'.',file_ext,sep=""),sep="\t",header=F)
  head(cdr_input)
  summary(cdr_input)
  
  
  # Rename col names
  colnames(cdr_input) <- c("square_id","time_interval","country_code","sms_in","sms_out","call_in","call_out","internet_traffic")
  
  cdr_input_subset_df <- cdr_input
  head(cdr_input_subset_df)
  
  
  cdr_input_subset_df_cln <- cdr_input_subset_df%>% group_by(country_code) %>%mutate(sms_in = ifelse(is.na(sms_in),as.integer(mean(sms_in, na.rm = TRUE)), sms_in))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(sms_in = ifelse(is.na(sms_in),0, sms_in))
  cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(sms_out = ifelse(is.na(sms_out),as.integer(mean(sms_out, na.rm = TRUE)), sms_out))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(sms_out = ifelse(is.na(sms_out),0, sms_out))
  cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(call_in = ifelse(is.na(call_in),as.integer(mean(call_in, na.rm = TRUE)), call_in))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(call_in = ifelse(is.na(call_in),0, call_in))
  cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(call_out = ifelse(is.na(call_out),as.integer(mean(call_out, na.rm = TRUE)), call_out))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(call_out = ifelse(is.na(call_out),0, call_out))
  
  print(summary(cdr_input_subset_df_cln))
  print(summary(cdr_input_subset_df))
  
  factorColumns <- c("square_id","country_code")
  cdr_input_subset_df_cln[factorColumns] <- lapply(cdr_input_subset_df_cln[factorColumns],as.factor)
  head(cdr_input_subset_df_cln)
  
  val <- cdr_input_subset_df_cln$time_interval/1000
  cdr_input_subset_df_cln$outputTime <- as.POSIXct(val, origin="1970-01-01",tz="UTC")
  print(head(cdr_input_subset_df_cln))
  
  # Transform time
  # derive activity start date and hour from time interval
  cdr_input_subset_df_cln$activity_start_time <- cdr_input_subset_df_cln$outputTime 
  #cdr_input_subset_df$activity_date <- as.Date(as.POSIXct(cdr_input_subset_df$activity_start_time,origin="1970-01-01"))
  cdr_input_subset_df_cln$activity_date <- as.Date(cdr_input_subset_df_cln$activity_start_time)
  cdr_input_subset_df_cln$activity_time <- format(cdr_input_subset_df_cln$activity_start_time,"%H")
  
  # derive total activity from sms in and out, call in and out and internet traffic activity 
  cdr_input_subset_df_cln$total_activity <- rowSums(cdr_input_subset_df_cln[, c(4,5,6,7,8)],na.rm=T)
  
  # filter NAs from internet_traffic column 
  cdr_input_subset_df_cln <- cdr_input_subset_df_cln[!is.na(cdr_input_subset_df_cln$internet_traffic),]
  
  return (cdr_input_subset_df_cln)
}


aggragateTrafficXY <- function(df){
  df <- subset(df,select=c("square_id","internet_traffic"))
  
  df <- aggregate(internet_traffic ~ square_id, df, FUN=sum)
  df$x <- floor(as.numeric(df$square_id)/100)
  df$y <- as.numeric(df$square_id) %% 100
  
  return (df)
}

subMap <- function(df, x_max, x_min, y_max, y_min){
  filteredMap <- filter(df, (x > x_min & y > y_min) & (x < x_max & y < y_max))
  print(filteredMap)
  return (filteredMap)
}

elbowTest <- function(df){
  # Find optimal number of clusters
  # Find the vector of sum of squared error(SSE) for each cluster selection
  wcss <- vector()
  for(i in 1:20){
    wcss[i] = sum(kmeans(df,i)$withinss)
  }
  clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))
  
  # When number of cluster increases then the SSE will be reduced.
  ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white")
  
}


plotHeatMap <- function(df){
  # heatmap
  p <- ggplot(df, aes(x,y, fill=internet_traffic))
  p + geom_tile()
}



# Clustering

applySpectralClustering <- function(df, nclusters){
  specDF <- subset(df,select=c("internet_traffic","x","y"))
  specDF <- as.matrix(specDF)
  specModel <- specc(specDF,  kernel = "rbfdot", kpar = "automatic", centers=nclusters)
  print(summary(specModel))
  
  # Append the identified cluster to the input dataframe
  df$square_id <- as.factor(df$square_id)
  df$cluster <- as.factor(specModel)
  
  # clustered map
  p <- ggplot(df, aes(x,y))
  print(p + geom_point(aes(colour=cluster)))
  
  return (df)
}

applyKmeans <- function(df, nclusters){
  kmeansModel <- kmeans(df,nclusters,nstart=10)
  print(summary(kmeansModel))
  
  # Append the identified cluster to the input dataframe
  df$cluster <- as.factor(kmeansModel$cluster)
  df$square_id <- as.factor(df$square_id)
  
  # clustered map
  p <- ggplot(df, aes(x,y))
  print(p + geom_point(aes(colour=cluster)))
  
  return(df)
}

mergeClusterActivityTime <- function(df_full, df_cluster){
  # add activity time
  subdf <- subset(df_full,select=c("square_id","activity_time","total_activity"))
  subdf <- aggregate(total_activity ~ square_id + activity_time, subdf, FUN=sum)
  
  return (merge(subset(df_cluster, select=c("square_id","cluster")), subdf, by=c("square_id")))
}

normalizeActivity <- function(df){
  df$total_activity <- normalize(df$total_activity)
  return (df)
}

barplotActivityCluster <- function(df, nclusters){
  for(i in 1:nclusters){
    activityCluster <- df %>% filter(cluster == i)
    ggplot(aggregate(total_activity ~ activity_time,activityCluster ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(activityCluster))) + 
      geom_bar(stat="identity")
  }
}


boxplotActivityCluster <- function(df, nclusters, facet = TRUE){
  for(i in 1:nclusters){
    activityCluster <- df %>% filter(cluster == i)
    ggplot(data=activityCluster, aes(x=activity_time, y=total_activity)) +
      geom_boxplot()
  }
  
  if(facet){
    # Boxplot + Clusters normalized with facet
    ggplot(data=df, aes(x=activity_time, y=total_activity)) +
      geom_boxplot() +
      facet_grid(rows = vars(cluster))
  }
}



main <- function(){
  # Set directory
  set_wdir()
  data_type = "weekday"
  file_name = "sms-call-internet-mi-2013-11-11"
  file_ext = "txt"
  df_full <- cleanData(data_type, file_name, file_ext)
  
  #df_internet_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time","total_activity"))
  df_internet_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
  
  # aggregate squares and insert xy columns
  df_internet_ag_sum <- aggragateTrafficXY(df_internet_full)
  
  # elbow test
  elbowTest(df_internet_ag_sum)
  
  # clustering
  full_map <- TRUE
  nclusters <- 4
  if(full_map){
    # plot heat map
    plotHeatMap(df_internet_ag_sum)
    
    df_internet_ag_sum_clustered <- applyKmeans(df_internet_ag_sum, nclusters)
  }else{
    x_max <- 76
    x_min <- 33
    y_max <- 75
    y_min <- 38
    df_internet_ag_sum_milano <- subMap(df_internet_ag_sum, x_max, x_min, y_max, y_min)
    
    # plot heat map
    plotHeatMap(df_internet_ag_sum_milano)
    set.seed(4242) 
    #df_internet_ag_sum_clustered_milano <- applySpectralClustering(df_internet_ag_sum_milano, nclusters)
    df_internet_ag_sum_clustered_milano_kmeans <- applyKmeans(df_internet_ag_sum_milano, nclusters)
  }
  
}










# total area - 3 clusters




subdf <- subset(cdr_input_subset_df_cln,select=c("square_id","total_activity"))

df_milano <- aggregate(total_activity ~ square_id, subdf, FUN=sum)
df_milano$x <- floor(as.numeric(df_milano$square_id)/100)
df_milano$y <- as.numeric(df_milano$square_id) %% 100
# df_milano <- filter(df_milano, (x > 45 & y > 37) & (x < 73 & y < 64))
# df_milano <- filter(df_milano, (x > 32 & y > 37) & (x < 75 & y < 74))
#df_milano <- filter(df_milano, (x > 33 & y > 38) & (x < 76 & y < 75))
#df_milano <- filter(df_milano, (x > 33 & y > 44) & (x < 76 & y < 81))



# find number of clusters
# subset the required columns
cdrActivityDF <- df_milano


# find the vector of sum of squared error(SSE) for each cluster selection
wcss <- vector()
for(i in 1:20) wcss[i] = sum(kmeans(cdrActivityDF,i)$withinss)
clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))

# plot the result and selects the optimal cluster number by looking at the graph. When number of cluster increases then the SSE will be reduced.
ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white")+theme_bw()



# applying k-means on acitivity hours and total_activity - using clusters
cdrClusterModel <- kmeans(cdrActivityDF,3,nstart=10)
#cdrClusterModel <- kmeans(cdrActivityDF,10,nstart=10)

print(summary(cdrClusterModel))

# Append the identified cluster to the input dataframe
cdrActivityDF$cluster <- as.factor(cdrClusterModel$cluster)
cdrActivityDF$square_id <- as.factor(cdrActivityDF$square_id)



# heatmap
p <- ggplot(cdrActivityDF, aes(x,y, fill=total_activity))
p + geom_tile()

# clustered map
p <- ggplot(cdrActivityDF, aes(x,y))
p + geom_point(aes(colour=cluster))





subdf <- subset(cdr_input_subset_df_cln,select=c("square_id","activity_time","total_activity"))
subdf <- aggregate(total_activity ~ square_id + activity_time, subdf, FUN=sum)
cdrActivityDF_time <-  merge(subset(cdrActivityDF, select=c("square_id","cluster")), subdf, by=c("square_id"))



#square1DF = cdrActivityDF %>% filter(x == 44 & y == 44)
#totalSquare1ActivityPlot = ggplot(data=square1DF, aes(x=activity_time, y=total_activity)) + geom_bar(stat="identity")+theme_bw()
#print(totalSquare1ActivityPlot)


cdrActivityDFC1 = cdrActivityDF_time %>% filter(cluster == 1)
cdrActivityDFC2 = cdrActivityDF_time %>% filter(cluster == 2)
cdrActivityDFC3 = cdrActivityDF_time %>% filter(cluster == 3)


totalActivityPlot <- ggplot(aggregate(total_activity ~ activity_time,cdrActivityDFC1 ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(cdrActivityDFC1))) + geom_bar(stat="identity")+theme_bw()
print(totalActivityPlot)

totalActivityPlot <- ggplot(aggregate(total_activity ~ activity_time,cdrActivityDFC2 ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(cdrActivityDFC2))) + geom_bar(stat="identity")+theme_bw()
print(totalActivityPlot)

totalActivityPlot <- ggplot(aggregate(total_activity ~ activity_time,cdrActivityDFC3 ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(cdrActivityDFC3))) + geom_bar(stat="identity")+theme_bw()
print(totalActivityPlot)















# milano area - 4 clusters




subdf <- subset(cdr_input_subset_df_cln,select=c("square_id","total_activity"))

df_milano <- aggregate(total_activity ~ square_id, subdf, FUN=sum)
df_milano$x <- floor(as.numeric(df_milano$square_id)/100)
df_milano$y <- as.numeric(df_milano$square_id) %% 100
# df_milano <- filter(df_milano, (x > 45 & y > 37) & (x < 73 & y < 64))
# df_milano <- filter(df_milano, (x > 32 & y > 37) & (x < 75 & y < 74))
df_milano <- filter(df_milano, (x > 33 & y > 38) & (x < 76 & y < 75))
#df_milano <- filter(df_milano, (x > 33 & y > 44) & (x < 76 & y < 81))



# find number of clusters
# subset the required columns
cdrActivityDF <- df_milano


# find the vector of sum of squared error(SSE) for each cluster selection
wcss <- vector()
for(i in 1:20) wcss[i] = sum(kmeans(cdrActivityDF,i)$withinss)
clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))

# plot the result and selects the optimal cluster number by looking at the graph. When number of cluster increases then the SSE will be reduced.
ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white")+theme_bw()



# applying k-means on acitivity hours and total_activity - using clusters
#  K-means, as a data-clustering algorithm, is ideal for discovering globular clusters 
# where all members of each cluster are in close proximity to each other (in the Euclidean sense)
cdrClusterModel <- kmeans(cdrActivityDF,4,nstart=10)
specDF <- subset(cdrActivityDF,select=c("total_activity","x","y"))
specDF <- as.matrix(specDF)
cdrClusterModel <- specc(specDF,  kernel = "rbfdot", kpar = "automatic", centers=4)
#cdrClusterModel <- kmeans(cdrActivityDF,10,nstart=10)

print(summary(cdrClusterModel))

# Append the identified cluster to the input dataframe
# cdrActivityDF$cluster <- as.factor(cdrClusterModel$cluster)
cdrActivityDF$square_id <- as.factor(cdrActivityDF$square_id)
cdrActivityDF$cluster <- as.factor(cdrClusterModel)




p <- ggplot(cdrActivityDF, aes(x,y))
p + geom_point(aes(colour=cluster),size = 5)

p <- ggplot(cdrActivityDF, aes(x,y, fill=total_activity))
p + geom_tile()





subdf <- subset(cdr_input_subset_df_cln,select=c("square_id","activity_time","total_activity"))
subdf <- aggregate(total_activity ~ square_id + activity_time, subdf, FUN=sum)
cdrActivityDF_time <-  merge(subset(cdrActivityDF, select=c("square_id","cluster")), subdf, by=c("square_id"))


# Normalize
plot_normalized = TRUE
norm_cdrActivityDF_time <- cdrActivityDF_time
norm_cdrActivityDF_time$total_activity <- normalize(norm_cdrActivityDF_time$total_activity)
#scaled_cdrActivityDF_time$total_activity <- scale(cdrActivityDF_time$total_activity)
#colMeans(scaled_cdrActivityDF_time$total_activity)
#apply(scaled_cdrActivityDF_time$total_activity$total_activity, 2, sd)



#square1DF = cdrActivityDF %>% filter(x == 44 & y == 44)
#totalSquare1ActivityPlot = ggplot(data=square1DF, aes(x=activity_time, y=total_activity)) + geom_bar(stat="identity")+theme_bw()
#print(totalSquare1ActivityPlot)



cdrActivityDFC1 = cdrActivityDF_time %>% filter(cluster == 1)
cdrActivityDFC2 = cdrActivityDF_time %>% filter(cluster == 2)
cdrActivityDFC3 = cdrActivityDF_time %>% filter(cluster == 3)
cdrActivityDFC4 = cdrActivityDF_time %>% filter(cluster == 4)


if(plot_normalized){
  cdrActivityDF_time <- norm_cdrActivityDF_time  
  
  totalActivityPlot <- ggplot(data=cdrActivityDFC1, aes(x=activity_time, y=total_activity)) +
    geom_boxplot()
  print(totalActivityPlot)
  
  
  totalActivityPlot <- ggplot(data=cdrActivityDFC2, aes(x=activity_time, y=total_activity)) +
    geom_boxplot()
  print(totalActivityPlot)
  
  
  totalActivityPlot <- ggplot(data=cdrActivityDFC3, aes(x=activity_time, y=total_activity)) +
    geom_boxplot()
  print(totalActivityPlot)
  
  
  totalActivityPlot <- ggplot(data=cdrActivityDFC4, aes(x=activity_time, y=total_activity)) +
    geom_boxplot()
  print(totalActivityPlot)
  
  # Boxplot 
  # ggplot(data=cdrActivityDF_time, aes(x=activity_time, y=total_activity)) +
  #   geom_boxplot(aes(fill=cluster))
  
  
  # Boxplot + Clusters normalized with facet
  ggplot(data=norm_cdrActivityDF_time, aes(x=activity_time, y=total_activity)) +
    geom_boxplot() +
    facet_grid(rows = vars(cluster))
  
} else{
  totalActivityPlot <- ggplot(aggregate(total_activity ~ activity_time,cdrActivityDFC1 ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(cdrActivityDFC1))) + 
    geom_bar(stat="identity")
  print(totalActivityPlot)
  
  totalActivityPlot <- ggplot(aggregate(total_activity ~ activity_time,cdrActivityDFC2 ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(cdrActivityDFC2))) +
    geom_bar(stat="identity")
  print(totalActivityPlot)
  
  totalActivityPlot <- ggplot(aggregate(total_activity ~ activity_time,cdrActivityDFC3 ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(cdrActivityDFC3))) +
    geom_bar(stat="identity")
  print(totalActivityPlot)
  
  totalActivityPlot <- ggplot(aggregate(total_activity ~ activity_time,cdrActivityDFC4 ,FUN=sum), aes(x=activity_time, y=total_activity/nrow(cdrActivityDFC4))) +
    geom_bar(stat="identity")
  print(totalActivityPlot)
}

