# based on: https://rstudio-pubs-static.s3.amazonaws.com/343521_19d968f5a54141a49f6db3eb7b6ff099.html

###################################################
# Libraries
library(ggplot2)
library(reshape)
library(dplyr)
library(plyr)
library(tidyr)
library(kernlab)
library(ggsci)

set.seed(73) 
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



cleanData <- function(cdr_input){
  # Get data
  # cdr_input <- read.csv(input_dir,sep="\t",header=F)
  # head(cdr_input)
  # summary(cdr_input)
  # 
  # 
  # # Rename col names
  colnames(cdr_input) <- c("square_id","time_interval","country_code","sms_in","sms_out","call_in","call_out","internet_traffic")
  
  cdr_input_subset_df <- cdr_input
  #head(cdr_input_subset_df)
  
  # filter NAs from internet_traffic column 
  cdr_input_subset_df <- cdr_input_subset_df[!is.na(cdr_input_subset_df$internet_traffic),]
  
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
  
  return (cdr_input_subset_df_cln)
}


aggragateTrafficXY <- function(df){
  df <- subset(df,select=c("square_id","internet_traffic","weekday"))
  
  df <- aggregate(internet_traffic ~ square_id + weekday, df, FUN=sum)
  df$square_id <- as.numeric(df$square_id) - 1
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
  print(p + geom_point(aes(colour=cluster), size=3))
  
  return (df)
}

applyKmeans <- function(df, nclusters, psize = 1){
  kmeansModel <- kmeans(df,nclusters,nstart=10)
  print(summary(kmeansModel))
  
  # Append the identified cluster to the input dataframe
  df$cluster <- as.factor(kmeansModel$cluster)
  df$square_id <- as.factor(df$square_id)
  
  # clustered map
  p <- ggplot(df, aes(x,y))
  print(p + geom_point(aes(colour=cluster), size=psize) + scale_color_npg())#scale_color_manual(values = c("#FC4E07", "#E7B800",  "#00AFBB", "#4E84C4", "#52854C")))# + scale_color_brewer(palette="Set2"))
  
  return(df)
}

mergeClusterActivityTime <- function(df_full, df_cluster,day_type=FALSE){
  # add activity time
  if(day_type){
    subdf <- subset(df_full,select=c("square_id","activity_time","internet_traffic","weekday"))
    subdf <- aggregate(internet_traffic ~ square_id + activity_time, subdf, FUN=sum)
    
    return (merge(subset(df_cluster, select=c("square_id","cluster","weekday")), subdf, by=c("square_id")))
  }else{
    subdf <- subset(df_full,select=c("square_id","activity_time","internet_traffic"))
    subdf <- aggregate(internet_traffic ~ square_id + activity_time, subdf, FUN=sum)
    
    return (merge(subset(df_cluster, select=c("square_id","cluster")), subdf, by=c("square_id")))
  }
}

normalizeActivity <- function(df){
  df$total_activity <- normalize(df$total_activity)
  return (df)
}

barplotActivityCluster <- function(df, nclusters, divide = TRUE){
  if(divide){
    for(i in 1:nclusters){
      activityCluster <- df %>% filter(cluster == i)
      print(ggplot(aggregate(internet_traffic ~ activity_time,activityCluster ,FUN=sum), aes(x=activity_time, y=internet_traffic/nrow(activityCluster))) + 
              geom_bar(stat="identity") +
              xlab("Hour of day") + ylab("Internet traffic"))
    }
  }else{
    for(i in 1:nclusters){
      activityCluster <- df %>% filter(cluster == i)
      print(ggplot(aggregate(internet_traffic ~ activity_time,activityCluster ,FUN=sum), aes(x=activity_time, y=internet_traffic)) + 
              geom_bar(stat="identity") + 
              xlab("Hour of day") + ylab("Internet traffic"))
    }
  }
  print(ggplot(data=df, aes(x=activity_time, y=internet_traffic)) +
          geom_bar(stat="identity") +
          xlab("Hour of day") + ylab("Internet traffic") +
          facet_grid(rows = vars(cluster)))
  
}


boxplotActivityCluster <- function(df, nclusters, facet = TRUE){
  for(i in 1:nclusters){
    activityCluster <- filter(df, cluster == i)
    print(ggplot(data=activityCluster, aes(x=activity_time, y=internet_traffic)) +
            geom_boxplot() +
            xlab("Hour of day") + ylab("Internet traffic"))
  }
  
  if(facet){
    # Boxplot + Clusters normalized with facet
    print(ggplot(data=df, aes(x=activity_time, y=internet_traffic)) +
            geom_boxplot() +
            xlab("Hour of day") + ylab("Internet traffic") +
            facet_grid(rows = vars(cluster)))
  }
  
}

getMeanPerTime <- function(df, nclusters,weekday=FALSE){
  if(weekday){
    return (aggregate(internet_traffic ~ activity_time + cluster + weekday,df ,FUN=mean))  
  }else{
    return (aggregate(internet_traffic ~ activity_time + cluster,df ,FUN=sum))
  }
  
  #return (aggregate(internet_traffic ~ activity_time + cluster,df ,FUN=mean))
  #return (aggregate(internet_traffic ~ activity_time + cluster, df_internet_full_clustered_norm , function(x) c(mean = mean(x), sd = sd(x))))
}


getSdPerTime <- function(df, nclusters){
  if(nclusters > 1){
    return (aggregate(internet_traffic ~ activity_time + cluster, df_internet_full_clustered_norm , function(x) sd(x)))
  }else{
    return (aggregate(internet_traffic ~ activity_time, df , function(x) sd(x)))
  }
}

initialCleaning <- function(){
  
  weekday_files <- list.files("data/weekday")
  weekend_files <- list.files("data/weekend")
  
  if(clean){
    file_type = "weekday"
    df_weekday_full_raw <- read.csv(paste('data/',file_type,'/',weekday_files[1],sep=""),sep="\t",header=F)
    for(file in weekday_files[-1]){
      print(file)
      
      full_dir <- paste('data/',file_type,'/',file,sep="")
      
      cdr_input <- read.csv(full_dir,sep="\t",header=F)
      
      df_weekday_full_raw <- rbind(df_weekday_full_raw, cdr_input)
      
    }
    
    # Clean data
    df_full <- cleanData(df_weekday_full_raw)
    write.csv(df_full, file = "weekday_cln.csv")
    
    
    file_type = "weekend"
    df_weekend_full_raw <- read.csv(paste('data/',file_type,'/',weekend_files[1],sep=""),sep="\t",header=F)
    for(file in weekend_files[-1]){
      print(file)
      
      full_dir <- paste('data/',file_type,'/',file,sep="")
      
      cdr_input <- read.csv(full_dir,sep="\t",header=F)
      
      df_weekend_full_raw <- rbind(df_weekend_full_raw, cdr_input)
      
    }
    
    
    # Clean data
    df_full <- cleanData(df_weekend_full_raw)
    write.csv(df_full, file = "weekend_cln.csv")
    
  }
}

runAnalysis <- function(df_full){
  # df_full<-df_weekday_full
  # df_full<-df_weekend_full
  
  
  #df_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time","total_activity"))
  # df_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
  df_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time","weekday"))
  df_full$activity_time <- as.factor(df_full$activity_time)
  
  #df_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
  #df_full <- as.factor(df_full$activity_time)
  #rm(df_full)
  # aggregate squares and insert xy columns
  df_internet_ag_sum <- aggragateTrafficXY(df_full)
  
  # df_internet_ag_sum <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-fullmap/df_internet_ag_sum.csv",sep=",",header=T)
  # colnames(df_internet_ag_sum) <- c("square_id", "internet_traffic", "activity_date","activity_time")
  
  pdf("weekday-week-fullmap-plots.pdf")
  # elbow test
  elbowTest(df_internet_ag_sum)
  
  # clustering
  nclusters <- 5
  
  # Full Map
  # plot heat map
  plotHeatMap(df_internet_ag_sum)
  #plotHeatMap(filter(df_internet_ag_sum, weekday==0))
  #plotHeatMap(filter(df_internet_ag_sum, weekday==1))
  write.csv(df_internet_ag_sum, file = "df_internet_ag_sum.csv")
  # Clustering
  df_internet_ag_sum_clustered <- applyKmeans(df_internet_ag_sum, nclusters)
  write.csv(df_internet_ag_sum_clustered, file = "df_internet_ag_sum_clustered.csv")
  
  for(i in 0:1){
    # Barplot
    df_internet_full_clustered_day <- mergeClusterActivityTime(filter(df_full,weekday==i), filter(df_internet_ag_sum_clustered,weekday==i))
    barplotActivityCluster(df_internet_full_clustered_day, nclusters)
    # if(i==0){
    #   write.csv(filter(df_internet_full_clustered_day,weekday==i), file = "weekend-df_internet_full_clustered.csv")
    # }else{
    #   write.csv(filter(df_internet_full_clustered_day,weekday==i), file = "weekday-df_internet_full_clustered.csv")
    # }
  }
  rm(df_internet_full_clustered_day)
  # subdf <- subset(filter(df_internet_full_clustered_norm, cluster == 1), select=c("activity_time","internet_traffic"))
  # write.csv(subdf, file = "subdf.csv")
  
  
  # Boxplot (normalized)
  df_internet_full_clustered <- mergeClusterActivityTime(df_full, df_internet_ag_sum_clustered, TRUE)
  df_internet_full_clustered_norm <- df_internet_full_clustered
  df_internet_full_clustered_norm$internet_traffic <- normalize(df_internet_full_clustered$internet_traffic)
  for(i in 0:1){
    boxplotActivityCluster(filter(df_internet_full_clustered_norm, weekday==i), nclusters)
    if(i==0){
      write.csv(filter(df_internet_full_clustered_norm, weekday==i), file = "weekend-df_internet_full_clustered_norm.csv")
    }else{
      write.csv(filter(df_internet_full_clustered_norm, weekday==i), file = "weekday-f_internet_full_clustered_norm.csv")
    }
  }
  
  # Barplot (normalized)
  df_internet_full_sum_clustered <- getMeanPerTime(df_internet_full_clustered, nclusters, weekday=TRUE)
  df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
  for(i in 0:1){
    #df_internet_full_sum_clustered <- getMeanPerTime(filter(df_internet_full_clustered,weekday==i), nclusters)
    #df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
    #df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
    
    
    barplotActivityCluster(filter(df_internet_full_sum_clustered, weekday==i), nclusters, divide=FALSE)
    if(i == 0){
      write.csv(filter(df_internet_full_sum_clustered, weekday==i), file = "weekend-df_internet_full_sum_clustered.csv")
    }else{
      write.csv(filter(df_internet_full_sum_clustered, weekday==i), file = "weekday-df_internet_full_sum_clustered.csv")
    }
    
  }
  
  
  
  #write.csv(getSdPerTime(df_internet_full_clustered,nclusters), file = "fullmap_all-clusters-sd.csv")
  
  for(i in 1:nclusters){
    for (j in 0:1){
      df_act <- subset(filter(filter(df_internet_full_sum_clustered, weekday==j), cluster == i), select=c("activity_time","internet_traffic"))
      df_act <- merge(getSdPerTime(filter(df_internet_full_clustered_norm, cluster == i), 1), df_act, by=c("activity_time"))
      df_act <- df_act[order(df_act$activity_time),]
      df_act <- df_act[,c(1,3,2)]
      colnames(df_act) <- c("activity_time","internet_traffic","internet_traffic_sd")
      if(j == 0){
        write.csv(df_act, file = paste("weekend-fullmap_cluster",i,".csv", sep=""))
      }else{
        write.csv(df_act, file = paste("weekday-fullmap_cluster",i,".csv", sep=""))
      }
      #write.csv(mean_sd <- aggregate(internet_traffic ~ activity_time, subset(filter(df_internet_full_clustered_norm, cluster == i), select=c("activity_time","internet_traffic")),  function(x) c(mean = mean(x), sd = sd(x))), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
      #write.csv(getSdPerTime(filter(df_internet_full_clustered_norm, cluster == i)), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
      
    }
    
  }
  
  dev.off()
  
  
  
  
  
  
  
  
  # Milano Map
  nclusters <- 5
  x_max <- 75
  x_min <- 35
  y_max <- 80
  y_min <- 40
  df_internet_ag_sum <- subMap(df_internet_ag_sum, x_max, x_min, y_max, y_min)
  
  pdf("weekday-1week-milano-plots.pdf")
  # elbow test
  elbowTest(df_internet_ag_sum)
  
  # clustering
  nclusters <- 5
  
  # Full Map
  # plot heat map
  plotHeatMap(df_internet_ag_sum)
  #plotHeatMap(filter(df_internet_ag_sum, weekday==0))
  #plotHeatMap(filter(df_internet_ag_sum, weekday==1))
  write.csv(df_internet_ag_sum, file = "df_internet_ag_sum.csv")
  # Clustering
  df_internet_ag_sum_clustered <- applyKmeans(df_internet_ag_sum, nclusters, psize=4.5)
  write.csv(df_internet_ag_sum_clustered, file = "df_internet_ag_sum_clustered.csv")
  
  for(i in 0:1){
    # Barplot
    df_internet_full_clustered_day <- mergeClusterActivityTime(filter(df_full,weekday==i), filter(df_internet_ag_sum_clustered,weekday==i))
    barplotActivityCluster(df_internet_full_clustered_day, nclusters)
    # if(i==0){
    #   write.csv(filter(df_internet_full_clustered_day,weekday==i), file = "weekend-df_internet_full_clustered.csv")
    # }else{
    #   write.csv(filter(df_internet_full_clustered_day,weekday==i), file = "weekday-df_internet_full_clustered.csv")
    # }
  }
  rm(df_internet_full_clustered_day)
  # subdf <- subset(filter(df_internet_full_clustered_norm, cluster == 1), select=c("activity_time","internet_traffic"))
  # write.csv(subdf, file = "subdf.csv")
  
  
  # Boxplot (normalized)
  df_internet_full_clustered <- mergeClusterActivityTime(df_full, df_internet_ag_sum_clustered, TRUE)
  df_internet_full_clustered_norm <- df_internet_full_clustered
  df_internet_full_clustered_norm$internet_traffic <- normalize(df_internet_full_clustered$internet_traffic)
  for(i in 0:1){
    boxplotActivityCluster(filter(df_internet_full_clustered_norm, weekday==i), nclusters)
    if(i==0){
      write.csv(filter(df_internet_full_clustered_norm, weekday==i), file = "weekend-df_internet_milano_clustered_norm.csv")
    }else{
      write.csv(filter(df_internet_full_clustered_norm, weekday==i), file = "weekday-df_internet_milano_clustered_norm.csv")
    }
  }
  
  # Barplot (normalized)
  df_internet_full_sum_clustered <- getMeanPerTime(df_internet_full_clustered, nclusters, weekday=TRUE)
  df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
  for(i in 0:1){
    #df_internet_full_sum_clustered <- getMeanPerTime(filter(df_internet_full_clustered,weekday==i), nclusters)
    #df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
    #df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
    
    
    barplotActivityCluster(filter(df_internet_full_sum_clustered, weekday==i), nclusters, divide=FALSE)
    if(i == 0){
      write.csv(filter(df_internet_full_sum_clustered, weekday==i), file = "weekend-df_internet_milano_sum_clustered.csv")
    }else{
      write.csv(filter(df_internet_full_sum_clustered, weekday==i), file = "weekday-df_internet_milano_sum_clustered.csv")
    }
    
  }
  
  
  
  #write.csv(getSdPerTime(df_internet_full_clustered,nclusters), file = "fullmap_all-clusters-sd.csv")
  
  for(i in 1:nclusters){
    for (j in 0:1){
      df_act <- subset(filter(filter(df_internet_full_sum_clustered, weekday==j), cluster == i), select=c("activity_time","internet_traffic"))
      df_act <- merge(getSdPerTime(filter(df_internet_full_clustered_norm, cluster == i), 1), df_act, by=c("activity_time"))
      df_act <- df_act[order(df_act$activity_time),]
      df_act <- df_act[,c(1,3,2)]
      colnames(df_act) <- c("activity_time","internet_traffic","internet_traffic_sd")
      if(j == 0){
        write.csv(df_act, file = paste("weekend-milano_cluster",i,".csv", sep=""))
      }else{
        write.csv(df_act, file = paste("weekday-milano_cluster",i,".csv", sep=""))
      }
      #write.csv(mean_sd <- aggregate(internet_traffic ~ activity_time, subset(filter(df_internet_full_clustered_norm, cluster == i), select=c("activity_time","internet_traffic")),  function(x) c(mean = mean(x), sd = sd(x))), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
      #write.csv(getSdPerTime(filter(df_internet_full_clustered_norm, cluster == i)), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
      
    }
    
  }
  
  dev.off()
  
}





# Set directory
set_wdir()
clean <- FALSE

if(clean){
  initialCleaning()
}

#df_weekday_full <- read.csv("data/cleaned/week/weekday_cln_week.csv",sep=",",header=T)
#runAnalysis(df_weekday_full)
df_weekday <- read.csv("data/cleaned/week/weekday_cln_week.csv",sep=",",header=T)
df_weekday$weekday <- 1

df_weekend <- read.csv("data/cleaned/week/weekend_cln_2week.csv",sep=",",header=T)
df_weekend$weekday <- 0

df_full <- rbind(df_weekday, df_weekend)
rm(df_weekday,df_weekend)

# Full - 10gb
#df_weekday_full <- read.csv("data/cleaned/full/weekday_cln.csv",sep=",",header=T)
#runAnalysis(df_weekday_full)







































































































weekday_df_internet_full_clustered <- read.csv("results/v6/weekday-df_internet_full_clustered.csv",sep=",",header=T)
weekend_df_internet_full_clustered <- read.csv("results/v6/weekend-df_internet_full_clustered.csv",sep=",",header=T)

weekend_df_internet_full_clustered$cluster[weekend_df_internet_full_clustered$cluster==1]  <- "c4"
weekend_df_internet_full_clustered$cluster[weekend_df_internet_full_clustered$cluster==2]  <- "c5"
weekend_df_internet_full_clustered$cluster[weekend_df_internet_full_clustered$cluster==3]  <- "c1"
weekend_df_internet_full_clustered$cluster[weekend_df_internet_full_clustered$cluster==4]  <- "c2"
weekend_df_internet_full_clustered$cluster[weekend_df_internet_full_clustered$cluster==5]  <- "c3"

weekend_df_internet_full_clustered$weekday <- 0

weekday_df_internet_full_clustered$cluster[weekday_df_internet_full_clustered$cluster==1]  <- "c1"
weekday_df_internet_full_clustered$cluster[weekday_df_internet_full_clustered$cluster==2]  <- "c2"
weekday_df_internet_full_clustered$cluster[weekday_df_internet_full_clustered$cluster==3]  <- "c3"
weekday_df_internet_full_clustered$cluster[weekday_df_internet_full_clustered$cluster==4]  <- "c4"
weekday_df_internet_full_clustered$cluster[weekday_df_internet_full_clustered$cluster==5]  <- "c5"

weekday_df_internet_full_clustered$weekday <- 1


all_df_internet_full_clustered <- rbind(weekday_df_internet_full_clustered, weekend_df_internet_full_clustered)



df_internet_full_clustered_norm <- all_df_internet_full_clustered
df_internet_full_clustered_norm$internet_traffic <- normalize(df_internet_full_clustered_norm$internet_traffic)
df_internet_full_clustered_norm$internet_traffic <- as.numeric(df_internet_full_clustered_norm$internet_traffic)
df_internet_full_clustered_norm$activity_time <- as.factor(df_internet_full_clustered_norm$activity_time)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c1"] <- as.numeric(1)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c2"] <- as.numeric(2)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c3"] <- as.numeric(3)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c4"] <- as.numeric(4)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c5"] <- as.numeric(5)


weekday_df_internet_full_clustered_norm <- filter(df_internet_full_clustered_norm, weekday==1)
weekend_df_internet_full_clustered_norm <- filter(df_internet_full_clustered_norm, weekday==0)







alldaysnorm(weekday_df_internet_full_clustered_norm,"weekday","fullmap")
alldaysnorm(weekend_df_internet_full_clustered_norm,"weekend","fullmap")

alldaysnorm <- function(df, day_type, map_type){
  nclusters = 5
  pdf(paste("totalnorm",day_type,"-",map_type,"-plots.pdf",sep=""))

  # Barplot (normalized)
  barplotActivityCluster(df, nclusters, TRUE)

    # Boxplot (normalized)
  boxplotActivityCluster(df, nclusters)
  
  
  # subdf <- subset(filter(df, cluster == 1), select=c("activity_time","internet_traffic"))
  # write.csv(subdf, file = "subdf.csv")
  
  
  
  #write.csv(getSdPerTime(df_internet_full_clustered,nclusters), file = "fullmap_all-clusters-sd.csv")
  
  for(i in 1:nclusters){
    df_act <- subset(filter(df, cluster == i), select=c("activity_time","internet_traffic"))
    df_act <- merge(getSdPerTime(filter(df, cluster == i), 1), df_act, by=c("activity_time"))
    df_act <- df_act[order(df_act$activity_time),]
    df_act <- df_act[,c(1,4,3,2)]
    colnames(df_act) <- c("id","activity_time","internet_traffic","internet_traffic_sd")
    
    write.csv(df_act, file = paste(map_type,"-cluster",i,".csv", sep=""))
    #write.csv(mean_sd <- aggregate(internet_traffic ~ activity_time, subset(filter(df, cluster == i), select=c("activity_time","internet_traffic")),  function(x) c(mean = mean(x), sd = sd(x))), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
    #write.csv(getSdPerTime(filter(df, cluster == i)), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
    
  }
  
  dev.off()
  
}






df_internet_full_clustered_norm <- all_df_internet_full_clustered
df_internet_full_clustered_norm$internet_traffic <- normalize(all_df_internet_full_clustered$internet_traffic)
df_internet_full_clustered_norm$internet_traffic <- as.numeric(df_internet_full_clustered_norm$internet_traffic)
df_internet_full_clustered_norm$activity_time <- as.factor(df_internet_full_clustered_norm$activity_time)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c1"] <- as.numeric(1)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c2"] <- as.numeric(2)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c3"] <- as.numeric(3)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c4"] <- as.numeric(4)
df_internet_full_clustered_norm$cluster[df_internet_full_clustered_norm$cluster=="c5"] <- as.numeric(5)


weekday_df_internet_full_clustered_norm <- filter(df_internet_full_clustered_norm, weekday==1)
weekend_df_internet_full_clustered_norm <- filter(df_internet_full_clustered_norm, weekday==0)




pdf("totalnorm-weekday-2week-milano-plots.pdf")
# Boxplot (normalized)
boxplotActivityCluster(weekday_df_internet_full_clustered_norm, nclusters)
write.csv(weekday_df_internet_full_clustered_norm, file = "weekday_df_internet_full_clustered_norm.csv")

# Barplot (normalized)
df_internet_full_sum_clustered <- getMeanPerTime(df_internet_full_clustered, nclusters)
df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
barplotActivityCluster(df_internet_full_sum_clustered, nclusters, divide=FALSE)
write.csv(df_internet_full_sum_clustered, file = "df_internet_full_sum_clustered.csv")


# subdf <- subset(filter(weekday_df_internet_full_clustered_norm, cluster == 1), select=c("activity_time","internet_traffic"))
# write.csv(subdf, file = "subdf.csv")



#write.csv(getSdPerTime(df_internet_full_clustered,nclusters), file = "fullmap_all-clusters-sd.csv")

for(i in 1:nclusters){
  df_act <- subset(filter(df_internet_full_sum_clustered, cluster == i), select=c("activity_time","internet_traffic"))
  df_act <- merge(getSdPerTime(filter(weekday_df_internet_full_clustered_norm, cluster == i), 1), df_act, by=c("activity_time"))
  df_act <- df_act[order(df_act$activity_time),]
  df_act <- df_act[,c(1,3,2)]
  colnames(df_act) <- c("activity_time","internet_traffic","internet_traffic_sd")
  
  write.csv(df_act, file = paste("fullmap_cluster",i,".csv", sep=""))
  #write.csv(mean_sd <- aggregate(internet_traffic ~ activity_time, subset(filter(weekday_df_internet_full_clustered_norm, cluster == i), select=c("activity_time","internet_traffic")),  function(x) c(mean = mean(x), sd = sd(x))), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
  #write.csv(getSdPerTime(filter(weekday_df_internet_full_clustered_norm, cluster == i)), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
  
}

dev.off()


##############



#######

































############################################################
# analysis pos data


# df_full<-df_weekday_full

df_internet_ag_sum <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-fullmap/df_internet_ag_sum.csv",sep=",",header=T)

pdf("weekday-2week-fullmap-plots.pdf")
# elbow test
elbowTest(df_internet_ag_sum)

# clustering
nclusters <- 5

# Full Map
# plot heat map
plotHeatMap(df_internet_ag_sum)
# Clustering
df_internet_ag_sum_clustered <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-fullmap/df_internet_ag_sum_clustered.csv",sep=",",header=T)
# Barplot
df_internet_full_clustered <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-fullmap/df_internet_full_clustered.csv",sep=",",header=T)
barplotActivityCluster(df_internet_full_clustered, nclusters)
# Boxplot (normalized)
df_internet_full_clustered_norm <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-fullmap/df_internet_full_clustered_norm.csv",sep=",",header=T)
boxplotActivityCluster(df_internet_full_clustered_norm, nclusters)

# Barplot (normalized)
df_internet_full_sum_clustered <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-fullmap/df_internet_ag_sum_clustered.csv",sep=",",header=T)
barplotActivityCluster(df_internet_full_sum_clustered, nclusters, divide=FALSE)


subdf <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-fullmap/subdf.csv",sep=",",header=T)


for(i in 1:nclusters){
  write.csv(subset(filter(df_internet_full_sum_clustered, cluster == i), select=c("activity_time","internet_traffic")), file = paste("fullmap_cluster",i,".csv", sep=""))
  write.csv(mean_sd <- aggregate(internet_traffic ~ activity_time, subset(filter(df_internet_full_clustered_norm, cluster == i), select=c("activity_time","internet_traffic")),  function(x) c(mean = mean(x), sd = sd(x))), file = paste("fullmap_cluster",i,"-summary.csv", sep=""))
}

dev.off()





pdf("weekday-2week-milano-plots.pdf")

# Milano Map
nclusters <- 5
x_max <- 75
x_min <- 35
y_max <- 80
y_min <- 40
df_internet_ag_sum_milano <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-milano/df_internet_ag_sum_milano.csv",sep=",",header=T)
# plot heat map
plotHeatMap(df_internet_ag_sum_milano)
#df_internet_ag_sum_clustered_milano <- applySpectralClustering(df_internet_ag_sum_milano, nclusters)
df_internet_ag_sum_milano_clustered <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-milano/df_internet_ag_sum_milano_clustered.csv",sep=",",header=T)

# Barplot (not normalized)
df_internet_milano_clustered <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-milano/df_internet_milano_clustered.csv",sep=",",header=T)
barplotActivityCluster(df_internet_milano_clustered, nclusters)

# Boxplot (normalized)
df_internet_milano_clustered_norm <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-milano/df_internet_milano_clustered_norm.csv",sep=",",header=T)
boxplotActivityCluster(df_internet_milano_clustered_norm, nclusters)

# Barplot (normalized)
df_internet_milano_sum_clustered <- read.csv("results/v5/weekday/dataframes-weekday-2weeks-milano/df_internet_milano_sum_clustered.csv",sep=",",header=T)
barplotActivityCluster(df_internet_milano_sum_clustered, nclusters, divide=FALSE)

dev.off()

for(i in 1:nclusters){
  write.csv(subset(filter(df_internet_milano_sum_clustered, cluster == i), select=c("activity_time","internet_traffic")), file = paste("milano_cluster",i,".csv", sep=""))
  write.csv(aggregate(internet_traffic ~ activity_time, subset(filter(df_internet_full_clustered_norm, cluster == i), select=c("activity_time","internet_traffic")),  function(x) c(mean = mean(x), sd = sd(x))), file = paste("milano_cluster",i,"-summary.csv", sep=""))
}









# df_weekend_milano_aux <- df_weekend_milano
# 
# df_weekend_milano_aux %>% 
#   group_by(square_id, cluster, activity_time) %>% 
#   summarise_all(funs(trimws(paste(., collapse = ''))))
# 
# dd <- df_weekend_milano_aux
# dd[order(dd$square_id, dd$activity_time, decreasing = FALSE),]  
# 
# 
# df_weekend_milano_aux <- aggregate(internet_traffic ~ square_id + activity_time + cluster, df_weekend_milano_aux, FUN=sum)
# 
# df_weekend_milano_aux$internet_traffic <- normalize(df_weekend_milano_aux$internet_traffic)







# df_weekend_full <- data.frame(square_id=as.factor(factor()),
#                          cluster=as.factor(factor()), 
#                          activity_time=integer(), 
#                          internet_traffic=numeric(),
#                          stringsAsFactors=FALSE) 
# df_weekend_milano <- data.frame(square_id=as.factor(factor()),
#                          cluster=as.factor(factor()), 
#                          activity_time=integer(), 
#                          internet_traffic=numeric(),
#                          stringsAsFactors=FALSE) 
# 
# 
# 
# df_weekday_full <- data.frame(square_id=as.factor(factor()),
#                               cluster=as.factor(factor()), 
#                               activity_time=integer(), 
#                               internet_traffic=numeric(),
#                               stringsAsFactors=FALSE) 
# df_weekday_milano <- data.frame(square_id=as.factor(factor()),
#                                 cluster=as.factor(factor()), 
#                                 activity_time=integer(), 
#                                 internet_traffic=numeric(),
#                                 stringsAsFactors=FALSE) 


# file_type = "weekday"
# for(file in weekday){
#   print(file)
#   
#   full_dir <- paste('data/',file_type,'/',file,sep="")
#   df_full <- cleanData(full_dir)
#   
#   rbind(df_weekend_full, df_internet_full_clustered)
#   
# }