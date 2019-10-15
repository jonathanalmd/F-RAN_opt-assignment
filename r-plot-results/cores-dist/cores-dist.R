# based on: https://rstudio-pubs-static.s3.amazonaws.com/343521_19d968f5a54141a49f6db3eb7b6ff099.html

###################################################
# Libraries
library(tidyverse)
library(ggplot2)
library(ggsci)

#Set dir
set_wdir <- function(){
  library(rstudioapi) 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_wdir()

df <- read.csv("cores-dist-red.csv",sep=";",header=F)
df_p <- read.csv("cost-dist.csv",sep=";",header=F)
colnames(df) <- c("Instances","Class","Distance")
colnames(df_p) <- c("Distance","Cost")

cores1 <- 6
cores2 <- 8
cores3 <- 2
cores4 <- 4

dfc1 <- filter(df, Class == 1)
dfc2 <- filter(df, Class == 2)
dfc3 <- filter(df, Class == 3)
dfc4 <- filter(df, Class == 4)

dfc1$Instances <- ceiling(dfc1$Instances / cores1)
dfc2$Instances <- ceiling(dfc2$Instances / cores2)
dfc3$Instances <- ceiling(dfc3$Instances / cores3)
dfc4$Instances <- ceiling(dfc4$Instances / cores4)

df_cln <- rbind(dfc1,dfc2)
df_cln <- rbind(df_cln, dfc3)
df_cln <- rbind(df_cln, dfc4)

df_cln$Class <- as.factor(df_cln$Class)

ggplot(df_cln, aes(x=Distance, y=Instances, group=Class)) +
  geom_line(aes(linetype=Class, colour = Class))+
  geom_point(aes(shape=Class, colour = Class)) +
  geom_line(data=df_p, aes(x=Distance, y=Cost)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.0425))+
  scale_color_npg() + theme_bw() + 
  xlab("Distance (m)") + ylab("Number of allocated processors")


ggplot(df_cln, aes(x=Distance, y=Instances, group=Class)) +
  geom_line(aes(linetype=Class, colour = Class))+
  geom_point(aes(shape=Class, colour = Class)) +
  geom_line(aes(y=Instances*0.0425*8)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.0425))+
  scale_color_npg() + theme_bw() + 
  xlab("Distance (m)") + ylab("Number of allocated processors")


