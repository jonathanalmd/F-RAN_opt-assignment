# based on: https://rstudio-pubs-static.s3.amazonaws.com/343521_19d968f5a54141a49f6db3eb7b6ff099.html

###################################################
# Libraries
library(tidyverse)
library(ggplot2)

#Set dir
set_wdir <- function(){
  library(rstudioapi) 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_wdir()


# Functions

# Normalize
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}


get_df <- function(m_type, d_type, reg){
  # m_type = "milano"
  # d_type = "weekday"
  # reg = "downtown"
  files <- list.files(paste("data/",m_type,"/",d_type,"/",reg, sep=""))
  df <- data.frame(prop=double(),
                   cell_type=factor(),
                   day_type=factor(),
                   map_type=factor(),
                   region=factor(),
                   day_part=factor(),
                   stringsAsFactors=FALSE)
  colnames(df) <- c("prop","cell_type","day_type","map_type","region","day_part")
  
  for (file in files){
    df_unit <- read.csv(paste("data/",m_type,"/",d_type,"/",reg,"/",file,sep=""),header=F)
    if (d_type == "weekend"){
      day_type = 0
    }else{
      day_type = 1
    }
    row <- data.frame(df_unit[1,1], "mc", day_type, m_type, reg, "Night")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[2,1], "mc", day_type, m_type, reg, "Morning")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[3,1], "mc", day_type, m_type, reg, "Afternoon")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[4,1], "mc", day_type, m_type, reg, "Evening")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    
    row <- data.frame(df_unit[1,2], "sc", day_type, m_type, reg, "Night")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[2,2], "sc", day_type, m_type, reg, "Morning")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[3,2], "sc", day_type, m_type, reg, "Afternoon")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[4,2], "sc", day_type, m_type, reg, "Evening")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
  }
  return (df)
}




# Main
total_df <- data.frame(prop=double(),
                       cell_type=factor(),
                       day_type=factor(),
                       map_type=factor(),
                       region=factor(),
                       day_part=factor(),
                       stringsAsFactors=FALSE)
maps <- c("milano","fullmap")
days <- c("weekday","weekend")
milano_regs <- c("downtown","urban","semiurban")
fullmap_regs <- c("urban","semiurban","semirural")

# milano_df <- get_df("milano","weekday","downtown")

for (map in maps){
  if (map == "milano"){
    for (day in days){
      for (reg in milano_regs){
        df <- get_df(map, day, reg)  
        total_df <- rbind(total_df, df)
      }
    }    
  }else{ # fullmap
    for (day in days){
      for (reg in fullmap_regs){
        df <- get_df(map, day, reg)  
        total_df <- rbind(total_df, df)
      }
    }
  }
}

milano_df <- filter(total_df, map_type == "milano")
fullmap_df <- filter(total_df, map_type == "fullmap")

pdf("week-milano-plots.pdf")


# total_df$day_part[total_df$day_part=="night"] <- "Night"
# total_df$day_part[total_df$day_part=="morning"] <- "Morning"
# total_df$day_part[total_df$day_part=="afternoon"] <- "Afternoon"
# total_df$day_part[total_df$day_part=="evening"] <- "Evening"
# 
# total_df$day_type[total_df$day_type=="Weekend"] <- "Weekend"
# total_df$day_type[total_df$day_type=="Weekday"] <- "Weekday"
# 
# total_df$region[total_df$region=="downtown"] <- "Downtown"
# total_df$region[total_df$region=="urban"] <- "Urban"
# total_df$region[total_df$region=="semiurban"] <- "Suburban"
# 
# total_df$cell_type[total_df$cell_type=="mc"] <- "Macrocell"
# total_df$cell_type[total_df$cell_type=="sc"] <- "Smallcell"

colnames(total_df) <- c("prop","MDC","day_type","map_type","region","day_part")

day_type.labs <- c("Weekend","Weekday")
names(day_type.labs) <- c(0,1)
total_df$day_type <- factor(total_df$day_type, levels=c(1,0))

region.labs <- c("Downtown", "Urban", "Suburban")
names(region.labs) <- c("downtown", "urban", "semiurban")
total_df$region <- factor(total_df$region, levels = c("downtown", "urban", "semiurban"))




ggplot(data=filter(total_df, map_type == "milano"), aes(x=day_part, y=prop, fill=RRH)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type, labeller = labeller(day_type = day_type.labs, region = region.labs)) +
  theme_bw()


ggplot(data=filter(total_df, map_type == "milano"), aes(x=day_part, y=prop, fill=RRH)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type, labeller = labeller(day_type = day_type.labs, region = region.labs)) +
  theme_bw()

ggplot(data=filter(total_df, map_type == "milano" & day_part != "Night"), aes(x=day_part, y=prop, fill=Antenna)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type, labeller = labeller(day_type = day_type.labs, region = region.labs)) +
  theme_bw()



# horizontal

ggplot(data=filter(total_df, map_type == "milano" & prop < 10), aes(x=day_part, y=prop, fill=MDC)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") +
  facet_grid(day_type~region, labeller = labeller(day_type = day_type.labs, region = region.labs)) +
  theme_bw() #+   theme(legend.position="bottom", legend.direction="horizontal", 
                      #legend.title = element_blank())

# normalized
# df_norm_weekday <- filter(total_df, map_type == "milano", day_type == 1)
# df_norm_weekday$prop <- normalize(df_norm_weekday$prop)
# df_norm_weekend <- filter(total_df, map_type == "milano", day_type == 0)
# df_norm_weekend$prop <- normalize(df_norm_weekend$prop)
# df_norm <- rbind(df_norm_weekday, df_norm_weekend)
# df_norm <- total_df
# df_norm$prop <- normalize(filter(df_norm, prop <= 12)$prop)
# ggplot(data=filter(df_norm, map_type == "milano"), aes(x=day_part, y=prop, fill=RRH)) +
#   geom_boxplot() +
#   xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
#   facet_grid(day_type~region, labeller = labeller(day_type = day_type.labs, region = region.labs)) +
#   theme_bw() #+   theme(legend.position="bottom", legend.direction="horizontal", 
#legend.title = element_blank())







  ggplot(data=filter(total_df, map_type == "milano" & day_part != "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type) + 
  theme_bw()
    

ggplot(data=filter(total_df, map_type == "milano" & day_part == "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type) + 
  theme_bw()

# without semiurban
# with night
# weekday vs weekend and downtown vs urban
ggplot(data=filter(total_df, map_type == "milano" & (region == "downtown" | region == "urban")), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type) + 
  theme_bw()


ggplot(data=filter(total_df, map_type == "milano" & (region == "downtown")), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(day_type)) + 
  theme_bw()



# without night
ggplot(data=filter(total_df, map_type == "milano" & (region == "downtown" | region == "urban") & day_part != "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type) + 
  theme_bw()

ggplot(data=filter(total_df, map_type == "milano" & (region == "downtown" | region == "urban") & day_part != "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(region)) + 
  theme_bw()


ggplot(data=filter(total_df, map_type == "milano" & (region == "downtown") & day_part != "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(day_type)) + 
  theme_bw()



dev.off()




# without semiurban
# with night
# weekday vs weekend and downtown vs urban
ggplot(data=filter(total_df, map_type == "fullmap" & (region == "urban" | region == "semiurban")), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type) + 
  theme_bw()

ggplot(data=filter(total_df, map_type == "fullmap" & (region == "urban" | region == "semiurban")), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(region)) + 
  theme_bw()


ggplot(data=filter(total_df, map_type == "fullmap" & (region == "urban")), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(day_type)) + 
  theme_bw()



# without night
ggplot(data=filter(total_df, map_type == "fullmap" & (region == "urban" | region == "semiurban") & day_part != "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type) + 
  theme_bw()

ggplot(data=filter(total_df, map_type == "fullmap" & (region == "urban" | region == "semiurban") & day_part != "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(region)) + 
  theme_bw()


ggplot(data=filter(total_df, map_type == "fullmap" & (region == "urban") & day_part != "night"), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(day_type)) + 
  theme_bw()









# t.test FULLMAP
# fullmap mc weekday urban 
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "Night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "Morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "Afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "Evening")$prop)$"conf.int"
# fullmap mc weekend urban 
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "Night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "Morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "Afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "Evening")$prop)$"conf.int"


# fullmap mc weekday semiurban
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"
# fullmap mc weekend semiurban
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"


# fullmap mc weekday semirural -> CONSTANT
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semirural" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semirural" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semirural" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "semirural" & day_part == "evening")$prop)$"conf.int"
# fullmap mc weekend semirural
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semirural" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semirural" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semirural" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "semirural" & day_part == "evening")$prop)$"conf.int"



# fullmap sc weekday urban 
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "evening")$prop)$"conf.int"
# fullmap sc weekend urban 
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "evening")$prop)$"conf.int"


# fullmap sc weekday semiurban
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"
# fullmap sc weekend semiurban
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"


# fullmap sc weekday semirural 
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semirural" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semirural" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semirural" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekday" & region == "semirural" & day_part == "evening")$prop)$"conf.int"
# fullmap sc weekend semirural
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semirural" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semirural" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semirural" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "sc" & day_type == "weekend" & region == "semirural" & day_part == "evening")$prop)$"conf.int"












# t.test milano
# MC
# milano mc weekday urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "Downtown" & day_part == "Night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "Morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "Afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "Evening")$prop)$"conf.int"
# milano mc weekend urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "Night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "Morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "Afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "Evening")$prop)$"conf.int"


# milano mc weekday semiurban
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "evening")$prop)$"conf.int"
# milano mc weekend semiurban
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "evening")$prop)$"conf.int"


# milano mc weekday semirural 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"
# milano mc weekend semirural
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"






# SC
# milano sc weekday urban 
t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "downtown" & day_part == "Night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "downtown" & day_part == "Morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "downtown" & day_part == "Afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "downtown" & day_part == "Evening")$prop)$"conf.int"
# milano sc weekend urban 
t.test(filter(milano_df, cell_type == "sc" & day_type == 0 & region == "downtown" & day_part == "Night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == 0 & region == "downtown" & day_part == "Morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == 0 & region == "downtown" & day_part == "Afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == 0 & region == "downtown" & day_part == "Evening")$prop)$"conf.int"


# milano sc weekday semiurban
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban" & day_part == "evening")$prop)$"conf.int"
# milano sc weekend semiurban
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban" & day_part == "evening")$prop)$"conf.int"


# milano sc weekday semirural 
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"
# milano sc weekend semirural
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban" & day_part == "evening")$prop)$"conf.int"











# t.test milano
# MC
# milano mc weekday urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "downtown")$prop)
# milano mc weekend urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == 0 & region == "downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == 0 & region == "downtown")$prop)

# milano mc weekday semiurban
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban")$prop)
# milano mc weekend semiurban
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban")$prop)

# milano mc weekday semirural 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban")$prop)
# milano mc weekend semirural
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban")$prop)





# SC
# milano sc weekday urban 
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "downtown")$prop)
# milano sc weekend urban 
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "downtown")$prop)

# milano sc weekday semiurban
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban")$prop)
# milano sc weekend semiurban
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban")$prop)

# milano sc weekday semirural 
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban")$prop)
# milano sc weekend semirural
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban")$prop)










# t.test milano
# MC
# milano mc weekday urban 
t.test(filter(milano_df, cell_type == "mc" & region == "downtown")$prop)
# milano mc weekday semiurban
t.test(filter(milano_df, cell_type == "mc" & region == "urban")$prop)
# milano mc weekday semirural 
t.test(filter(milano_df, cell_type == "mc" & region == "semiurban")$prop)

# SC
# milano mc weekday urban 
t.test(filter(milano_df, cell_type == "sc" & region == "downtown")$prop)
# milano mc weekday semiurban
t.test(filter(milano_df, cell_type == "sc" & region == "urban")$prop)
# milano mc weekday semirural 
t.test(filter(milano_df, cell_type == "sc" & region == "semiurban")$prop)














# t.test milano
# MC
milano_df <- filter(milano_df, day_part != "night")

# milano mc weekday urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "Downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "Downtown")$prop)
# milano mc weekend urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown")$prop)

# milano mc weekday semiurban
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "urban")$prop)
# milano mc weekend semiurban
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "urban")$prop)

# milano mc weekday semirural 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "semiurban")$prop)
# milano mc weekend semirural
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "semiurban")$prop)





# SC
# milano sc weekday urban 
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "downtown")$prop)
# milano sc weekend urban 
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "downtown")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "downtown")$prop)

# milano sc weekday semiurban
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "urban")$prop)
# milano sc weekend semiurban
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "urban")$prop)

# milano sc weekday semirural 
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekday" & region == "semiurban")$prop)
# milano sc weekend semirural
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "sc" & day_type == "weekend" & region == "semiurban")$prop)











t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "night")$prop)



t.test(filter(total_df, cell_type == "mc")$prop)$"conf.int"
# [104.2587 , 144.8466] 95%

# One Sample t-test
# 
# data:  filter(total_df, cell_type == "mc")$prop
# t = 12.049, df = 719, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   104.2587 144.8466
# sample estimates:
#   mean of x 
# 124.5527 
# p value = 2.2 * 10^-16 << 0.05


t.test(filter(total_df, cell_type == "sc")$prop)$"conf.int"
# [27.42 , 29.64104] 95%
# One Sample t-test
# 
# data:  filter(total_df, cell_type == "sc")$prop
# t = 50.453, df = 719, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   27.42061 29.64104
# sample estimates:
#   mean of x 
# 28.53083 




# without night
t.test(filter(total_df, cell_type == "mc" & day_part != "night")$prop)$"conf.int"
# [58.7940 , 71.96055] 95%
#
# One Sample t-test
# 
# data:  filter(total_df, cell_type == "mc" & day_part != "night")$prop
# t = 19.51, df = 539, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   58.79540 71.96055
# sample estimates:
#   mean of x 
# 65.37798 



t.test(filter(total_df, cell_type == "sc" & day_part != "night")$prop)$"conf.int"
# [23.68825 , 25.40421] 95%
# One Sample t-test
# 
# data:  filter(total_df, cell_type == "sc")$prop
# t = 50.453, df = 719, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   27.42061 29.64104
# sample estimates:
#   mean of x 
# 28.53083 




# 17


t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "downtown" & day_part == "Night")$prop)
t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "downtown" & day_part == "Morning")$prop)
t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "downtown" & day_part == "Afternoon")$prop)
t.test(filter(milano_df, cell_type == "mc" & day_type == 1 & region == "downtown" & day_part == "Evening")$prop)

t.test(filter(filter(milano_df, prop < 15), cell_type == "mc" & day_type == 1 & region == "urban" & day_part == "Morning")$prop)
t.test(filter(filter(milano_df, prop < 15), cell_type == "mc" & day_type == 1 & region == "urban" & day_part == "Afternoon")$prop)


t.test(filter(filter(milano_df, prop < 15), cell_type == "mc" & day_type == 0 & region == "downtown" & day_part == "Afternoon")$prop)




t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "downtown" & day_part == "Morning")$prop)
t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "downtown" & day_part == "Afternoon")$prop)

t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "urban" & day_part == "Morning")$prop)
t.test(filter(milano_df, cell_type == "sc" & day_type == 1 & region == "urban" & day_part == "Afternoon")$prop)


t.test(filter(milano_df, cell_type == "sc" & day_type == 0 & region == "downtown" & day_part == "Afternoon")$prop)


