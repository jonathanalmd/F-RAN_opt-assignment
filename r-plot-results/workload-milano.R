# based on: https://rstudio-pubs-static.s3.amazonaws.com/343521_19d968f5a54141a49f6db3eb7b6ff099.html

###################################################
# Libraries
library(tidyverse)
library(ggplot2)

# Functions
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
    
    row <- data.frame(df_unit[1,1], "mc", d_type, m_type, reg, "night")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[2,1], "mc", d_type, m_type, reg, "morning")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[3,1], "mc", d_type, m_type, reg, "afternoon")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[4,1], "mc", d_type, m_type, reg, "evening")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    
    row <- data.frame(df_unit[1,2], "sc", d_type, m_type, reg, "night")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[2,2], "sc", d_type, m_type, reg, "morning")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[3,2], "sc", d_type, m_type, reg, "afternoon")
    colnames(row) <- c("prop","cell_type","day_type","map_type","region","day_part")
    df <- rbind(df, row)
    row <- data.frame(df_unit[4,2], "sc", d_type, m_type, reg, "evening")
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



# without semiurban
# with night
# weekday vs weekend and downtown vs urban
ggplot(data=filter(total_df, map_type == "milano" & (region == "downtown" | region == "urban")), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(region~day_type) + 
  theme_bw()

ggplot(data=filter(total_df, map_type == "milano" & (region == "downtown" | region == "urban")), aes(x=day_part, y=prop, fill=cell_type)) +
  geom_boxplot() +
  xlab("Part of day") + ylab("Maximum income / Cost of allocation") + 
  facet_grid(rows = vars(region)) + 
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
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekday" & region == "urban" & day_part == "evening")$prop)$"conf.int"
# fullmap mc weekend urban 
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "night")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "morning")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(fullmap_df, cell_type == "mc" & day_type == "weekend" & region == "urban" & day_part == "evening")$prop)$"conf.int"


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
# milano mc weekday urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekday" & region == "downtown" & day_part == "evening")$prop)$"conf.int"
# milano mc weekend urban 
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "night")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "morning")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "afternoon")$prop)$"conf.int"
t.test(filter(milano_df, cell_type == "mc" & day_type == "weekend" & region == "downtown" & day_part == "evening")$prop)$"conf.int"


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
