library(maptools)
library(rgeos)
library(gpclib)
library("RColorBrewer")
guat <- readShapePoly("GTM_adm/GTM_adm1.shp")
plot(guat)

names(guat)
guat$NAME_1

zika <- read.csv("./data_country/Guatemala/SEMEPI/data/
                 SEMEPI_24_2016-06-28.csv")
data <- list.files(path = "data_country/Guatemala/SEMEPI/data", full.names = T) 

data <- do.call(rbind,lapply(data,read.csv, na.strings=c("","NA")))
head(data)

data <- subset(data, location_type=="municipality")

data$location <- substring(data$location,11)

data <- subset(data, data_field_code=="GT0001")
data <- subset(data, report_date == "2016-06-28")

library(ggplot2)
guat_dist <- fortify(guat, region = "NAME_1")

library(scales)  
library(dplyr)
library(plyr)# hsv colorspace manipulations

distcenters <- ddply(guat_dist, .(id), summarize, clat = mean(lat), 
                     clong = mean(long))
  xolo <- ggplot() + geom_map(data=data, aes(map_id= location, 
                                   fill= value), 
                    map=guat_dist) + expand_limits(x = guat_dist$long, 
                                                   y = guat_dist$lat) +
    geom_polygon(inherit.aes =T, color="white")+
  scale_fill_gradient2(low = muted("blue"), mid = "white",  midpoint = 
                         (range(data$value)[2]-range(data$value)[1])/2, 
                       high = muted("red"), limits = c(min(data$value), max(data$value))) +  
    geom_text(data=distcenters, aes(x= clong, y= clat, label=id), size=2.5)+
     theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) + 
  ggtitle("Guatemala ZIKA") +coord_map()
  xolo

nameindata <- levels(factor(data$location))
nameinmap <- levels(factor(guat_dist$id))
data$location <- nameinmap

nameindata[which(!nameindata %in% nameinmap)]
nameinmap[which(!nameinmap %in% nameindata)]

data <- data[ ! data$location %in% c("Nor_Oriente", "Central", "El_Quiche", 
                             "Ixcan", "Nor_Occidente", "Nor_Oriente"), ]
