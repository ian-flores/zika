
library(maptools)
library(rgeos)
library(gpclib)
library("RColorBrewer")
library(ggplot2)
library(scales)  
library(dplyr)
library(plyr)

guat <- readShapePoly("GTM_adm/GTM_adm1.shp")
plot(guat)

data <- list.files(path = "data_country/Guatemala/SEMEPI/data", full.names = T) 
data <- do.call(rbind,lapply(data,read.csv, na.strings=c("","NA")))
head(data)
data <- subset(data, location_type=="municipality")
data$location <- substring(data$location,11)
data <- subset(data, data_field_code=="GT0001")

data <- subset(data, report_date == "2016-06-28")
guat_dist <- fortify(guat, region = "NAME_1")

centre <- coordinates(guat)
centre <- data.frame(centre)
centre$id <- guat@data$NAME_1

data <- data[ ! data$location %in% c("Nor_Oriente", "Central", "El_Quiche", 
                                     "Ixcan", "Nor_Occidente", "Nor_Oriente"), ]
nameindata <- levels(factor(data$location))
nameinmap <- levels(factor(guat_dist$id))
data$location <- nameinmap

xolo <- ggplot() + geom_map(data=data, aes(map_id= location, 
                                   fill= value), 
                    map=guat_dist) + expand_limits(x = guat_dist$long, 
                                                   y = guat_dist$lat) +
    geom_polygon(data=guat_dist, aes(x=long, y=lat, group=group), color="white", fill=NA)+
  scale_fill_gradient2(low = muted("blue"), mid = "white",  midpoint = 
                         (range(data$value)[2]-range(data$value)[1])/2, 
                       high = muted("red"), limits = c(min(data$value), max(data$value)), 
                       name="Zika Cases") +  
    geom_label(data=centre, aes(x= X1, y= X2, label=id), size=2.5)+
     theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) + 
  ggtitle("Guatemala ZIKA") + coord_map()
xolo
