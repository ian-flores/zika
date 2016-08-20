
library(maptools)
library(rgeos)
library(gpclib)
library("RColorBrewer")
library(ggplot2)
library(scales)  
library(dplyr)
library(plyr)

###Guatemala###
guat <- readShapePoly("GTM_adm/GTM_adm1.shp")
guat_dist <- fortify(guat, region = "NAME_1")

data <- list.files(path = "data_country/Guatemala/SEMEPI/data", full.names = T) 
data <- do.call(rbind,lapply(data,read.csv, na.strings=c("","NA")))
data <- subset(data, location_type=="municipality")
data$location <- substring(data$location,11)
data <- subset(data, data_field_code=="GT0001")
data$report_date <- as.Date(data$report_date)
data <- within(data, {
  casos <- ave(value, data$location, FUN = cumsum)
})
max_date<- max(data$report_date)
data <- subset(data, report_date==max_date)
nameindata <- levels(factor(data$location))
data <- data[ ! data$location %in% c("Nor_Oriente", "Central", "El_Quiche", 
                                     "Ixcan", "Nor_Occidente", "Nor_Oriente"), ]
nameinmap <- levels(factor(guat_dist$id))
data$location <- nameinmap

centre <- coordinates(guat)
centre <- data.frame(centre)
centre$id <- guat@data$NAME_1



mapa <- function(country="", data, shp){
  plot <- ggplot() + geom_map(data=data, aes(map_id= location, 
                                   fill= casos), 
                    map=shp) + expand_limits(x = shp$long, 
                                                   y = shp$lat) +
    geom_polygon(data=shp, aes(x=long, y=lat, group=group), color="black", fill=NA)+
  scale_fill_gradient2(low = muted("blue"), mid = "white",  midpoint = 
                         (range(data$casos)[2]-range(data$casos)[1])/2, 
                       high = muted("red"), limits = c(min(data$casos), max(data$casos)), 
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
  ggtitle(paste(country, "ZIKA", max_date)) + coord_map()
  return(plot)}
  
  mapa("Guatemala", data=data, shp=guat_dist)
  
### Dominican Republic ###

dom <-  readShapePoly("DOM_adm/DOM_adm1.shp")
dom_dist <- fortify(dom, region="NAME_1")

data <- list.files(path = "data_country/Dominican_Republic/Epidemiological_Bulletin/data", full.names = T) 
data <- do.call(rbind,lapply(data,read.csv, na.strings=c("","NA")))
data <- subset(data, location_type=="province")
data$location <- substring(data$location,20)
data <- subset(data, data_field_code=="DO0001")
data$report_date <- as.Date(data$report_date)
data <- within(data, {
  casos <- ave(value, data$location, FUN = cumsum)
})
max_date<- max(data$report_date)
data <- subset(data, report_date == max_date)
nameindata <- levels(factor(data$location))
nameinmap <- levels(factor(dom_dist$id))
data$location <- nameinmap

centre <- coordinates(dom)
centre <- data.frame(centre)
centre$id <- dom@data$NAME_1

mapa("Dominican Republic", data=data, shp=dom_dist)

### Mexico ###

mex <-  readShapePoly("MEX_adm/MEX_adm1.shp")
mex_dist <- fortify(mex, region="NAME_1")

data <- list.files(path = "data_country/Mexico/DGE_Zika/data", full.names = T) 
data <- do.call(rbind,lapply(data,read.csv, na.strings=c("","NA")))
data <- subset(data, location_type=="state")
data$location <- substring(data$location,7)
data <- subset(data, data_field_code=="MX0001")
data$report_date <- as.Date(data$report_date)
data <- within(data, {
  casos <- ave(value, data$location, FUN = cumsum)
})
max_date<- max(data$report_date)
data <- subset(data, report_date == max_date)
nameindata <- levels(factor(data$location))
nameinmap <- levels(factor(mex_dist$id))
data$location <- nameinmap

centre <- coordinates(mex)
centre <- data.frame(centre)
centre$id <- mex@data$NAME_1

mapa("Mexico", data=data, shp=mex_dist)
