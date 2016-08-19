library(maptools)
brasil <- readShapePoly("brazil-administrative/brazil_administrative.shp")
plot(brasil)

names(brasil)
brasil$NAME_1
brasil$regiao_id
brasil$codigo_ibg
brasil$nome
