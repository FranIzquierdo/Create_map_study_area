###Charge neede libraries
library(marmap)
library(ggplot2)
library(mapdata)
library(mapproj)
library(cowplot)
library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")

###Read data
load("data.RData")

### Country MAP
world <- ne_countries(scale = "medium", returnclass = "sf")

ply = data.frame(
  lon = c(-9.8,-9.8,-9.8, -1, -1, -1),
  lat = c(44.3,  43, 42, 42, 43, 44.3))# build a polygon of study area

minimap <- ggplot(data = world) +
  geom_sf(fill="grey", colour="darkgrey") + 
  coord_sf(xlim = c(-15, 15), ylim = c(33, 53), expand = FALSE, datum=NA) +
  geom_polygon(data = ply, aes(x = lon, y = lat), color = "black", 
               fill="#fe9152", alpha = 0.2, size=0.1) +
  xlab(" ") + ylab(" ") + theme_bw()+  theme(panel.spacing = unit(0.1, "cm"))

minimap

### Main MAP
b = getNOAA.bathy(lon1 = -9.8, lon2 = -1, lat1 = 44.3, lat2 = 41.2, 
                  resolution = 1)# get bathymetry data
bf = fortify.bathy(b) # convert bathymetry to data frame 
reg = map_data("world2Hires")#names(reg)#table(reg$region)
reg = subset(reg, region %in% c('Spain', 'Portugal', 'France'))
# set map limits
lons = c(-9.8, -1)
lats = c(46, 41.5)

# create the breaks- and label vectors
ewbrks <- seq(-8,-1,2)
nsbrks <- seq(42,46,1)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(-x, "W"), ifelse(x > 0, paste(x, "E"),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "S"), ifelse(x > 0, paste(x, "N"),x))))


main <- ggplot()+
  
  # add 100m contour
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-100),
               size=c(0.3),
               colour="grey")+ 
  
  # add 250m contour
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-200),
               size=c(0.6),
               colour="grey")+ 
  
  # add coastline
  geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill= "grey", color = "darkgrey") + 
  
  # Add city
  annotate("text", x=-8.2, y=43.1, label= "La Coruna", size=3, fontface= "bold") + 
  annotate("text", x=-6, y=43.3, label= "Gijon", size=3, fontface= "bold") + 
  annotate("text", x=-3.9, y=43.2, label= "Santander", size=3 ,fontface= "bold") + 
  annotate("text", x=-2.55, y=43, label= "Bilbao", size=3,fontface= "bold") + 
  annotate("text", x=-8.3, y=42.3, label= "Vigo", size=3 ,fontface= "bold") + 
  
  
  # add points
  geom_point(data = data, aes(x = lon, y = lat ),
             colour = "black", fill = "black", 
             stroke = .5, size = 0.8, 
             alpha = 1, shape = 21)+
  # configure projection and plot domain
  coord_map(xlim = lons, ylim = lats) +
  # formatting
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  ylab(" ")+xlab(" ")+
  theme_bw() + theme(axis.text.x= element_text(size=8), axis.text.y= element_text(size=8))

main

# COMPLETE MAIN + MINIMAP

areaest<-ggdraw() +
  draw_plot(main) +
  draw_plot(minimap, x =0.652, y = 0.56, width = 0.35, height = 0.35) # posicion minimapa

areaest
ggsave("areaest.jpg")
