library("ggspatial")
library('sf')
library("rnaturalearth")
library("rnaturalearthdata")
library("grid")
library("ggplot2")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)



ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5, 10), ylim = c(45, 61))

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

geom_text(data= world_points,aes(x=X, y=Y, label=name),
          color = "darkblue", fontface = "bold", check_overlap = FALSE)

ggplot(data = world) +
  geom_sf() +
  annotate(geom = "text", x = 4, y = 56.3, label = "North Sea", 
           fontface = "italic", color = "grey60", size = 10) +
  annotate(geom = "text", x = 3, y = 59, label = "NNS", 
           fontface = "bold", color = "darkblue", size = 3)+
  annotate(geom = "text", x = 0.5, y = 55.75, label = "CNS_pure", 
           fontface = "bold", color = "darkblue", size = 3)+
  annotate(geom = "text", x = 4.1, y = 57.3, label = "CNS_mix", 
           fontface = "bold", color = "darkblue", size = 3)+
  annotate(geom = "text", x = 0.85, y = 51.76, label = "SNS_Bradwell", 
           fontface = "bold", color = "darkblue", size = 3)+
  annotate(geom = "text", x = 1.64, y = 52.21, label = "SNS_Sizewell", 
           fontface = "bold", color = "darkblue", size = 3)+
  annotate(geom = "text", x = 1.57, y = 50.81, label = "SNS_Channel", 
           fontface = "bold", color = "darkblue", size = 3)+
  annotate(geom = "text", x = -6.14, y = 57.45, label = "West1", 
           fontface = "bold", color = "darkgreen", size = 3)+
  annotate(geom = "text", x = -5.37, y = 58.73, label = "West2", 
           fontface = "bold", color = "darkgreen", size = 3)+
  annotate(geom = "text", x = 5.25, y = 62.53, label = "BS_NO", 
           fontface = "bold", color = "darkred", size = 3)+
  annotate(geom = "text", x = 22, y = 64.14, label = "BS_IC", 
           fontface = "bold", color = "darkred", size = 3)+
  annotate(geom = "text", x = -1.9, y = 53.7, label = "U.K.", 
           fontface = "italic", color = "black", size = 4)+
  annotate(geom = "text", x = -4.3, y = 56.9, label = "Scotland", 
           fontface = "italic", color = "black", size = 4)+
  annotate(geom = "text", x = 0.43, y = 63.5, label = "Norwegian Sea", 
           fontface = "italic", color = "grey60", size = 10)+
  annotate(geom = "text", x = 19.5, y = 62, label = "Bothnia Bay", 
           fontface = "italic", color = "grey60", size = 5)+
  annotate(geom = "text", x = 20, y = 58, label = "Baltic Sea", 
           fontface = "italic", color = "grey60", size = 5)+
  annotate(geom = "text", x = 1.3, y = 50.3, label = "English Channel", 
           fontface = "italic", color = "grey60", size = 5)+
  annotate(geom = "text", x = 9.2, y = 61, label = "Norway", 
           fontface = "italic", color = "black", size = 4)+
  annotate(geom = "text", x = 15, y = 62.5, label = "Sweden", 
           fontface = "italic", color = "black", size = 4)+
  coord_sf(xlim = c(-7,23), ylim = c(50,65), expand = FALSE)

citation()
RStudio.Version()
