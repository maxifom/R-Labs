library(sf)
library(spData)
library(spDataLarge)
library(tidyverse)

names(world)
plot(world)
summary(world["lifeExp"])
world$name_long
world_small = world %>%
  dplyr::select(iso_a2,name_long,continent) %>%
  head(10)
world_small


world %>% dplyr::select(c(3:6)) %>%
  plot()

world %>% dplyr::select(pop) %>%
  plot()

w_asia = world %>%
  filter(continent=="Asia")
asia = st_union(w_asia)
world %>% dplyr::select(pop) %>%
  plot(reset=FALSE)

#step 4 - plot another layer - our Asian data with a color of RED

#add = TRUE makes it possible
#reset = FALSE is necessary for us to superimpose the subsequent plot

plot(asia, add=TRUE, col="red")

world %>% dplyr::select(continent) %>%
  plot(reset=FALSE)
central = sqrt(world$pop)/10000
world_cents = st_centroid(world,of_largest_polygon = TRUE)
st_geometry(world_cents) %>%
  plot(add=TRUE, cex=central)

w_europe = world %>%
  filter(continent=="Europe")

europe = st_union(w_europe)

russia = world %>% dplyr::filter(name_long=="Russian Federation")

plot(russia) #this gives us all plots of Russia

plot(st_geometry(russia)) #gives us the outline

plot(st_geometry(russia), col="gray", lwd=2) #gives us the outline, fills with gray and line width as 2,
#we can adjust by including expandBB=c(0,0,0.1,1)

plot(w_europe[0], add=TRUE) #let's add the european map as a layer, add=TRUE