
########
# SETUP
########

library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)

root.dir = "https://github.com/mlkornhauser/MUSA508_midterm.git"

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

#################
# LOAD DATA WRANGLING
#################
#Census data

miami <- st_read('data/studentsData.geojson') %>%
  st_transform('ESRI:102728')
muni <- st_read('https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  filter(MUNICID =="01" | MUNICID =="02") 
water <- st_read('https://opendata.arcgis.com/datasets/b44862ec4a1447c09fc6ff0e3d70f81a_0.geojson') %>%
  st_transform(st_crs(miami))

tracts <- st_read('https://opendata.arcgis.com/datasets/d48ccb2860804468aef0123cd4509dae_0.geojson') %>%
  st_transform(st_crs(miami))
tracts <- tracts[muni, op = st_intersects] #this isn't right.


mia_nhoods <- st_read('https://opendata.arcgis.com/datasets/2f54a0cbd67046f2bd100fb735176e6c_0.geojson')

schools <- st_read('https://opendata.arcgis.com/datasets/d3db0fce650d4e40a5949b0acae6fe3a_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
schools <- st_join(schools, muni, join = st_intersects, left = FALSE)

# library <- st_read('https://opendata.arcgis.com/datasets/ab490a5cefd04c12b6b5e53a6b60f41c_0.geojson') %>%
#   st_as_sf(crs = 4326) %>% 
#   st_transform(st_crs(miami))
# library <- st_join(library, muni, join = st_intersects, left = FALSE) 
#there appears to be no libraries within miami city limits? -- check data

hospitals <- st_read('https://opendata.arcgis.com/datasets/0067a0e8b40644f980afa23ad34c32c4_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
hospitals <- st_join(hospitals, muni, join = st_intersects, left = FALSE)

assisted_living <- st_read('https://opendata.arcgis.com/datasets/9bb1ec069f134635b6fcb0173408a23d_0.geojson')%>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
assisted_living <- st_join(assisted_living, muni, join = st_intersects, left = FALSE)

landmarks <- st_read('https://opendata.arcgis.com/datasets/70a14825e66f4f0eb28d2a9cceba1761_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
landmarks <- st_join(landmarks, muni, join = st_intersects, left = FALSE)

PO <- st_read('https://opendata.arcgis.com/datasets/74d93cf9e9a44feba3288263a36a6659_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
PO <- st_join(PO, muni, join = st_intersects, left = FALSE)

hotel <- st_read('https://opendata.arcgis.com/datasets/d37bbc15e7304b4ca4607783283147b7_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
hotel <- st_join(hotel, muni, join = st_intersects, left = FALSE)

malls <- st_read('https://opendata.arcgis.com/datasets/cb24d578246647a9a4c57bbd80c1caa8_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
hotel <- st_join(hotel, muni, join = st_intersects, left = FALSE)

culture <- st_read('https://opendata.arcgis.com/datasets/70c48f0eb067448c8a787cfa1c1c3bb9_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
culture <- st_join(culture, muni, join = st_intersects, left = FALSE)

commercial <- st_read('https://opendata.arcgis.com/datasets/fb8303c577c24ea386a91be7329842be_0.geojson')%>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
commercial <- st_join(commercial, muni, join = st_intersects, left = FALSE)

#Looking at types ofbusinesses
commercial_group <- commercial %>% count(BUSDESC)
commercial_group <- commercial_group[order(-commercial_group$n),]
commercial_group

#need crime data
#need neighborhoods for miami beach

View(miami)
head(miami)

ggplot() + 
  geom_sf(data = muni) +
  geom_sf(data = miami, aes(colour = q5(SalePrice))) 

head(water)

ggplot() +
  geom_sf(data = muni, fill = "grey40") +
  geom_sf(data = nhoods, fill = "transparent") +
  geom_sf(data = miami, aes(colour = q5(SalePrice)))
  



