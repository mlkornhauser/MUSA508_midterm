#MUSA 508 Midterm
#Maddy Kornhauser & Adam Ghazzawi
#Flymiamibro

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
library(osmdata)
library(Hmisc)
library(tidycensus)

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

palette5 <- c('#f765b8','#f98dc9','#d7fffe','#a8f6f8', '#27fdf5')

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

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    dplyr::summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

############
# MIAMI BASE
############

#Open Street Map Workflow
#install.packages("osmdata")
#https://wiki.openstreetmap.org/wiki/Map_Features
#Check above link for list of available features

# I moved other GEOJSON links from the Miami data portal to the end of the script.
miami.base <- 
  st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union() #Need to keep miami.base unprojected to pull and filter OSM data

miami.base.sf <- miami.base %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') 

#Setting the bounding box
xmin = st_bbox(miami.base)[[1]]
ymin = st_bbox(miami.base)[[2]]
xmax = st_bbox(miami.base)[[3]]  
ymax = st_bbox(miami.base)[[4]]

#Housing data from Ken
miami <- st_read('data/studentsData.geojson')
miami.sf  <- miami %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(miami.base.sf))

ggplot() +
  geom_sf(data = miami.base.sf, fill = "grey40") +
  geom_sf(data = miami.sf, aes(colour = q5(SalePrice)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5) +
  labs(title="Sales Price, Miami") +
  mapTheme()

##################################
# DATA PULLS & FEATURE ENGINEERING
##################################
st_c <- st_coordinates #needed for the NN code

# #----Assisted Living (GEOJSON)
# assisted_living <- st_read('https://opendata.arcgis.com/datasets/9bb1ec069f134635b6fcb0173408a23d_0.geojson')
# assisted.sf <- assisted_living%>%
#   st_transform(st_crs(miami.base.sf)) %>%
#   st_as_sf() 
# assisted.sf <- st_join(assisted.sf, miami.base.sf, join = st_intersects, left = FALSE)
# 
# miami.sf <-
#   miami.sf %>%
#   mutate(
#     assisted_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(assisted.sf)), 1),
#     assisted_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(assisted.sf)), 2),
#     assisted_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(assisted.sf)), 3),
#     assisted_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(assisted.sf)), 4))

#----Bars (OSM)
bars <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("bar", "pub", "biergarten")) %>%
  osmdata_sf()
bars <- 
  bars$osm_points %>%
  .[miami.base,] 

bars.sf <-
  bars %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf))%>%
  distinct()

# miami.sf$bars.buffer_660 = #I think it would be good practice to list the number of feet in the column name
#   st_buffer(miami.sf, 660) %>%
#   aggregate(mutate(bars.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$bars.buffer_660[is.na(miami.sf$bars.buffer_660)] <- 0
# 
# miami.sf$bars.buffer_2640 =
#   st_buffer(miami.sf, 2640) %>%
#   aggregate(mutate(bars.sf, counter = 1),., sum) %>%
#   pull(counter) 
# miami.sf$bars.buffer_2640[is.na(miami.sf$bars.buffer_2640)] <- 0

miami.sf <-
  miami.sf %>%
  mutate(
    bars_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 1),
    bars_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 2),
    bars_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 3),
    bars_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 4))

#----Beaches (GEOJSON)
beaches <- st_read('https://opendata.arcgis.com/datasets/d0d6e6c9d47145a0b05d6621ef29d731_0.geojson') #pull data
beaches.sf <- beaches %>% #project and convert to sf object
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() %>%
  st_union()

ggplot() + geom_sf(data = beaches.sf)

BeachDistance <- st_distance(miami.sf, beaches.sf, by_element = TRUE)
BeachDistance <- as.vector(BeachDistance)
miami.sf$BeachDist <- BeachDistance

#----Coastline (OSM)
coast <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = 'natural', value = c("coastline")) %>%
  osmdata_sf()
coast <-
  coast$osm_points %>%
  .[miami.base,]

coast.sf <-
  coast %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf)) %>%
  distinct() %>%
  st_union

CoastDistance <- st_distance(miami.sf, coast.sf)
CoastDistance <- as.vector(CoastDistance)
miami.sf$CoastDist <- CoastDistance

#----Entertainment (OSM)
entertainment <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = 'amenity', value = c("arts_centre", "cinema", "theatre")) %>%
  osmdata_sf()
entertainment <-
  entertainment$osm_points %>%
  .[miami.base,]

entertainment.sf <-
  entertainment %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf)) %>%
  distinct()

# miami.sf$entertainment.buffer_2460 =
#   st_buffer(miami.sf, 2460) %>%
#   aggregate(mutate(entertainment.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$entertainment.buffer_2460[is.na(miami.sf$entertainment.buffer_2460)] <- 0
# 
# miami.sf$entertainment.buffer_5280 =
#   st_buffer(miami.sf, 5280) %>%
#   aggregate(mutate(entertainment.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$entertainment.buffer_5280[is.na(miami.sf$entertainment.buffer_5280)] <- 0

miami.sf <-
  miami.sf %>%
  mutate(
    ent_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(entertainment.sf)), 1),
    ent_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(entertainment.sf)), 2),
    ent_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(entertainment.sf)), 3),
    ent_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(entertainment.sf)), 4))

# #----Hospitals (OSM)
# hospitals <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
#   add_osm_feature(key = 'amenity', value = c("hospital")) %>%
#   osmdata_sf()
# hospitals <-
#   hospitals$points %>%
#   .[miami.base,] 
# 
# ggplot() +
#   geom_sf(data = miami.base) +
#   geom_sf(data = hospitals, fill = "purple")
# 
# ggplot() + 
#   geom_sf(data=miami.base, fill="grey") +
#   geom_sf(data=bars, colour="red", size=.75) +
#   geom_sf(data=libraries, colour="orange", size=1) +
#   geom_sf(data=parking, colour="purple", size=.5)
# 
# hospitals.sf <- 
#   hospitals %>%
#   dplyr::select(geometry) %>%
#   na.omit() %>%
#   st_transform(st_crs(miami.sf)) %>%
#   distinct()
# 
# miami.sf$hospitals_5280 =
#   st_buffer(miami.sf, 5280) %>%
#   aggregate(mutate(hospitals.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$hospitals_5280[is.na(miami.sf$hospitals_5280)] <- 0

#----Hotel (GEOJSON)
hotel <- st_read('https://opendata.arcgis.com/datasets/d37bbc15e7304b4ca4607783283147b7_0.geojson') 
hotel.sf <- hotel %>%
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() 
hotel.sf <- st_join(hotel.sf, miami.base.sf, join = st_intersects, left = FALSE)

hotel.sf <- 
  hotel %>%
  na.omit() %>%
  st_transform(st_crs(miami.sf)) %>%
  distinct()

miami.sf <-
  miami.sf %>%
  mutate(
    hotel_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(hotel.sf)), 1),
    hotel_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(hotel.sf)), 2),
    hotel_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(hotel.sf)), 3),
    hotel_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(hotel.sf)), 4))

# #----Libraries (OSM)
# libraries <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
#   add_osm_feature(key = 'amenity', value = c("library")) %>%
#   osmdata_sf()
# libraries <- 
#   libraries$osm_points %>%
#   .[miami.base,]
# 
# libraries.sf <-
#   libraries %>%
#   dplyr::select(geometry) %>%
#   na.omit() %>%
#   st_transform(st_crs(miami.sf)) %>%
#   distinct()
# 
# miami.sf$libraries.buffer_2640 =
#   st_buffer(miami.sf, 2640) %>%
#   aggregate(mutate(libraries.sf, counter = 1),., sum) %>%
#   pull(counter)

#----Malls (GEOJSON)
malls <- st_read('https://opendata.arcgis.com/datasets/cb24d578246647a9a4c57bbd80c1caa8_0.geojson') 
malls.sf <- malls %>%
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() 
malls.sf <- st_join(malls.sf, miami.base.sf, join = st_intersects, left = FALSE)

miami.sf <-
  miami.sf %>%
  mutate(
    malls_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(malls.sf)), 1),
    malls_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(malls.sf)), 2),
    malls_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(malls.sf)), 3),
    malls_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(malls.sf)), 4))

#----Marina (OSM)
marina <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'leisure', value = c("marina")) %>%
  osmdata_sf()
marina  <- 
  marina$osm_polygons %>%
  .[miami.base,] 

marina.sf <-
  marina %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.sf)) %>%
  distinct() %>%
  st_centroid()

miami.sf <-
  miami.sf %>%
  mutate(
    marina_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(marina.sf)), 1),
    marina_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(marina.sf)), 2),
    marina_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(marina.sf)), 3),
    marina_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(marina.sf)), 4))

#----Parking (OSM)
# parking <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
#   add_osm_feature(key = 'amenity', value = c("parking")) %>%
#   osmdata_sf()
# parking <- 
#   parking$osm_points %>%
#   .[miami.base,]
# 
# parking.sf <-
#   parking %>%
#   dplyr::select(geometry) %>%
#   na.omit() %>%
#   st_transform(st_crs(miami.sf)) %>%
#   distinct()
# 
# miami.sf$parking_660 =
#   st_buffer(miami.sf, 660) %>%
#   aggregate(mutate(entertainment.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$parking_660[is.na(miami.sf$parking_660)] <- 0
# 
# miami.sf$parking_1320 =
#   st_buffer(miami.sf, 1320) %>%
#   aggregate(mutate(entertainment.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$parking_1320[is.na(miami.sf$parking_1320)] <- 0
# 
# miami.sf <-
#   miami.sf %>%
#   mutate(
#     parking_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(parking.sf)), 1),
#     parking_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(parking.sf)), 2),
#     parking_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(parking.sf)), 3),
#     parking_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(parking.sf)), 4))

#----Parks (GEOJSON)
# parks <- st_read('https://opendata.arcgis.com/datasets/0228d15b2f004758adfdbb4fd71bae10_0.geojson')
# parks.sf <- parks %>%
#   st_transform(st_crs(miami.base.sf)) %>%
#   st_as_sf()
# parks.sf <- st_join(parks.sf, miami.base.sf, join = st_intersects, left = FALSE)
# 
# miami.sf$parks_660max =
#   st_buffer(miami.sf, 660) %>%
#   aggregate(mutate(parks.sf, counter = SQFT),., max) %>%
#   pull(counter)
# miami.sf$parks_660max[is.na(miami.sf$parks_660max)] <- 0
# 
# miami.sf$parks_2640max =
#   st_buffer(miami.sf, 2640) %>%
#   aggregate(mutate(parks.sf, counter = SQFT),., max) %>%
#   pull(counter)
# miami.sf$parks_2640max[is.na(miami.sf$parks_2640max)] <- 0
# 
# miami.sf$parks_2640 =
#   st_buffer(miami.sf, 2640) %>%
#   aggregate(mutate(parks.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$parks_2640[is.na(miami.sf$parks_2640)] <- 0

#----Post Offices (GEOMJSON)
# PO <- st_read('https://opendata.arcgis.com/datasets/74d93cf9e9a44feba3288263a36a6659_0.geojson') 
# PO.sf <- PO %>%
#   st_transform(st_crs(miami.base.sf)) %>%
#   st_as_sf()
# PO.sf <- st_join(PO.sf, miami.base.sf, join = st_intersects, left = FALSE)
# ggplot() + geom_sf(data = miami.base.sf) + geom_sf(data = PO.sf)
# 
# miami.sf <-
#   miami.sf %>%
#   mutate(
#     PO_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(PO.sf)), 1))

#----Restaurants (OSM)
restaurants <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("cafe", "restaurant")) %>%
  osmdata_sf()
restaurants <- 
  restaurants$osm_points %>%
  .[miami.base,]

restaurants.sf <- 
  restaurants %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.sf)) %>%
  distinct()

# miami.sf$restaurants_2640 =
#   st_buffer(miami.sf, 2640) %>%
#   aggregate(mutate(restaurants.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$restaurants_2640[is.na(miami.sf$restaurants_2640)] <- 0

miami.sf <-
  miami.sf %>%
  mutate(
    restaurants_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(restaurants.sf)), 1),
    restaurants_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(restaurants.sf)), 2),
    restaurants_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(restaurants.sf)), 3),
    restaurants_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(restaurants.sf)), 4))

#----Schools (GEOJSON)
schools <- st_read('https://opendata.arcgis.com/datasets/d3db0fce650d4e40a5949b0acae6fe3a_0.geojson') 
schools.sf <- schools %>%
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() 
schools.sf <- st_join(schools.sf, miami.base.sf, join = st_intersects, left = FALSE)

schools.sf <-
  schools %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf))%>%
  distinct()

miami.sf <-
  miami.sf %>%
  mutate(
    schools_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(schools.sf)), 1),
    schools_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(schools.sf)), 2),
    schools_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(schools.sf)), 3),
    schools_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(schools.sf)), 4))

# miami.sf$schools.buffer_5280 =
#   st_buffer(miami.sf, 5280) %>%
#   aggregate(mutate(schools.sf, counter = 1),., sum) %>%
#   pull(counter)
# miami.sf$schools.buffer_5280[is.na(miami.sf$schools.buffer_5280)] <- 0

#----Water (GEOJSON)
# water <- st_read('https://opendata.arcgis.com/datasets/b44862ec4a1447c09fc6ff0e3d70f81a_0.geojson') 
# water.sf <- water %>%
#   st_transform(st_crs(miami.base.sf)) %>%
#   st_as_sf() %>%
#   st_crop(miami.base.sf) %>%
#   st_union ()
# 
# ggplot() + geom_sf(data = water.sf)
# 
# WaterDistance <- st_distance(miami.sf, water.sf, by_element = TRUE)
# WaterDistance <- as.numeric(WaterDistance)
# miami.sf$WaterDist <- WaterDistance

#------Other Calculated Variables

miami.sf <- miami.sf %>% 
  mutate(Age = 2020 - YearBuilt) %>%
  mutate(BR_4more = Bed > 4) %>%
  mutate(post2000 = as.numeric(YearBuilt > 2000)) %>%
  mutate(PriceperSQFT = SalePrice / ActualSqFt) 
miami.sf$Zip.short <- substr(miami.sf$Property.Zip, 1, 5)

miami.sf <- 
  miami.sf %>%
  mutate(Bed.cat = case_when(
    Bed >= 0 & Bed < 3  ~ "Up to 2 Beds",
    Bed >= 3 & Bed < 4  ~ "3 Beds",
    Bed >= 4                    ~ "4+ Beds"))

miami.sf <- 
  miami.sf %>%
  mutate(Bath.cat = case_when(
    Bath >= 0 & Bath < 3  ~ "Up to 2 Baths",
    Bath >= 3 & Bath < 4  ~ "3 Baths",
    Bath >= 4                    ~ "4+ Baths"))

miami.sf <-
  miami.sf %>%
  mutate(Zip_33139 = as.numeric(ifelse(miami.sf$Zip.short == "33139", 1, 0)))
miami.sf$Zip_33139

#Pool
poollist <- list("Pool 6' res BETTER 3-8' dpth, tile 250-649 sf",
                 "Pool 6' res AVG 3-8' dpth, plain feat 250-649 sf",
                 "Pool 8' res BETTER 3-8' dpth, tile 650-1000 sf",
                 "Pool COMM AVG 3-6' dpth, plain feat 15x30 av size",
                 "Luxury Pool - Good.",
                 "Luxury Pool - Better",
                 "Luxury Pool - Better")
miami.sf <- 
  miami.sf %>%
  mutate(Pool_1 = XF1 %in% poollist, 
         Pool_2 = XF2 %in% poollist, 
         Pool_3 = XF3 %in% poollist)
miami.sf$Pool <- ifelse(miami.sf$Pool_1 == "TRUE" | 
                          miami.sf$Pool_2 == "TRUE" | 
                          miami.sf$Pool_3 == "TRUE", 1, 0)
drop <- c("Pool_1","Pool_2","Pool_3")
miami.sf <- miami.sf[,!(names(miami.sf) %in% drop)]

#Luxury Pool
luxpoollist <- list( "Luxury Pool - Good.",
                     "Luxury Pool - Better",
                     "Luxury Pool - Better")
miami.sf <- 
  miami.sf %>%
  mutate(LuxPool_1 = XF1 %in% luxpoollist, LuxPool_2 = XF2 %in% luxpoollist, LuxPool_3 = XF3 %in% luxpoollist)
miami.sf$LuxPool <- ifelse(miami.sf$LuxPool_1 == "TRUE" | 
                          miami.sf$LuxPool_2 == "TRUE" | 
                          miami.sf$LuxPool_3 == "TRUE", 1, 0)
drop <- c("LuxPool_1","LuxPool_2","LuxPool_3")
miami.sf <- miami.sf[,!(names(miami.sf) %in% drop)]

#Dock
docklist <- list("Dock - Wood on Light Posts",      
                 "Dock - Wood Girders on Concrete Pilings",
                 "Dock - Concrete Griders on Concrete Pilings")
miami.sf <- 
  miami.sf %>%
  mutate(
    Dock_1 = XF1 %in% docklist,
    Dock_2 = XF2 %in% docklist, 
    Dock_3 = XF3 %in% docklist)
miami.sf$Dock <- ifelse(miami.sf$Dock_1 == "TRUE" | 
                          miami.sf$Dock_2 == "TRUE" | 
                          miami.sf$Dock_3 == "TRUE", 1, 0)
drop <- c("Dock_1","Dock_2","Dock_3")
miami.sf <- miami.sf[,!(names(miami.sf) %in% drop)]

#Hot tub
hottublist <- list("Jacuzzi",      
                 "Whirlpool - Attached to Pool (whirlpool area only)")
miami.sf <- 
  miami.sf %>%
  mutate(
    Hottub_1 = XF1 %in% hottublist,
    Hottub_2 = XF2 %in% hottublist, 
    Hottub_3 = XF3 %in% hottublist)
miami.sf$Hottub <- ifelse(miami.sf$Hottub_1 == "TRUE" | 
                          miami.sf$Hottub_2 == "TRUE" | 
                          miami.sf$Hottub_3 == "TRUE", 1, 0)
drop <- c("Hottub_1","Hottub_2","Hottub_3")
miami.sf <- miami.sf[,!(names(miami.sf) %in% drop)]

#Patio
patiolist <- list("Patio - Screened over Concrete Slab",
                  "Patio - Concrete Slab",
                  "Patio - Terrazzo, Pebble",
                  "Patio - Brick, Tile, Flagstone",
                  "Patio - Wood Deck",
                  "Patio - Concrete Slab w/Roof Aluminum or Fiber",
                  "Patio - Concrete stamped or stained",
                  "Patio - Marble")
miami.sf <- 
  miami.sf %>%
  mutate(
    Patio_1 = XF1 %in% patiolist,
    Patio_2 = XF2 %in% patiolist, 
    Patio_3 = XF3 %in% patiolist)
miami.sf$Patio <- ifelse(miami.sf$Patio_1 == "TRUE" | 
                          miami.sf$Patio_2 == "TRUE" | 
                          miami.sf$Patio_3 == "TRUE", 1, 0)
drop <- c("Patio_1","Patio_2","Patio_3")
miami.sf <- miami.sf[,!(names(miami.sf) %in% drop)]

#Elevator
elevlist <- list("Elevator - Passenger")
miami.sf <- 
  miami.sf %>%
  mutate(
    elev_1 = XF1 %in% elevlist,
    elev_2 = XF2 %in% elevlist, 
    elev_3 = XF3 %in% elevlist)
miami.sf$Elevator <- ifelse(miami.sf$elev_1 == "TRUE" | 
                             miami.sf$elev_2 == "TRUE" | 
                             miami.sf$elev_3 == "TRUE", 1, 0)
drop <- c("elev_1","elev_2","elev_3")
miami.sf <- miami.sf[,!(names(miami.sf) %in% drop)]

#Carport / Garage
carportlist <- list("Carport - Aluminum - With Floor",
                    "Carport - Wood - Built-up Tar & Gravel",
                    "Carport - Aluminum - No Floor")

miami.sf <- 
  miami.sf %>%
  mutate(
    Carport_1 = XF1 %in% carportlist,
    Carport_2 = XF2 %in% carportlist, 
    Carport_3 = XF3 %in% carportlist)
miami.sf$Carport <- ifelse(miami.sf$Carport_1 == "TRUE" | 
                           miami.sf$Carport_2 == "TRUE" | 
                           miami.sf$Carport_3 == "TRUE", 1, 0)
drop <- c("Carport_1","Carport_2","Carport_3")
miami.sf <- miami.sf[,!(names(miami.sf) %in% drop)]

# #AC <- not statistically significant
# AClist <- list("Central A/C (Aprox 400 sqft/Ton)")
# 
# miami.sf <- 
#   miami.sf %>%
#   mutate(
#     AC_1 = XF1 %in% AClist,
#     AC_2 = XF2 %in% AClist, 
#     AC_3 = XF3 %in% AClist)
# miami.sf$AC <- ifelse(miami.sf$AC_1 == "TRUE" | 
#                            miami.sf$AC_2 == "TRUE" | 
#                            miami.sf$AC_3 == "TRUE", 1, 0)

# #Tikihut <- Not statistically significant
# tikilist <- list("Tiki Hut - Better Thatch roof, plumb, elec, large",
#                  "Tiki Hut - Standard Thatch roof w/poles")
# 
# miami.sf <- 
#   miami.sf %>%
#   mutate(
#     Tiki_1 = XF1 %in% tikilist,
#     Tiki_2 = XF2 %in% tikilist, 
#     Tiki_3 = XF3 %in% tikilist)
# miami.sf$Tiki <- ifelse(miami.sf$Tiki_1 == "TRUE" | 
#                            miami.sf$Tiki_2 == "TRUE" | 
#                            miami.sf$Tiki_3 == "TRUE", 1, 0)

#Neighborhood data
nhoods <- st_read('https://opendata.arcgis.com/datasets/2f54a0cbd67046f2bd100fb735176e6c_0.geojson')

miamibeach  <-
  st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME == "MIAMI BEACH")
miamibeach.sf <- 
  miamibeach %>%
  dplyr::select(geometry) %>%
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf()

nhoods <- st_union(nhoods, miamibeach) %>% ungroup() 
nhoods$LABEL[nhoods$FID==107] <- "Miami Beach"

nhoods.sf <-
  nhoods %>%
  dplyr::select(LABEL, geometry) %>%
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() 
nhoods.sf <- st_cast(nhoods.sf, "POLYGON")
plot(nhoods.sf)

miami.sf <- st_join(st_centroid(miami.sf), nhoods.sf, join = st_intersects, left = TRUE, largest = TRUE)

PricebyNHood <- 
  miami.sf %>% group_by(LABEL.x) %>%
  dplyr::summarise(AvgSalePrice = mean(SalePrice),
                   MedSalePrice = median(SalePrice))

PricebyNHood[order(-PricebyNHood$AvgSalePrice),]
PricebyNHood[order(-PricebyNHood$MedSalePrice),]

topavgnhoodlist <- list("San Marco Island",
                        "Biscayne Island",
                        "South Grove Bayside",
                        "Baypoint",
                        "Belle Island")
miami.sf <- miami.sf %>%
  mutate(TopAvgNhood = LABEL.x %in% topavgnhoodlist)
miami.sf$TopAvgNhood <- ifelse(miami.sf$TopAvgNhood == "TRUE", 1, 0)

topmednhoodlist <- list("San Marco Island",
                        "South Grove Bayside",
                        "Belle Island",
                        "Baypoint",
                        "Fair Isle")
miami.sf <- miami.sf %>%
  mutate(TopMedNhood = LABEL.x %in% topmednhoodlist)
miami.sf$TopMedNhood <- ifelse(miami.sf$TopMedNhood == "TRUE", 1, 0)

# miami.sf <- miami.sf %>%
#   mutate(SanMarcoIs = LABEL == "San Marco Island")
# miami.sf$SanMarcoIs <- ifelse(miami.sf$SanMarcoIs == "TRUE", 1, 0)

#Census Data
#vars18 <- load_variables(2018, "acs5")
#View(vars18)
tracts18 <-
  get_acs(geography = "tract", variables = c("B01003_001E", "B02001_002E", "B01002_001E",
                                             "B19013_001E", "B25064_001E", "B17020_002E"),
          year=2018, state="Florida", county="Miami-Dade", geometry=T, output = "wide") %>%
  st_transform(st_crs(miami.sf))%>%
  rename(TotalPop = B01003_001E,
         Whites = B02001_002E,
         MedAge = B01002_001E,
         MedHHInc = B19013_001E,
         MedRent = B25064_001E,
         Poverty = B17020_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctPov = ifelse(TotalPop > 0, Poverty / TotalPop,0),
         year = "2018") %>%
  dplyr::select(-Whites, -Poverty) 
miami.sf <- st_join(st_centroid(miami.sf), tracts18, join = st_intersects, left = TRUE, largest = TRUE)

# PopByHome <-
#   j %>% 
#   group_by(Property.Address) %>%
#   dplyr::summarize(sumPop = sum(TotalPop),
#                    PctWhite = mean(pctWhite),
#                    AvgMedIncome = mean(MedHHInc)) %>%
#   st_drop_geometry() %>%
#   left_join(miami.join.sf) %>%
#   st_sf()

ggplot() +
  geom_sf(data = miami.base.sf) +
  geom_sf(data = PopByHome, aes(colour = AvgMedIncome), 
          show.legend = "point", size = .75)


# tracts18 <- st_join(tracts18, miami.sf)
Centroids18 <- st_centroid(tracts18) 
join <- st_join(tracts18, miami.sf, largest = TRUE) 

# Near very expensive house
neighbor.1M <- miami.sf %>% subset(SalePrice > 1000000)

neighbor.1M.sf <-
  neighbor.1M %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf))%>%
  distinct()

miami.sf$neighbor.1M_1320 =
  st_buffer(miami.sf, 1320) %>%
  aggregate(mutate(neighbor.1M.sf, counter = 1),., sum) %>%
  pull(counter)
miami.sf$neighbor.1M_1320[is.na(miami.sf$neighbor.1M_1320)] <- 0

neighbor.10M <- miami.sf %>% subset(SalePrice > 10000000)

neighbor.10M.sf <-
  neighbor.10M %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf))%>%
  distinct()

miami.sf$neighbor.10M_1320 =
  st_buffer(miami.sf, 1320) %>%
  aggregate(mutate(neighbor.10M.sf, counter = 1),., sum) %>%
  pull(counter)
miami.sf$neighbor.10M_1320[is.na(miami.sf$neighbor.10M_1320)] <- 0

#DJ Khaled
colnames(miami.sf)
DJKhaled <- miami.sf %>% subset(Property.Address == "4609 PINE TREE DR")

DJKhaled.sf <-
  DJKhaled %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf))%>%
  distinct()

miami.sf$DJKhaled_1320 =
  st_buffer(miami.sf, 1320) %>%
  aggregate(mutate(DJKhaled.sf, counter = 1),., sum) %>%
  pull(counter)
miami.sf$DJKhaled_1320[is.na(miami.sf$DJKhaled_1320)] <- 0


ggplot() + geom_sf(data = tracts18) + geom_sf(data = miami.base.sf, fill = "transparent", colour = "red")



######################
# EXPLORATORY ANALYSIS
######################
# #running list of vars: 
# marina_nn1
# ent_nn4
# SalePrice, Age, post2000, AdjustedSqFt, 
# Stories, Bed, Bath, bars_nn4,
# ent_nn4, malls_nn4, hotel_nn4,
# assisted_nn1, BeachDist, WaterDist, 
# parks_660, parks_2640max
# Pool, LuxPool, Dock
# Hottub (meh)

#Code for corr plots
colnames(miami.sf)
st_drop_geometry(miami.sf) %>% 
  dplyr::select(SalePrice, pctWhite.y.1, pctPov.y) %>% #Choose variables from main dataset
  gather(Variable, Value, -SalePrice) %>% #convert to long format
  ggplot(aes(Value, SalePrice)) + #plot
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

colnames(miami.sf)

#Corr matrix
numericVars <- 
  select_if(st_drop_geometry(miami.sf), is.numeric) %>%
  dplyr::select(SalePrice, AdjustedSqFt, Stories, Bed, Bath, bars_nn4, malls_nn4, 
                hotel_nn4, BeachDist, CoastDist, Pool, LuxPool, Dock, Patio, Elevator,
                TopAvgNhood, TopMedNhood, Zip_33139, neighbor.10M_1320, pctWhite.y.1, pctPov.y) %>%
  na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#f765b8", "white", "#27fdf5"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

#####################
# REGRESSION WORKFLOW
#####################
#Subset miami dataframe into all homes with listed sale prices
sales <- subset(miami.sf, SalePrice > 0)

#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(sales$SalePrice), 
  p = .60, list = FALSE)

miami.training <- sales[inTrain,] 
miami.test <- sales[-inTrain,]  

#Multivariate regression
reg1 <- lm(SalePrice ~ ., data = st_drop_geometry(miami.training) %>% 
             dplyr::select(SalePrice, marina_nn1, ent_nn4, Age, post2000,
                           AdjustedSqFt, Stories, bars_nn4, malls_nn4, 
                           hotel_nn4, BeachDist, Pool, LuxPool, Dock, Patio,
                           Elevator, ent_nn4, ent_nn3, ent_nn2, ent_nn1, marina_nn4, 
                           marina_nn3, marina_nn2, marina_nn1, restaurants_nn4, restaurants_nn3,
                           restaurants_nn2, restaurants_nn1, Zip_33139, neighbor.10M_1320, pctWhite.y.1,
                           pctPov.y, TopMedNhood))
summary(reg1)

#Predicting test values using regression
reg1_predict <- predict(reg1, newdata = miami.test)

#Calculating the error terms
rmse.train <- caret::MAE(predict(reg1), miami.training$SalePrice)
rmse.test <- caret::MAE(reg1_predict, miami.test$SalePrice)

cat("Train MAE: ", as.integer(rmse.train), " \n","Test MAE: ", as.integer(rmse.test))

#Plot the error terms
preds.train <- data.frame(pred   = predict(reg1),
                          actual = miami.training$SalePrice,
                          source = "training data")
preds.test  <- data.frame(pred   = reg1_predict,
                          actual = miami.test$SalePrice,
                          source = "testing data")
preds <- rbind(preds.train, preds.test)
head(preds)

ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  geom_abline(color = "orange") +
  coord_equal() +
  theme_bw() +
  facet_wrap(~source, ncol = 2) +
  labs(title = "Comparing predictions to actual values",
       x = "Predicted Value",
       y = "Actual Value") +
  theme(
    legend.position = "none"
  )

#Plotting predictions by actual price broken into thirds
preds$cat <- as.numeric(cut2(preds$actual, g=3))
head(preds)

ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
  geom_smooth(data = subset(preds, cat ==1), method = "lm", color = "red") +
  geom_smooth(data = subset(preds, cat ==2), method = "lm", color = "orange") +
  geom_smooth(data = subset(preds, cat ==3), method = "lm", color = "yellow") +
  geom_abline(color = "blue") +
  coord_equal() +
  theme_bw() +
  facet_wrap(~source, ncol = 2) +
  labs(title = "Comparing predictions to actual values",
       x = "Predicted Value",
       y = "Actual Value") +
  theme(
    legend.position = "none"
  )

# Cross validation
fitControl <- trainControl(method = "cv", 
                           number = 10,
                           savePredictions = TRUE)

set.seed(717)
reg1.cv <- 
  train(SalePrice ~ ., data = st_drop_geometry(sales) %>% 
          dplyr::select(SalePrice, AdjustedSqFt, LotSize,
                        Bed, Bath, YearBuilt), 
        method = "lm", 
        trControl = fitControl, 
        na.action = na.pass)

reg1.cv 
#this line of code shows the average values across all 10 folds.
#RMSE = [R] Mean Squared Error
#MAE = Mean Average Error.  I think this is the value that we're interested in.

reg1.cv$resample
#This line of code shows the above values for each individual fold

###################
# SPATIAL PROCESSES
###################

#Summary Table
miami.test <-
  miami.test %>%
  mutate(Regression = "Baseline Regression",
         SalePrice.Predict = predict(reg1, miami.test),
         SalePrice.Error = SalePrice.Predict - SalePrice,
         SalePrice.AbsError = abs(SalePrice.Predict - SalePrice),
         SalePrice.APE = (abs(SalePrice.Predict - SalePrice)) / SalePrice.Predict)

#Plot errors on a map
cv_preds <- reg1.cv$pred

nrow(sales)
nrow(cv_preds)

map_preds <- sales %>% 
  rowid_to_column(var = "rowIndex") %>% 
  left_join(cv_preds, by = "rowIndex") %>% 
  mutate(SalePrice.AbsError = abs(pred - SalePrice)) 

ggplot() +
  geom_sf(data = miami.base.sf, fill = "grey40") +
  geom_sf(data = map_preds, aes(colour = q5(SalePrice.AbsError)),
          show.legend = "point", size = 1) +
  scale_colour_manual(values = palette5,
                      labels=qBr(map_preds,"SalePrice.AbsError"),
                      name="Quintile\nBreaks") +
  labs(title="Absolute sale price errors on the OOF set",
       subtitle = "OOF = 'Out Of Fold'") +
  mapTheme()

#THE SPATIAL LAG
k_nearest_neighbors = 5
#prices
coords <- miami.sf %>%
  dplyr::select(geometry) %>%
  st_centroid() %>%
  st_coordinates()
# k nearest neighbors
neighborList <- knn2nb(knearneigh(coords, k_nearest_neighbors))
spatialWeights <- nb2listw(neighborList, style="W")
miami.sf$lagPrice <- lag.listw(spatialWeights, miami.sf$SalePrice)

#errors
coords.test <-  miami.test %>%
  dplyr::select(geometry) %>%
  st_centroid() %>%
  st_coordinates()
neighborList.test <- knn2nb(knearneigh(coords.test, k_nearest_neighbors))
spatialWeights.test <- nb2listw(neighborList.test, style="W")
miami.test$lagPriceError <- lag.listw(spatialWeights.test, miami.test$SalePrice.AbsError)

ggplot(miami.sf, aes(x=lagPrice, y=SalePrice)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Price as a function of the spatial lag of price",
       caption = "Public Policy Analytics, Figure 6.6",
       x = "Spatial lag of price (Mean price of 5 nearest neighbors)",
       y = "Sale Price") +
  plotTheme()

ggplot(miami.test, aes(x=lagPriceError, y=SalePrice)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Error as a function of the spatial lag of price",
       caption = "",
       x = "Spatial lag of errors (Mean error of 5 nearest neighbors)",
       y = "Sale Price") +
  plotTheme()

#MORAN'S I -- can't run this without the spatial weights object. Need to figure out knearneigh function
moranTest <- moran.mc(miami.test$SalePrice.AbsError, 
                      spatialWeights.test, nsim = 999)

ggplot(as.data.frame(moranTest$res[c(1:999)]), aes(moranTest$res[c(1:999)])) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = moranTest$statistic), colour = "#FA7800",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I",
       subtitle= "Observed Moran's I in orange",
       x="Moran's I",
       y="Count",
       caption="Public Policy Analytics, Figure 6.8") +
  plotTheme()

###############################
# FEATURE ENGINEERING REFERENCE
###############################

#-----Buffer: Count within buffer of each house
#Adding column to main dataset indicating the count of X within the buffer area
miami.sf$bars.buffer_660 = #I think it would be good practice to list the number of feet in the column name
  st_buffer(miami.sf, 660) %>%
  aggregate(mutate(bars.sf, counter = 1),., sum) %>%
  pull(counter) 
miami.sf$bars.buffer_660[is.na(miami.sf$bars.buffer_660)] <- 0

#generate a density plot
ggplot() + geom_sf(data = miami.base.sf, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(bars.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Bars, Miami") +
  mapTheme()

#-----Nearest Neighbor Feature Workflow
#rename st_coordinates for brevity
st_c <- st_coordinates

#generate nearest neighbor values in the miami.sf dataset
miami.sf <-
  miami.sf %>%
  mutate(
    bars_nn1 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 1),
    bars_nn2 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 2),
    bars_nn3 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 3),
    bars_nn4 = nn_function(st_c(st_centroid(miami.sf)), st_c(st_centroid(bars.sf)), 4))

#plot NN count over space - Should increase or decrease?
miami.sf.plot <- miami.sf %>%
  st_drop_geometry() %>%
  dplyr::select(Folio, starts_with("bars_")) %>%
  tidyr::pivot_longer(cols = -Folio, names_to = "bars_nn")

ggplot(miami.sf.plot, aes(x = bars_nn, y = value, group = Folio)) +
  geom_line(alpha = 0.05, color = "firebrick") +
  theme_bw()

######################
# Online JSON sources
######################
head(tracts)
tracts <- st_read('https://opendata.arcgis.com/datasets/d48ccb2860804468aef0123cd4509dae_0.geojson') %>%
  st_transform(st_crs(miami))
tracts <- tracts[muni, op = st_intersects] #this isn't right.

# muni.sf <- st_read('https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson') %>%
#   st_transform(st_crs(miami.base)) %>%
#   filter(MUNICID =="01" | MUNICID =="02") %>%
#   st_as_sf()

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

landmarks <- st_read('https://opendata.arcgis.com/datasets/70a14825e66f4f0eb28d2a9cceba1761_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
landmarks <- st_join(landmarks, muni, join = st_intersects, left = FALSE)

culture <- st_read('https://opendata.arcgis.com/datasets/70c48f0eb067448c8a787cfa1c1c3bb9_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
culture <- st_join(culture, muni, join = st_intersects, left = FALSE)

commercial <- st_read('https://opendata.arcgis.com/datasets/fb8303c577c24ea386a91be7329842be_0.geojson')%>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
commercial <- st_join(commercial, muni, join = st_intersects, left = FALSE)

rail <- st_read('https://opendata.arcgis.com/datasets/ee3e2c45427e4c85b751d8ad57dd7b16_0.geojson')%>%
  st_transform(st_crs(miami))%>%
  st_as_sf()
rail <- st_join(rail, muni, join = st_intersects, left = FALSE)

#Looking at types ofbusinesses
commercial_group <- commercial %>% count(BUSDESC)
commercial_group <- commercial_group[order(-commercial_group$n),]
commercial_group