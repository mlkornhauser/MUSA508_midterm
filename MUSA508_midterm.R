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
# LOAD DATA 
############

#Open Street Map Workflow
#install.packages("osmdata")
#https://wiki.openstreetmap.org/wiki/Map_Features
#Check above link for list of available features

# I moved other GEOJSON links from the Miami data portal to the end of the script.

miami.base <- 
  st_read("https://opendata.arcgis.com/datasets/5ece0745e24b4617a49f2e098df8117f_0.geojson") %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()
#Need to keep miami.base unprojected to pull and filter OSM data

miami.base.sf <- miami.base %>%
  st_as_sf(coords = "geometry", crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') 

#Setting the bounding box
xmin = st_bbox(miami.base)[[1]]
ymin = st_bbox(miami.base)[[2]]
xmax = st_bbox(miami.base)[[3]]  
ymax = st_bbox(miami.base)[[4]]

#----GEOJSON data pulls

miami <- st_read('data/studentsData.geojson')
miami.sf  <- miami %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(miami.base.sf))

beaches <- st_read('https://opendata.arcgis.com/datasets/d0d6e6c9d47145a0b05d6621ef29d731_0.geojson') #pull data
beaches.sf <- beaches %>% #project and convert to sf object
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() 
beaches.sf <- st_join(beaches.sf, miami.base.sf, join = st_intersects, left = FALSE) #join to miami.base

schools <- st_read('https://opendata.arcgis.com/datasets/d3db0fce650d4e40a5949b0acae6fe3a_0.geojson') 
schools.sf <- schools %>%
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() 
schools.sf <- st_join(schools.sf, miami.base.sf, join = st_intersects, left = FALSE)

water <- st_read('https://opendata.arcgis.com/datasets/b44862ec4a1447c09fc6ff0e3d70f81a_0.geojson') %>%
  st_transform(st_crs(miami.base.sf)) %>%
  st_as_sf() %>%
  st_crop(c(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))

#----OSM data pulls

bars <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("bar", "pub", "biergarten")) %>%
  osmdata_sf()
bars <- 
  bars$osm_points %>%
  .[miami.base,] 

restaurants <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("cafe", "restaurant", "fast_food")) %>%
  osmdata_sf()
restaurants <- 
  restaurants$osm_points %>%
  .[miami.base,]

libraries <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("library")) %>%
  osmdata_sf()
libraries <- 
  libraries$osm_points %>%
  .[miami.base,]

parking <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("parking")) %>%
  osmdata_sf()
parking <- 
  parking$osm_points %>%
  .[miami.base,]

entertainment <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>%
  add_osm_feature(key = 'amenity', value = c("arts_centre", "cinema", "theatre")) %>%
  osmdata_sf()
entertainment <-
  entertainment$osm_points %>%
  .[miami.base,]

ggplot() +
  geom_sf(data = miami.base) +
  geom_sf(data = entertainment, fill = "lightblue")

ggplot() + 
  geom_sf(data=miami.base, fill="grey") +
  geom_sf(data=bars, colour="red", size=.75) +
  geom_sf(data=cafes, colour="blue", size=.75) +
  geom_sf(data=libraries, colour="orange", size=1) +
  geom_sf(data=parking, colour="purple", size=.5)

#####################
# FEATURE ENGINEERING
#####################
#-----Buffer: Count within buffer of each house
#Convert OSM data into an sf object with same projection
bars.sf <-
  bars %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf))%>%
  distinct()

schools.sf <-
  schools %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.base.sf))%>%
  distinct()

#Adding column to main dataset indicating the count of X within the buffer area
miami.sf$bars.buffer_660 = #I think it would be good practice to list the number of feet in the column name
  st_buffer(miami.sf, 660) %>%
  aggregate(mutate(bars.sf, counter = 1),., sum) %>%
  pull(counter) #might need to change the NA values to 0

miami.sf$bars.buffer_2640 =
  st_buffer(miami.sf, 2640) %>%
  aggregate(mutate(bars.sf, counter = 1),., sum) %>%
  pull(counter) #might need to change the NA values to 0

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

#---Distance to beach
miami.sf %>% mutate(beachDistance = st_distance(miami.sf, beaches.sf))

plot(st_distance(miami.sf, beaches.sf))

list(st_distance(miami.sf, beaches.sf))

?st_nearest_points.sf

mutate(
  beachDist = st_Near(train, beach))

?st_distance

#------Regression Variables
#Libraries
libraries.sf <-
  libraries %>%
  dplyr::select(geometry) %>%
  na.omit() %>%
  st_transform(st_crs(miami.sf)) %>%
  distinct()
miami.sf$libraries.buffer_2640 =
  st_buffer(miami.sf, 2640) %>%
  aggregate(mutate(libraries.sf, counter = 1),., sum) %>%
  pull(counter)

miami.sf <- miami.sf %>% mutate(Age = 2020 - YearBuilt)

miami.sf <- miami.sf %>% mutate(DistBeach = st_distance(miami.sf, beaches.sf)) %>% drop_units()


######################
# EXPLORATORY ANALYSIS
######################
#Code for corr plots
st_drop_geometry(miami.sf) %>% 
  mutate(Age = 2020 - YearBuilt) %>% #calculating the age of the home
  mutate(BR_4more = Bed > 4) %>% #categorical variables for the number of bedrooms in a house.
  mutate(BR_4 = Bed == 4) %>%
  # mutate(BR_3 = Bed == 3) %>%
  # mutate(BR_2 = Bed == 2) %>%
  # mutate(BR_1 = Bed == 1) %>%
  dplyr::select(SalePrice, AdjustedSqFt, LotSize,
                Bed, Bath, YearBuilt, Age, BR_4more, 
                bars.buffer_660, bars.buffer_2640) %>% #Choose variables from main dataset
  gather(Variable, Value, -SalePrice) %>% #convert to long format
  ggplot(aes(Value, SalePrice)) + #plot
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

colnames(miami.sf)

#Corr matrix
numericVars <- 
  select_if(st_drop_geometry(miami.sf), is.numeric) %>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 
#This is messy - could drop some variables to make this clearer.

#####################
# REGRESSION WORKFLOW
#####################
#Subset miami dataframe into all homes with listed sale prices
sales <- subset(miami.sf, SalePrice > 0)
head(sales)

#Setting up test and training datasets
inTrain <- createDataPartition( 
  y = paste(sales$SalePrice), 
  p = .60, list = FALSE)

miami.training <- sales[inTrain,] 
miami.test <- sales[-inTrain,]  

#Multivariate regression
reg1 <- lm(SalePrice ~ ., data = st_drop_geometry(miami.training) %>% 
             dplyr::select(SalePrice, AdjustedSqFt, LotSize,
                           Bed, Bath, YearBuilt))
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
                           savePredictions = FALSE)

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


######################
# Online JSON sources
######################
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
malls <- st_join(malls, muni, join = st_intersects, left = FALSE)

culture <- st_read('https://opendata.arcgis.com/datasets/70c48f0eb067448c8a787cfa1c1c3bb9_0.geojson') %>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
culture <- st_join(culture, muni, join = st_intersects, left = FALSE)

commercial <- st_read('https://opendata.arcgis.com/datasets/fb8303c577c24ea386a91be7329842be_0.geojson')%>%
  st_transform(st_crs(miami)) %>%
  st_as_sf() 
commercial <- st_join(commercial, muni, join = st_intersects, left = FALSE)

parks <- st_read('https://opendata.arcgis.com/datasets/0228d15b2f004758adfdbb4fd71bae10_0.geojson')%>%
  st_transform(st_crs(miami)) %>%
  st_as_sf()
parks <- st_join(parks, muni, join = st_intersects, left = FALSE)

rail <- st_read('https://opendata.arcgis.com/datasets/ee3e2c45427e4c85b751d8ad57dd7b16_0.geojson')%>%
  st_transform(st_crs(miami))%>%
  st_as_sf()
rail <- st_join(rail, muni, join = st_intersects, left = FALSE)

#Looking at types ofbusinesses
commercial_group <- commercial %>% count(BUSDESC)
commercial_group <- commercial_group[order(-commercial_group$n),]
commercial_group