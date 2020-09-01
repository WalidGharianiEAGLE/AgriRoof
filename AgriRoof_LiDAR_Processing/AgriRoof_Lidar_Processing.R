##########################################################################################################################################################################################################################
# Title         : AgriRoof: Potential Flat Rooftop Detection using LiDAR for Rooftop Farming
#
# Purpose       : This Script is written in the framework of AgriRoof LiDAR data processing workflow  
#
# Author        : Walid Ghariani (linkedin: https://www.linkedin.com/in/walid-ghariani-893365138/) (E-mail: walid.ghariani@stud-mail.uni-wuerzburg.de | walid11ghariani@gmail.com) 
#
# Input         : The LiDAR data utilized in our project has been downloaded from [Cologne municipality website](https://www.bezreg-koeln.nrw.de/brk_internet/geobasis/hoehenmodelle/3d-messdaten/index.html). 
#                 It contains 3D measurements and the citys surface data collected by aircraft-based laser scanning.
#
# Processing    : Generate the the DTM, Normalize the lidar data, Filter the point clouds from the first return, remove the outlines
#
# Output        : nDSM
##########################################################################################################################################################################################################################
library(lidR)
library(rLiDAR)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(rasterVis)
####################### I. AgriRoof LiDAR Processing Methodology:

############## ~~First Step:  Read the laz file
lidar<- lidR::readLAS("./AgriRoof_LiDAR_Processing/Input_LiDAR_Data/3dm_32_364_5622_1_nw.laz")

lidar

plot(lidar)
############## ~~Second Step:  Generate the the DTM
DTM = grid_terrain(lidar, algorithm = knnidw(k = 6L, p=2))

DTM

plot(DTM, main = "DTM at BOnn AOI")

gplot(DTM)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_fill_viridis_c()+
  coord_equal()+
  theme(text = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "DTM at Bonn AOI",
       x="Longitude", y="Latitude")
writeRaster(DTM, "./AgriRoof_LiDAR_Processing/Output/DTM.tif")

# Optional 1:
# Although the grid_terrain function and the implemented algorithm takes into account the points classified as "ground" (Classification = 2) to generate the DTM
# we can also use beforehand the classify_ground function to classify the point clouds into ground and non-ground

las <- lidR::readLAS("./AgriRoof_LiDAR_Processing/Input_LiDAR_Data/3dm_32_364_5622_1_nw.laz", select = "xyzrn")
plot(las)

# Using the Progressive Morphological Filter
# --------------------------------------

ws  <- seq(3,12, 3)
th  <- seq(0.1, 1.5, length.out = length(ws))

las <- classify_ground(las, pmf(ws, th))
plot(las, color = "Classification")

# # Using the Cloth Simulation Filter
# --------------------------------------
las2 <- lidR::readLAS("./AgriRoof_LiDAR_Processing/Input_LiDAR_Data/3dm_32_364_5622_1_nw.laz", select = "xyzrn")

# (Parameters chosen mainly for speed)
mycsf <- csf(TRUE, 1, 1, time_step = 1)
las2 <- classify_ground(las2, mycsf)
plot(las2, color = "Classification")

# Optional 2:
# Select the first returns classified as ground

# firstground = filter_poi(lidar, Classification == 2L & ReturnNumber == 1L)
# firstground
# plot(firstground)
############## ~~Third Step: Normalize the lidar data = Remove the topography from a point cloud
lidar <- normalize_height(lidar, DTM)

lidar

plot(lidar)

############## ~~Fourth Step: Filter the point clouds from the first return  classified as ground
lidar  = lasfilter(lidar, Classification != 2L & ReturnNumber == 1L)
lidar
plot(lidar)

############## ~~Fifth step : remove the outlines: we remove/filter some points that are considered as errors/outliers they usually have negative values ; and it can be seen when generating the nDSM
lidar <- filter_poi(lidar, Z >= 0)
lidar
plot(lidar)

writeLAS(lidar, "./AgriRoof_LiDAR_Processing/Output/lidar_filtered.las")


############## ~~Sixth Step : We generate the nDSM aka Height
nDSM <- grid_canopy(lidar, res = 1, p2r())
nDSM

plot(nDSM, main ="nDSM at Bonn AOI")

gplot(nDSM)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_fill_gradient(low="#FFFF80", high="#FF0000")+
  coord_equal()+
  theme(text = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "nDSM at Bonn AOI",
       x="Longitude", y="Latitude")
# the grey color represent the NA values when we filtered the LiDAR data

writeRaster(nDSM, "./AgriRoof_LiDAR_Processing/Output/nDSM_Bonn_Outlier_Gound_Removed.tif")


#################### II. Optional VIZ with rLiDAR Package: lets read and VIZ the filtered lidar data after the fifth step 
rLAS <- rLiDAR::readLAS("./AgriRoof_LiDAR_Processing/Output/lidar_filtered.las",short=TRUE)

summary(rLAS)

# Lest Viz the data on a 3D

# Define the color ramp
# color ramp
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

# Color by height
col <- myColorRamp(c("blue","green","yellow","red"),rLAS[,3])

# plot 2D
plot(rLAS[,1], rLAS[,2], col=col, xlab="UTM.Easting", ylab="UTM.Northing", main="Color by height")

# plot 3D
points3d(rLAS[,1:3], col=col, axes=FALSE, xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-"))                     # axes
grid3d(side=c('x+', 'y-', 'z'), col="gray")     # grid
title3d(xlab = "UTM.Easting", ylab = "UTM.Northing",zlab = "Height(m)", col="red") # title
planes3d(0, 0, -1, 0.001, col="gray",alpha=0.7) # terrain


####################### III. Experimental part: Estimation of the shape of the points neighborhood
# shp_plane
lidar1 <- segment_shapes(lidar, shp_plane(th1 = 25, th2 = 6, k = 8), "Coplanar")

plot(lidar1, color = "Coplanar")

# shp_hplane
lidar2 <- segment_shapes(lidar, shp_hplane(th1 = 25, th2 = 6, th3 = 0.98, k = 8), "Coplanar")

plot(lidar2, color = "Coplanar")

# shp_line 
lidar3 <- segment_shapes(lidar,shp_line(k =5), "Coplanar")

plot(lidar3, color="Coplanar")
