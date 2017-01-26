# gi-ws-09 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# improved  analysis of trees and crowns
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden

#--------- setup the environment ----------------------------------------------
library(link2GI)

# define project folder
filepath_base <- "~/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"

# define the actual course session
activeSession <- 9

# define the used input file(s)
dsmFn <- "cut_geonode-lidar_dsm_01m.tif"
demFn <- "cut_geonode-lidar_dem_01m.tif"

# make a list of relevant functions in the corresponding function folder
funList <- c(paste0(filepath_base,"fun/","calcTextures.R"),  
             paste0(filepath_base,"fun/","createMocFolders.R"),
             paste0(filepath_base,"fun/", "ffs.R"),             
             paste0(filepath_base,"fun/","getSessionPathes.R"),
             paste0(filepath_base,"fun/","gheatMap.R" ),       
             paste0(filepath_base,"fun/","gPointClust.R"),    
             paste0(filepath_base,"fun/","ras2vecpoiGRASS.R" ),
             paste0(filepath_base,"fun/","setPathGlobals.R" )) 

# source relevant functions
res <- sapply(funList, FUN = source)

# if at a new location create filestructure
createMocFolders(filepath_base)

# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "gi")

# set working directory
setwd(pd_gi_run)

#--------- initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal <- link2GI::linkgdalUtils()

# setup SAGA
link2GI::linkSAGA()

# (R) read the input file(s) into a R raster
demR <- raster::raster(paste0(pd_gi_input,demFn))
dsmR <- raster::raster(paste0(pd_gi_input,dsmFn))

# (R) setup GRASS7
link2GI::linkGRASS7(demR)

#--------- START of the thematic stuff ---------------------------------------
# 1) calculate a canopy height model (chm)
# 2) invert it for a watershed analysis
# 3) optionally smooth it for better crown surfaces
# 4) apply watershed analysis
# 5) apply an more sophisticated algorithm for tree top identification
# 6) calculate some metrics of the crown/tree objects
# 7) filter and reclassify the derived crown areas

#--------- set vars ----------------------------------------------------------
#### preclassification thresholds
#    min tree altitude
#    min Strahler order
#    min area of crowns
#    Gaussian filter sigma value
#    Gaussian filter radius
#    Switch for Gaussian filter

# tree-threshold altitude in meter
thChmAltitude <- 5
# strahler order threshold if > then secondary treetop
thStrahler <- 4
# sqm crownarea
thCrownArea <- 9
# sigma for Gaussian filtering of the CHM data
gsigma <- 1.000000
# radius of Gaussian filter
gradius <- 3
# switch for filtering
gauss <- FALSE

### postclassification thresholds
#   ratio of width and length of the croen shape
#   ratio of the longitudinal bias of the shape

# crown width length ratio
thWidthLengthRatio <- 0.5
# crown longitudiness 
thLongitudines <- 0.5

#--------- start core script     ---------------------------------------------
# (R) calculate canopy height model (chm)
chmR <- dsmR - demR 
# (R) invert chm and make positive altitudes
invChmR <- chmR + raster::minValue(chmR)*-1
# (R) apply minimum tree heihgt
invChmR[invChmR < thChmAltitude] <- thChmAltitude

# (R) export to TIF
raster::writeRaster(invChmR,paste0(pd_gi_run,"iChm.tif"),overwrite=TRUE)
# (GDAL) convert the TIF to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_run,"iChm.tif"), 
                    paste0(pd_gi_run,"rt_iChm.sdat"), 
                    overwrite=TRUE,  of='SAGA') 

# (SAGA) apply a gaussian filter (better for keeping thetops AND smoothing than mean)
if (gauss)  system(paste0(sagaCmd,' grid_filter 1 ',
              ' -INPUT ',pd_gi_run,"rt_iChm.sdat",
              ' -RESULT ',pd_gi_run,"rt_iChmGF.sgrd",
              ' -SIGMA ',gsigma,
              ' -MODE 1',
              ' -RADIUS ',gradius))


#------  optional to get an idea how much lokal minima exist
# (SAGA) calculate min max values for control purposes
system(paste0(sagaCmd,' shapes_grid ', 9 ,
                      ' -GRID ','rt_iChmGF.sgrd',
                      ' -MINIMA ',pd_gi_run,'min.shp',
                      ' -MAXIMA ',pd_gi_run,'mp_max.shp'))
# (R) convert to sp object
minZ <- rgdal::readOGR(pd_gi_run,'min')
#---------------------------------


# (SAGA) create watershed crowns segmentation using ta_channels 5
# generates also the nodes of the Strahler network
system(paste0(sagaCmd, " ta_channels 5 ",
                        " -DEM ",pd_gi_run,"rt_iChmGF.sgrd",
                        " -BASIN ",pd_gi_run,"rt_crown.sgrd",
                        " -BASINS ",pd_gi_run,"rt_crowns.shp",
                        " -SEGMENTS ",pd_gi_run,"rt_segs.shp",
                        " -CONNECTION ",pd_gi_run,"rt_treeNodes.sgrd",
                        " -THRESHOLD 1"))
 
# ---------- alternative calculation 
 # # (SAGA) create watershed crowns segmentation using imagery_segmentation 0 (same results)
 # # creates everything in one run except the Strahler network
 # system(paste0(sagaCmd, " imagery_segmentation 0 ",
 #                        " -GRID ",pd_gi_run,"rt_iChmGF.sgrd",
 #                        " -SEGMENTS ",pd_gi_run,"rt_segsimagery.sgrd",
 #                        " -SEEDS ",pd_gi_run,"rt_segsimageryseeds.shp",
 #                        " -BORDERS ",pd_gi_run,"rt_segsborders",
 #                        " -OUTPUT 1", 
 #                        " -DOWN 0", 
 #                        " -JOIN 0 ",
 #                        " -THRESHOLD 0.000000", 
 #                        " -EDGE 1"))
 # # (SAGA) create watershed crowns segmentation using imagery_segmentation 0 (same results)
 # system(paste0(sagaCmd, " ta_channels 6 ",
 #                        " -DEM ",pd_gi_run,"rt_iChmGF.sgrd",
 #                        " -STRAHLER ",pd_gi_run,"rt_ichmstrahler.sgrd"))
 # treesR <- ras2vecpoiGRASS(paste0(pd_gi_run,"rt_ichmstrahler.sdat"),retRaster=TRUE) 
#------------------------------------------------------------------------------


# (gdalUtils) export it to  R as an raster object
 gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_treeNodes.sdat"),
                     paste0(pd_gi_run,"treeNodes.tif") , 
                     overwrite = TRUE) 
# (R) import potential trees
 rt_trees<-raster::raster( paste0(pd_gi_run,"treeNodes.tif") )

 # (R) filter them according the Strahler threshold
 rt_trees[rt_trees < thStrahler]<-NA
 
 # (R) export trees as TIF
 raster::writeRaster(rt_trees,paste0(pd_gi_run,"rt_trees.tif"),overwrite=TRUE)
 
# (GRASS) convert raster to sp object
treesWsh <- ras2vecpoiGRASS(paste0(pd_gi_run,"rt_trees.tif"),retSP=TRUE) 

# (R) export trees as shape file
rgdal::writeOGR(obj = treesWsh,".","treesWsh",driver="ESRI Shapefile", overwrite_layer = TRUE)

# (R) import crown areas from the SAGA analysis
crownarea <- rgdal::readOGR(pd_gi_run,"rt_crowns")

# (R) calculate area of each crown
crownarea@data$area <- raster::area(crownarea) 

# (R) filter by thCrownArea
crownarea <- crownarea[crownarea@data$area > thCrownArea,]

# (R) calculate some crown related metrics https://cran.r-project.org/web/packages/Momocs/Momocs.pdf
# https://www.researchgate.net/profile/Paul_Rosin/publication/228382248_Computing_global_shape_measures/links/0fcfd510802e598c31000000.pdf?origin=publication_detail
polys <- crownarea@polygons

crownarea@data$rectangularity <- as.character(lapply(seq(1:length(polys)),function(i){
  comp <- Momocs::coo_rectangularity(as.matrix(polys[[i]]@Polygons[[1]]@coords))
  return(unlist(comp))
}))

crownarea@data$circularityharalick <- as.character(lapply(seq(1:length(polys)),function(i){
  comp <- Momocs::coo_circularityharalick(as.matrix(polys[[i]]@Polygons[[1]]@coords))
  return(unlist(round(comp,2)))
}))

crownarea@data$convexity <- as.character(lapply(seq(1:length(polys)),function(i){
  comp <- Momocs::coo_convexity(as.matrix(polys[[i]]@Polygons[[1]]@coords))
  return(unlist(round(comp,2)))
}))
crownarea@data$solidity <- as.character(lapply(seq(1:length(polys)),function(i){
  comp <- Momocs::coo_solidity(as.matrix(polys[[i]]@Polygons[[1]]@coords))
  return(unlist(round(comp,2)))
}))
crownarea@data$wlratio  <- as.character(lapply(seq(1:length(polys)),function(i){
  comp <- Momocs::coo_eccentricityboundingbox(as.matrix(polys[[i]]@Polygons[[1]]@coords))
  return(unlist(round(comp,2)))
}))
crownarea@data$elongation <- as.character(lapply(seq(1:length(polys)),function(i){
  comp <- Momocs::coo_elongation(as.matrix(polys[[i]]@Polygons[[1]]@coords))
  return(unlist(round(comp,2)))
}))

# (R) postclassify by thLongitudines and thWidthLengthRatio 
crowns<-crownarea[as.numeric(crownarea@data$solidity) != 1 &  
                  as.numeric(crownarea@data$elongation) > -thLongitudines & 
                  as.numeric(crownarea@data$elongation) < thLongitudines & 
                  as.numeric(crownarea@data$wlratio) > thWidthLengthRatio ,]

# (R) map it 
  
mapview::mapview(treesWsh,cex=2,alpha.regions = 0.3,lwd=1) +
mapview::mapview(crowns,alpha.regions = 0.1,lwd=1)+
mapview::mapview(crownarea,alpha.regions = 0.1,lwd=1)

