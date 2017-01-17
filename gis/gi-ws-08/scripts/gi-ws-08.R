# gi-ws-09 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# straightforward analysis of trees and crowns
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden

#--------- setup the environment ----------------------------------------------

# define project folder
filepath_base<-"~/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"

# define the actual course session
activeSession<-8

# define the used input file(s)
dsmFn <- "cut_geonode-lidar_dsm_01m.tif"
demFn <- "cut_geonode-lidar_dem_01m.tif"

# make a list of all functions in the corresponding function folder
sourceFileNames <- list.files(pattern="[.]R$", path=paste0(filepath_base,"fun"), full.names=TRUE)

# source all functions
res <- sapply(sourceFileNames, FUN=source)

# if at a new location create filestructure
createMocFolders(filepath_base)

# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "gi")

# set working directory
setwd(pd_gi_run)

#--------- initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal <- initgdalUtils()

# setup SAGA
initSAGA()

# (R) read the input file(s) into a R raster
demR <- raster::raster(paste0(pd_gi_input,demFn))
dsmR <- raster::raster(paste0(pd_gi_input,dsmFn))

# (R) setup GRASS7
initGrass4R(demR)

#--------- START of the thematic stuff ---------------------------------------
# 1) calculate a canopy height model (chm)
# 2) invert it for a watershed analysis
# 3) smooth it for better crown surfaces
# 4) apply watershed analysis

#--------- set vars ----------------------------------------------------------
# tree-threshold altitude in meter
tthrees <- 5 

# strahler order threshold for treetop
thStrahler <- 4

# Gauss params
gsigma <- 1.000000
gradius <- 3
#--------- start core script     ---------------------------------------------
# (R) calculate canopy height model (chm)
chmR <- dsmR - demR 

# (R) invert chm and make positive altitudes
invChmR<-chmR + minValue(chmR)*-1

# (R) apply minimum tree heihgt
invChmR[invChmR < tthrees] <- tthrees

# (R) export to TIF
writeRaster(invChmR,paste0(pd_gi_run,"iChm.tif"),overwrite=TRUE)

# (GDAL) convert the TIF to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_run,"iChm.tif"),paste0(pd_gi_run,"rt_iChm.sdat"), overwrite=TRUE,  of='SAGA') 

# (SAGA) apply a gaussian filter (more effective than mean)
system(paste0(sagaCmd,' grid_filter 1 ',
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
 
# convert raster to sp object
treesWsh <- ras2vecpoiGRASS(paste0(pd_gi_run,"rt_treeNodes.sdat"),retSP=TRUE) 
 
# filter trees according to Strahler order number
tws<-treesWsh[treesWsh@data$noNode > thStrahler,]

# export them to shape
rgdal::writeOGR(obj = tws,".","tws",driver="ESRI Shapefile")

# read tree crown watersheds 
crownarea <- rgdal::readOGR(pd_gi_run,"rt_crown")

# view it 
  
mapview::mapview(tws,zcol="noNode",cex=2,alpha.regions = 0.3,lwd=1) +
mapview::mapview(minZ,zcol="Z",cex=1,alpha.regions = 0.1,lwd=1)


