# gi-ws-10 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# improved  analysis of trees and crowns
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden
# Find the used data at: http://137.248.191.65:12921/


#--------- setup the environment ---------------------------------------------
#-----------------------------------------------------------------------------

# load package for linking  GI tools
library(link2GI)

# define project folder
filepath_base <- "~/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"

# define the actual course session
activeSession <- 11

# make manually a list of relevant functions in the corresponding function folder
funList <- c(paste0(filepath_base,"fun/","calcTextures.R"),  
             paste0(filepath_base,"fun/","createMocFolders.R"),
             paste0(filepath_base,"fun/","getSessionPathes.R"),
             paste0(filepath_base,"fun/","setPathGlobals.R" ),
             paste0(filepath_base,"fun/","caMetrics.R" ),
             paste0(filepath_base,"fun/","classifyTreeCrown.R")) 
# source functions
res <- sapply(funList, FUN = source)

# if at a new location create filestructure
createMocFolders(filepath_base)
# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "gi")
# set working directory
setwd(pd_gi_run)

# define the used input file(s)
dsmFn  <- "geonode-lidar_dsm_01m.tif"  # surface model
demFn  <- "geonode-lidar_dem_01m.tif"  # elevation model
pcagFn <- "geonode-lidar_pcag_01m.tif" # counts above ground
pcgrFn <- "geonode-lidar_pcgr_01m.tif" # ground counts
rgbFn  <- "geonode-ortho_muf_1m.tif"   # rgb ortho image
#  read the input file(s) into a R raster
demR <- raster::raster(paste0(pd_gi_input,demFn))
dsmR <- raster::raster(paste0(pd_gi_input,dsmFn))
pcagR <- raster::raster(paste0(pd_gi_input,pcagFn))
pcgrR <- raster::raster(paste0(pd_gi_input,pcgrFn))
rgbR <- raster::raster(paste0(pd_gi_input,rgbFn))

#--------- initialize the external GIS packages --------------------------------
# check GDAL binaries and start gdalUtils
gdal <- link2GI::linkgdalUtils()
# setup SAGA
link2GI::linkSAGA()
#  setup GRASS7
link2GI::linkGRASS7(demR)

#--------- START of the thematic stuff ---------------------------------------
#-----------------------------------------------------------------------------
# crownarea  vector containing all crown area related data
# trees vector containing all tree related data
#
### preprocessing and parameter generation
# A calculate horizontal surface (vegetation) density
# B calculate a canopy height model (chm) and inverted chm (iChm)
# C optional filter iChm

# 
#   invert it for a watershed analysis
# 3) optionally smooth it for better crown surfaces
# 4) apply watershed analysis
# 5) apply an more sophisticated algorithm for tree top identification
# 6) calculate some metrics of the crown/tree objects
# 7) filter and reclassify the derived crown areas


#-------- segmentation strategy -----------------------------------------------
# treeAlg = 1: treecrown segmentation using channel network and drainage basins (ta_channels 5)
#              except the common params thChmAltitude,thtreeNodes, crownMinArea, crownMaxArea
#              no other parameters are used  
# treeAlg = 2 treecrown segmentation using watershed segementation (imagery_segmentation 0)
#             additionally to the common params thChmAltitude,thtreeNodes, crownMinArea, crownMaxArea
#             the is0_putput, is0_join and is0_thresh parameters are used 
#             NOTE they will have an high impact on the results  

treeAlg <- 2


# ------- gauss filter -------------------------------------------------------
# switch for a DEM filtering that will be applied to all algorithms
gauss <- False
# sigma for Gaussian filtering of the CHM data
gsigma <- 1.0
# radius of Gaussian filter
gradius <- 3

# ---------- set tree thresholds ---------------------------------------------

# tree-threshold altitude in meter
thChmAltitude <- 3
# strahler order threshold if > then secondary treetop
thtreeNodes <- 6
# sqm crownarea
crownMinArea <- 5
crownMaxArea <- 150

# postclassification thresholds
# crown width length ratio
thWidthLengthRatio <- 0.5
# crown longitudiness 
thLongitudines <- 0.5
# solidity 
solidity <- 1.0

#----------- segementation thresholds -------------------
# --- watershed segementation (imagery_segmentation 0) 
#     used for treeAlg=2
is0_output <- 0   # 0= seed value 1=segment id
is0_join <- 1     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
is0_thresh <- 2.5 # threshold for join difference in m


#--------- start core script     ---------------------------------------------
#-----------------------------------------------------------------------------

# ------ calculate horizontal "Forest" density (hFdensity) --------------------

# the ratio of the above ground points to the total points is from 0 to 1 where 
# 0.0 represents no canopy and 1.0 very dense canopy
pTot <- pcagR + pcgrR
hFdensity <- pcagR / pTot
raster::writeRaster(hFdensity,paste0(pd_gi_run,"hFdensity.tif"),overwrite = TRUE)
gdalUtils::gdalwarp(paste0(pd_gi_run,"hFdensity.tif"), 
                    paste0(pd_gi_run,"hFdensity.sdat"), 
                    overwrite = TRUE,  
                    of = 'SAGA',
                    verbose = FALSE) 

# ----- calculate and convert canopy height model (chm) -----------------------
chmR <-  dsmR - demR
chmR[chmR < -thChmAltitude] <- thChmAltitude
raster::writeRaster(chmR,paste0(pd_gi_run,"chm.tif"),
                    overwrite = TRUE)
if (gauss)  ret <- system(paste0(sagaCmd,' grid_filter 1 ',
                                 ' -INPUT ',pd_gi_run,"chm.sdat",
                                 ' -RESULT ',pd_gi_run,"chm.sgrd",
                                 ' -SIGMA ',gsigma,
                                 ' -MODE 1',
                                 ' -RADIUS ',gradius),intern = TRUE)
gdalUtils::gdalwarp(paste0(pd_gi_run,"chm.tif"), 
                    paste0(pd_gi_run,"chm.sdat"), 
                    overwrite = TRUE,  
                    of = 'SAGA',
                    verbose = FALSE) #  calculate and convert inverse canopy height model (iChm)

# ------ calculate the INVERSE chm --------------------------------------------
invChmR <-  demR - dsmR
invChmR[invChmR > -thChmAltitude] <- thChmAltitude
# apply a gaussian filter 
# Gauss is more effective in preserving the tree tops 
# AND smoothing the crown area
raster::writeRaster(invChmR,paste0(pd_gi_run,"iChm.tif"),
                    overwrite = TRUE)
if (gauss)  ret <- system(paste0(sagaCmd,' grid_filter 1 ',
                                 ' -INPUT ',pd_gi_run,"iChm.sdat",
                                 ' -RESULT ',pd_gi_run,"iChm.sgrd",
                                 ' -SIGMA ',gsigma,
                                 ' -MODE 1',
                                 ' -RADIUS ',gradius),intern = TRUE)
gdalUtils::gdalwarp(paste0(pd_gi_run,"iChm.tif"), 
                    paste0(pd_gi_run,"iChm.sdat"), 
                    overwrite = TRUE,  
                    of = 'SAGA',
                    verbose = FALSE) 

# ----------  treeAlg = 1 -----------------------------------------------------
if (treeAlg == 1) {
  ### now there are some alternative algorithm to calculate the tree/treecrown identification
  ### (D.1) (SAGA) create watershed crowns segmentation using ta_channels 5
  #                 trees are assummed to be connection nodes with a specific number of links of the ldd
  ret <- system(paste0(sagaCmd, " ta_channels 5 ",
                       " -DEM ",pd_gi_run,"iChm.sgrd",            # inverse chm
                       " -BASINS ",pd_gi_run,"crownsHeight.shp",     # assumed to be crowns
                       " -SEGMENTS ",pd_gi_run,"ldd.shp",         # ldd
                       " -CONNECTION ",pd_gi_run,"rawTrees.sgrd", # all nodes will be calculated 
                       " -THRESHOLD 1"),intern = TRUE)            # all levels (max = 8) will be regarded
 
  #  make crown vector data set
  crownarea <- classifyTreeCrown(crownFn = paste0(pd_gi_run,"crownsHeight.shp"),segType = 2, 
                                 funNames = c("eccentricityboundingbox","solidity"),
                                 thChmAltitude = thChmAltitude, 
                                 crownMinArea = crownMinArea, 
                                 crownMaxArea = crownMaxArea, 
                                 solidity = solidity, 
                                 thWidthLengthRatio = thWidthLengthRatio)
  # in addition we derive alternatively trees from the initial seedings 
  # read from the analysis (ta_channels 5)
  # (gdalUtils) export it to  R as an raster object
  gdalUtils::gdalwarp(paste0(pd_gi_run,"rawTrees.sdat"),
                      paste0(pd_gi_run,"rawTrees.tif") , 
                      overwrite = TRUE,verbose = FALSE) 
  gdalUtils::gdal_translate(paste0(pd_gi_run,"rawTrees.tif"),
                            paste0(pd_gi_run,"rawTrees.xyz") ,
                            of = "XYZ",
                            overwrite = TRUE,verbose = FALSE) 
  # read XYZ data and create tree vector file
  nTr <- data.frame(data.table::fread(paste0(pd_gi_run,"rawTrees.xyz")))
  nTr <- nTr[nTr$V3 > thtreeNodes ,] 
  
  sp::coordinates(nTr) <- ~V1+V2
  sp::proj4string(nTr) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  rgdal::writeOGR(obj = nTr, 
                  layer = "nTr", 
                  driver = "ESRI Shapefile", 
                  dsn = pd_gi_run, 
                  overwrite_layer = TRUE,verbose = FALSE)
  
   
  # ----------  treeAlg = 2 -----------------------------------------------------
  
} else if (treeAlg == 2) {
  ### (SAGA) create watershed crowns segmentation using imagery_segmentation 0 
  ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                       " -GRID ",pd_gi_run,"chm.sgrd",
                       " -SEGMENTS ",pd_gi_run,"crownsHeight.sgrd",
                       " -SEEDS ",pd_gi_run,"rawTrees.shp",
                       " -OUTPUT ",is0_output, 
                       " -DOWN 1", 
                       " -JOIN ",is0_join,
                       " -THRESHOLD ", is0_thresh, 
                       " -EDGE 1"),intern = TRUE)
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID ",pd_gi_run,"crownsHeight.sgrd",
                       " -POLYGONS ",pd_gi_run,"crownsHeight.shp",
                       " -CLASS_ALL 1",
                       " -CLASS_ID 1.000000",
                       " -SPLIT 1"),intern = TRUE)
  
  #  make crown vector data set
  crownarea <- classifyTreeCrown(crownFn = paste0(pd_gi_run,"crownsHeight.shp"),segType = 2, 
                                 funNames = c("eccentricityboundingbox","solidity"),
                                 thChmAltitude = thChmAltitude, 
                                 crownMinArea = crownMinArea, 
                                 crownMaxArea = crownMaxArea, 
                                 solidity = solidity, 
                                 thWidthLengthRatio = thWidthLengthRatio)
  # ----------------------
  
  # in addition we derive alternatively trees from the initial seedings 
  # read from the analysis (imagery_segmentation 0)
  trees <- rgdal::readOGR(pd_gi_run,"rawTrees")
  trees <- trees[trees$VALUE > thChmAltitude ,] 
  trees@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # converting them to raster due to the fact of speeding up the process
  # vector operations with some mio points/polys are cumbersome using sp objects in R
  # optionally you could use the sf package...
  rawTrees  <-  demR * 0.0
  maskCrown <-  demR * 0.0
  # rasterize is much to slow for big vec data 
  # so we do it the long run
  # raster::rasterize(crowns,mask=TRUE,rawCrowns)
  raster::writeRaster(rawTrees,paste0(pd_gi_run,"rawTrees.tif"),overwrite = TRUE)
  raster::writeRaster(maskCrown,paste0(pd_gi_run,"maskCrown.tif"),overwrite = TRUE)
  ret <- system(paste0("gdal_rasterize ",
                       pd_gi_run,"rawTrees.shp ", 
                       pd_gi_run,"rawTrees.tif",
                       " -l rawTrees",
                       " -a VALUE"),intern = TRUE)
  ret <- system(paste0("gdal_rasterize ",
                       pd_gi_run,"crowns.shp ", 
                       pd_gi_run,"maskCrown.tif",
                       " -l crowns",
                       " -burn 1"),intern = TRUE)
  rawTrees  <- raster::raster(paste0(pd_gi_run,"rawTrees.tif"))
  maskCrown <- raster::raster(paste0(pd_gi_run,"maskCrown.tif"))
  # now we reclassify the areas for latter operation
  maskCrown[maskCrown == 0] <- NA
  maskCrown[maskCrown == 1] <- 0
  # addition with NA and zero mask aout all na areas
  sTr <-  rawTrees + maskCrown
  sTr[sTr <= 0] <- NA
  # and reconvert it
  raster::writeRaster(sTr,paste0(pd_gi_run,"sTr.tif"),overwrite = TRUE)
  gdalUtils::gdal_translate(paste0(pd_gi_run,"sTr.tif"),
                            paste0(pd_gi_run,"sTr.xyz") ,
                            of = "XYZ",
                            overwrite = TRUE,
                            verbose = FALSE) 
  # make seedTree vector data
  sTr <- data.frame(data.table::fread(paste0(pd_gi_run,"sTr.xyz")))
  sTr <- sTr[sTr$V3 != -9999,] 
  sp::coordinates(sTr) <- ~V1+V2
  colnames(sTr)
  sp::proj4string(sTr) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # save as shapefile
  rgdal::writeOGR(obj = sTr, 
                  layer = "sTr", 
                  driver = "ESRI Shapefile", 
                  dsn = pd_gi_run, 
                  overwrite_layer = TRUE)
  
  
} else if (treeAlg == 3) {
  
  # saga_cmd imagery_segmentation 3 -SEEDS= -FEATURES=iChm.sgrd;hFdensity.sgrd; -SEGMENTS= -SIMILARITY= -TABLE= -NORMALIZE -NEIGHBOUR=1 -METHOD=0 -SIG_1=0.100000 -SIG_2=0.100000 -THRESHOLD=0.000000 -LEAFSIZE=8
  # system(paste0(sagaCmd, " grid_gridding 6 ",
  #                " -FEATURES ",pd_gi_run,"chm.sgrd;",pd_gi_run,"hFdensity.sgrd",
  #                " -SEGMENTS ",pd_gi_run,"segment.sgrd",
  #                " -LEAFSIZE 8 ",
  #                " -NORMALIZE 1 ",
  #                " -NEIGHBOUR 1 ", 
  #                " -METHOD 0",
  #                " -SIG_1 0.100000",
  #                " -SIG_2 0.100000",
  #                " -THRESHOLD 0.000000"))
}


mapview::mapview(sTr,cex = 2,alpha.regions = 0.3,lwd = 1) 


