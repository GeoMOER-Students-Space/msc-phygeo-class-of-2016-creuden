# gi-ws-10 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# improved  analysis of trees and crowns
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden
# Find the data at: http://137.248.191.65:12921/ (need login)
#
# The script deals with an applied approach of treecrown segentation from chm raster data.
# The script provides four *examples* of tree crown segmentation based on different approaches ad provided by
# (3) SAGA GIS and (1) itcSegment package.  There are several more R packages 
#
# **NOTE** the SAGA command line parameters may change from version to version
#
# as well as at leat 5 OTB algorithms for this kind of segmentation.
# The segmentations need only the structural data derived by lidar 
# as raster data basically dsmFn  DSM, DEM, counts above ground, ground counts
# Returns are:  
# crown areas, 
# *synthetic* tree position (centroid) 
# some more indepentend tree estimations 
# trees/crowns metrics 
# vertical tree density
# horizontal tree density 
# 
# A calculate horizontal surface (vegetation) density
# B calculate a canopy height model (chm) and inverted chm (iChm)
# C optional filter iChm
# D calculate crown segementation and derive tree 
# the following 4 algorithms are available Note they can be parametrized heavily 
# D.1 deterministic 8 based flow network analysis
# D.2 watershed segmetnation analysis
# D.3 Seeded Region Growing 
# D.4 itcIMG
#
# NOTE: you must source the functions caMetrics.R and classifyTreeCrown.R
#
#--------- setup the environment ---------------------------------------------
#-----------------------------------------------------------------------------
#
# load package for linking  GI tools
require(link2GI)
#
# define project folder
filepath_base <- "~/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"
#
# define the actual course session
activeSession <- 11
#
# make manually a list of relevant functions in the corresponding function folder
funList <- c(paste0(filepath_base,"fun/","calcTextures.R"),  
             paste0(filepath_base,"fun/","createMocFolders.R"),
             paste0(filepath_base,"fun/","getSessionPathes.R"),
             paste0(filepath_base,"fun/","setPathGlobals.R" ),
             paste0(filepath_base,"fun/","caMetrics.R" ),
             paste0(filepath_base,"fun/","basicExtraction.R" ),
             paste0(filepath_base,"fun/","classifyTreeCrown.R")) 
# source functions
res <- sapply(funList, FUN = source)
#
# if at a new location create filestructure
createMocFolders(filepath_base)
# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "gi")
# set working directory
setwd(pd_gi_run)
#
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
# define EPSG code
EPSG <- 25832
#
#--------- initialize the external GIS packages --------------------------------
# check GDAL binaries and start gdalUtils
gdal <- link2GI::linkgdalUtils()
# setup SAGA
link2GI::linkSAGA()
#  setup GRASS7
link2GI::linkGRASS7(demR)
#
#--------- START of the thematic stuff ---------------------------------------
#
#
#-------- segmentation strategy -----------------------------------------------
# segType = 1: treecrown segmentation using channel network and drainage basins (SAGA ta_channels 5)
#              except the common params minTreeAlt,thtreeNodes, crownMinArea, crownMaxArea
#              no other parameters are used  
# segType = 2 treecrown segmentation using watershed segementation (SAGA imagery_segmentation 0)
#             additionally to the common params minTreeAlt,thtreeNodes, crownMinArea, crownMaxArea
#             the is0_putput, is0_join and is0_thresh parameters are used 
#             NOTE they will have an high impact on the results
# segType = 3 treecrown segmentation using growing segementation (SAGA imagery_segmentation 3)
#             additionally to the common params minTreeAlt,thtreeNodes, crownMinArea, crownMaxArea
#             all is3_ parameters are used 
#             NOTE they will have an high impact on the results
# segType = 4 treecrown segmentation (ITC delineation approach) finds local maxima (mean filtered)
#             within an imagery and uses a decision tree method to grow individual crowns
#             all itc_ params are used
#
segType       <- 2
#
# ------- gauss filter -------------------------------------------------------
gauss         <- FALSE # if TRUE chm filtering will be applied as first step
gsigma        <- 1.0   # sigma for Gaussian filtering of the CHM data
gradius       <- 3     # radius of Gaussian filter
#
# ---------- set tree thresholds ---------------------------------------------
#
#
minTreeAlt    <- 3   # -thresholdfor minimum tree altitude in meter
thtreeNodes   <- 6   # minimum number of ldd connections
# sqm crowns
crownMinArea  <- 5   #(approx 1.25 m diameter)
crownMaxArea  <- 150 #(approx 17.8 m diameter)
#
# --------  postclassification thresholds----------------
WLRatio       <- 0.5 # crown width length ratio
thLongit      <- 0.5 # crown longitudiness 
solidity      <- 1.0 # solidity 
#
#----------- segementation thresholds -------------------
# --- watershed segementation (imagery_segmentation 0) for segType=2
is0_output    <- 0   # 0= seed value 1=segment id
is0_join      <- 1     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
is0_thresh    <- 5.5 # threshold for join difference in m
#
# --- growing segementation (imagery_segmentation 3) for segType=3
is3_leafsize  <- 8
is3_normalize <- 1
is3_neighbour <- 1
is3_method    <- 0
is3_sig1      <- 0.100000
is3_sig2      <- 0.100000
is3_threshold <- 0.000000
#
#
# --- itcSegement for segType=4
itc_seed      <- 0.45
itc_crown     <- 0.55
#
#
#--------- start core script     ---------------------------------------------
#-----------------------------------------------------------------------------
#
# ------ calculate horizontal "Forest" density (hFdensity) --------------------
#
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
#
# ----- calculate and convert canopy height model (chm) -----------------------
chmR <-  dsmR - demR
chmR[chmR < -minTreeAlt] <- minTreeAlt
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
#
# ------ calculate the INVERSE chm --------------------------------------------
invChmR <-  demR - dsmR
invChmR[invChmR > -minTreeAlt] <- minTreeAlt
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
#
# ----------  segType = 1 -----------------------------------------------------
if (segType == 1) {
  ### now there are some alternative algorithm to calculate the tree/treecrown identification
  ### (D.1) (SAGA) create watershed crowns segmentation using ta_channels 5
  #                 trees are assummed to be connection nodes with a specific number of links of the ldd
  ret <- system(paste0(sagaCmd, " ta_channels 5 ",
                       " -DEM ",pd_gi_run,"iChm.sgrd",            # inverse chm
                       " -BASINS ",pd_gi_run,"crownsHeight.shp",     # assumed to be crowns
                       " -SEGMENTS ",pd_gi_run,"ldd.shp",         # ldd
                       " -CONNECTION ",pd_gi_run,"rawTrees.sgrd", # all nodes will be calculated 
                       " -THRESHOLD 1"),intern = TRUE)            # all levels (max = 8) will be regarded
  # 
  #  make crown vector data set
  trees_crowns_1 <- classifyTreeCrown(crownFn = paste0(pd_gi_run,"crownsHeight.shp"),segType = 1, 
                                      funNames = c("eccentricityboundingbox","solidity"),
                                      minTreeAlt = minTreeAlt, 
                                      crownMinArea = crownMinArea, 
                                      crownMaxArea = crownMaxArea, 
                                      solidity = solidity, 
                                      WLRatio = WLRatio)
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
  #
  # ----------  segType = 2 -----------------------------------------------------
  #
} else if (segType == 2) {
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
  #
  #  make crown vector data set
  trees_crowns_2 <- classifyTreeCrown(crownFn = paste0(pd_gi_run,"crownsHeight.shp"),segType = 2, 
                                      funNames = c("eccentricityboundingbox","solidity"),
                                      minTreeAlt = minTreeAlt, 
                                      crownMinArea = crownMinArea, 
                                      crownMaxArea = crownMaxArea, 
                                      solidity = solidity, 
                                      WLRatio = WLRatio)
  pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "NAME")
  # ----------------------
  #
  # in addition we derive alternatively trees from the initial seedings 
  # read from the analysis (imagery_segmentation 0)
  trees <- rgdal::readOGR(pd_gi_run,"rawTrees")
  trees <- trees[trees$VALUE > minTreeAlt ,] 
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
  sTr <- sTr[sTr$V3 != "-Inf",] 
  sTr <- sTr[sTr$V3 >= minTreeAlt ,]
  sp::coordinates(sTr) <- ~V1+V2
  colnames(sTr@data)<-"crownsHeigh"
  sp::proj4string(sTr) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # save as shapefile
  rgdal::writeOGR(obj = sTr, 
                  layer = "sTr", 
                  driver = "ESRI Shapefile", 
                  dsn = pd_gi_run, 
                  overwrite_layer = TRUE)
  raster::plot(trees_crowns_2[[1]])
  raster::plot(trees_crowns_2[[2]])
  raster::plot(sTr)
  #
  # ----------  segType = 3 -----------------------------------------------------
  # 
} else if (segType == 3) {
  #
  system(paste0(sagaCmd," shapes_grid ", 9 ,
                " -GRID ","chm.sgrd",
                " -MAXIMA ",path_run,"maxChm.shp"))
  # saga_cmd imagery_segmentation 3 -SEEDS= -FEATURES=iChm.sgrd;hFdensity.sgrd; -SEGMENTS= -SIMILARITY= -TABLE= -NORMALIZE -NEIGHBOUR=1 -METHOD=0 -SIG_1=0.100000 -SIG_2=0.100000 -THRESHOLD=0.000000 -LEAFSIZE=8
  system(paste0(sagaCmd, "imagery_segmentation 3 ",
                " -SEEDS     "   ,pd_gi_run,"maxChm.shp",
                " -FEATURES  "   ,pd_gi_run,"chm.sgrd",";",pd_gi_run,"hFdensity.sgrd",
                " -SEGMENTS  "   ,pd_gi_run,"segment.sgrd",
                " -LEAFSIZE  "   ,is3_leafsize,
                " -NORMALIZE ",is3_normalize,
                " -NEIGHBOUR ",is3_neighbour, 
                " -METHOD    ",is3_method,
                " -SIG_1     ",is3_sig1,
                " -SIG_2     ",is3_sig2,
                " -THRESHOLD ",is3_threshold))
  #  
  # ----------  segType = 2 -----------------------------------------------------
  #  
} else if (segType == 4) {
  require(itcSegment)
  itcSeg <- itcSegment::itcIMG(imagery = demR,
                               epsg=EPSG,
                               TRESHSeed = itc_seed,
                               TRESHCrown = itc_crown,
                               th = minTreeAlt,
                               ischm = TRUE,
                               DIST = sqrt(crownMaxArea/pi)*2)
}

# view it
mapview::mapview(sTr,cex = 2,alpha.regions = 0.3,lwd = 1) 


