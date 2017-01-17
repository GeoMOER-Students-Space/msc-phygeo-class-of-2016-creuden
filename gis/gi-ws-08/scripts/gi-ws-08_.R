# gi-ws-09 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# calculate the basic watershed and catchment parameters and returns the catchment of a given gauge level
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden
######### setup the environment -----------------------------------------------
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

######### initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal <- initgdalUtils()

initSAGA()

# (R) assign the input file to a R raster
demR <- raster::raster(paste0(pd_gi_input,demFn))
dsmR <- raster::raster(paste0(pd_gi_input,dsmFn))
initGrass4R(demR)
######### START of the thematic stuff ----------------------------------------

######## set vars ------------------------------------------------------------
ksize <- 3
treeth <- 1
treeOrder <- 2
wsize <- 3
tol.slope <- 0.500000
tol.curve <- 0.01
exponent <- 0.000000
zscale <- 1.000000
######### start core script     -----------------------------------------------

chmR <- dsmR - demR 
chmRf <- chmR
#chmRf<- raster::focal(chmR, w=matrix(1/(ksize*ksize)*1.0, nc=ksize, nr=ksize))

invChmR<-chmRf + minValue(chmRf)*-1

invChmR[invChmR < treeth]<-treeth
writeRaster(invChmR,paste0(pd_gi_run,"iChm.tif"),overwrite=TRUE)
# # (R) create an sp object of estimated gauge position
# gauge <- data.frame(y = lat, x = lon)
# 
# # (R) turn into a spatial object
# sp::coordinates(gauge) <- ~ x + y
# 
# # (R) assign the coordinate system (WGS84)
# raster::projection(gauge) <- sp::CRS("+init=epsg:4326")
# 
# # (R) reproject it
# estGauge <- sp::spTransform(gauge, sp::CRS("+init=epsg:25832"))
# 


# (GDAL) convert the TIF to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_run,"iChm.tif"),paste0(pd_gi_run,"rt_iChm.sdat"), overwrite=TRUE,  of='SAGA') 


system(paste0(sagaCmd,' shapes_grid ', 9 ,
                      ' -GRID ','rt_iChm.sgrd',
                      ' -MINIMA ',pd_gi_run,'min.shp',
                      ' -MAXIMA ',pd_gi_run,'mp_max.shp'))
min <- rgdal::readOGR(pd_gi_run,'min')

# # calculate wood's terrain indices   wood= 1=planar,2=pit,3=channel,4=pass,5=ridge,6=peak
# system(paste0(sagaCmd,' ta_morphometry 23 ',
#                       ' -DEM ',pd_gi_run,'rt_iChm.sgrd',
#                       ' -FEATURES ',pd_gi_run,'rt_wood.sgrd',
#                       ' -SLOPE ',pd_gi_run,'rt_slope.sgrd',
#                       ' -LONGC ',pd_gi_run,'rt_longcurv.sgrd',
#                       ' -CROSC ',pd_gi_run,'rt_crosscurv.sgrd',
#                       ' -MINIC ',pd_gi_run,'rt_mincurv.sgrd',
#                       ' -MAXIC ',pd_gi_run,'rt_maxcurv.sgrd',
#                       ' -SIZE ',wsize,
#                       ' -TOL_SLOPE ',tol.slope,
#                       ' -TOL_CURVE ',tol.curve,
#                       ' -EXPONENT ',exponent,
#                       ' -ZSCALE ',zscale))

# # (GDAL) convert the TIF to SAGA format
# gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_wood.sdat"),paste0(pd_gi_run,"rt_wood.tif") , overwrite=TRUE)  
# 
# # (R) assign to raster
# wood<-raster::raster(paste0(pd_gi_run,"rt_wood.tif"))
# 
# # reclassify from all landforms to flat only
# pit<-raster::reclassify(wood, c(0,2,0, 2,3,1,3,256,0 ))
# raster::plot(pit)
# raster::writeRaster(pit,paste0(pd_gi_run,"pit.tif"),overwrite=TRUE)
# summary(raster::values(pit))
# # (SAGA) create catchment area
# system(paste0(sagaCmd," garden_learn_to_program 7 ",
#               " -ELEVATION ",paste0(pd_gi_run,"rt_dempitless.sgrd"),
#               " -AREA ",paste0(pd_gi_run,"rt_catchmentarea.sgrd"),
#               " -METHOD 0"))
# 
# # (gdalUtils) export it to  R as an raster object
# gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_catchmentarea.sdat"),
#                     paste0(pd_gi_run,"rt_catchmentarea.tif") , 
#                     overwrite=TRUE) 
# # (R) 
# catchmentarea<-raster::raster(paste0(pd_gi_run,"rt_catchmentarea.tif"))

# # the gauge position is not very accurate- a straightforward buffering approach may help to find the correct outlet/gauge position
# # (R) buffer the gauge point for finding the  maximum  catchment value within 25 m radius
# gaugeBuffer <- as.data.frame(raster::extract(catchmentarea, estGauge, buffer = 25, cellnumbers = T)[[1]])
# 
# # (R) get the id of maxpos
# id <- gaugeBuffer$cell[which.max(gaugeBuffer$value)]
# 
# # (R) get the posistion that is estimated to be the gauge
# gaugeLoc <- raster::xyFromCell(dem, id)
CONNECTION=NULL -ORDER=NULL -BASIN=NULL -SEGMENTS= -BASINS=/home/creu/lehre/msc/active/msc-2016/data/gis/run/crowns.shp -NODES=NULL -THRESHOLD=1
system(paste0(sagaCmd, " ta_channels 5 ",
                        " -DEM ",pd_gi_run,"rt_iChm.sgrd",
                        " -BASIN ",pd_gi_run,"rt_crown.shp",
                        " -BASINS ",pd_gi_run,"crowns.shp",
                        " -SEGMENTS ",pd_gi_run,"crowns.shp",
                        " -CONNECTION ",pd_gi_run,"trees.sgrd",
                        " -THRESHOLD 1"))

rgrass7::execGRASS('r.import',  
                   flags=c('o',"overwrite","quiet"),
                   input=paste0(pd_gi_run,"trees.sdat"),
                   output="trees",
                   band=1
)

rgrass7::execGRASS('r.to.vect',  
                   flags=c('s',"overwrite","quiet"),
                   input="trees",
                   output="trees",
                   type="point",
                   column="noNode")

rgrass7::execGRASS('v.out.ogr',  
                   flags=c("overwrite","quiet"),
                   input="trees",
                   output=paste0(pd_gi_run,"trees.shp"),
                   format="ESRI_Shapefile")
treesR <- rgdal::readOGR(pd_gi_run,'trees')
treesR[treesR < treeOrder]<-NA
treesR2<-treesR[complete.cases(treesR@data$noNode),]

# system(paste0(sagaCmd," garden_learn_to_program 7 ",
#               " -ELEVATION ",paste0(pd_gi_run,"rt_iChm.sgrd"),
#               " -AREA ",paste0(pd_gi_run,"rt_crownarea.sgrd"),
#               " -METHOD 0"))
# 
# (SAGA) calculate upslope area
crowns <- lapply(seq(1:length(min)),function(x){
  system(paste0(sagaCmd," ta_hydrology 4 ",
                " -TARGET_PT_X ",min$X[x],
                " -TARGET_PT_Y ",min$Y[x],
                " -ELEVATION ",pd_gi_run,"rt_iChm.sgrd",
                " -AREA ",pd_gi_run,"rt_tree_",x,".sgrd",
                " -METHOD 0", 
                " -CONVERGE=1.100000"))
  
})

# (gdalUtils) export it to  R as an raster object
gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_tree_",x,".sdat"),
                    paste0(pd_gi_run,"rt_tree_",x,".tif") , 
                    overwrite=TRUE) 



# (R) 
upslope<-raster::raster(paste0(pd_gi_run,"rt_catch.tif"))
ws<-raster::raster(paste0(pd_gi_run,"rt_ws.tif"))

# view it 
mapview::mapview(ws)+ upslope




G2Tiff <- function (runDir=NULL,layer=NULL){
  
  rgrass7::execGRASS("r.out.gdal",
                     flags=c("c","overwrite","quiet"),
                     createopt="TFW=YES,COMPRESS=LZW",
                     input=layer,
                     output=paste0(runDir,"/",layer,".tif")
  )
}

GDAL2GRASS <- function (runDir=NULL,layer=NULL){
  rgrass7::execGRASS('r.import',  
                     flags=c('o',"overwrite","quiet"),
                     input=paste0(runDir,layer),
                     output=tools::file_path_sans_ext(layer),
                     band=1
  )
}

OGR2G <- function (runDir=NULL,layer=NULL){
  # import point locations to GRASS
  rgrass7::execGRASS('v.in.ogr',
                     flags=c('o',"overwrite","quiet"),
                     input=paste0(layer,".shp"),
                     output=layer
  )
}

G2OGR <- function (runDir=NULL,layer=NULL){
  rgrass7::execGRASS("v.out.ogr",
                     flags=c("overwrite","quiet"),
                     input=layer,
                     type="line",
                     output=paste0(layer,".shp")
  )
}
