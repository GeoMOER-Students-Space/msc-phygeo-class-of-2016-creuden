setupDir(projRootDir="~/proj/linkGI", projSubFolders=c("data/","result/","run/","log/"))

# define the used input file(s)
demFn <- "geonode-lidar_dem_01m.tif"
dsmFn <- "geonode-lidar_dsm_01m.tif"

# set working directory
setwd(path_run)


# check GDAL binaries and start gdalUtils

gdal <- initgdalUtils()
initSAGA()

demR <- raster::raster(paste0(path_data,demFn))
dsmR <- raster::raster(paste0(path_data,dsmFn))
initGRASS(demR)
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

invChmR <- chmRf + raster::minValue(chmRf) * -1

invChmR[invChmR < treeth] <- treeth
raster::writeRaster(invChmR,paste0(path_run,"iChm.tif"),overwrite = TRUE)

gdalUtils::gdalwarp(paste0(path_run,"iChm.tif"),paste0(path_run,"rt_iChm.sdat"), overwrite=TRUE,  of='SAGA') 


system(paste0(sagaCmd,' shapes_grid ', 9 ,
                      ' -GRID ','rt_iChm.sgrd',
                      ' -MINIMA ',path_run,'min.shp',
                      ' -MAXIMA ',path_run,'mp_max.shp'))
min <- rgdal::readOGR(path_run,'min')

# # calculate wood's terrain indices   wood= 1=planar,2=pit,3=channel,4=pass,5=ridge,6=peak
# system(paste0(sagaCmd,' ta_morphometry 23 ',
#                       ' -DEM ',path_run,'rt_iChm.sgrd',
#                       ' -FEATURES ',path_run,'rt_wood.sgrd',
#                       ' -SLOPE ',path_run,'rt_slope.sgrd',
#                       ' -LONGC ',path_run,'rt_longcurv.sgrd',
#                       ' -CROSC ',path_run,'rt_crosscurv.sgrd',
#                       ' -MINIC ',path_run,'rt_mincurv.sgrd',
#                       ' -MAXIC ',path_run,'rt_maxcurv.sgrd',
#                       ' -SIZE ',wsize,
#                       ' -TOL_SLOPE ',tol.slope,
#                       ' -TOL_CURVE ',tol.curve,
#                       ' -EXPONENT ',exponent,
#                       ' -ZSCALE ',zscale))

# # (GDAL) convert the TIF to SAGA format
# gdalUtils::gdalwarp(paste0(path_run,"rt_wood.sdat"),paste0(path_run,"rt_wood.tif") , overwrite=TRUE)  
# 
# # (R) assign to raster
# wood<-raster::raster(paste0(path_run,"rt_wood.tif"))
# 
# # reclassify from all landforms to flat only
# pit<-raster::reclassify(wood, c(0,2,0, 2,3,1,3,256,0 ))
# raster::plot(pit)
# raster::writeRaster(pit,paste0(path_run,"pit.tif"),overwrite=TRUE)
# summary(raster::values(pit))
# # (SAGA) create catchment area
# system(paste0(sagaCmd," garden_learn_to_program 7 ",
#               " -ELEVATION ",paste0(path_run,"rt_dempitless.sgrd"),
#               " -AREA ",paste0(path_run,"rt_catchmentarea.sgrd"),
#               " -METHOD 0"))
# 
# # (gdalUtils) export it to  R as an raster object
# gdalUtils::gdalwarp(paste0(path_run,"rt_catchmentarea.sdat"),
#                     paste0(path_run,"rt_catchmentarea.tif") , 
#                     overwrite=TRUE) 
# # (R) 
# catchmentarea<-raster::raster(paste0(path_run,"rt_catchmentarea.tif"))

# # the gauge position is not very accurate- a straightforward buffering approach may help to find the correct outlet/gauge position
# # (R) buffer the gauge point for finding the  maximum  catchment value within 25 m radius
# gaugeBuffer <- as.data.frame(raster::extract(catchmentarea, estGauge, buffer = 25, cellnumbers = T)[[1]])
# 
# # (R) get the id of maxpos
# id <- gaugeBuffer$cell[which.max(gaugeBuffer$value)]
# 
# # (R) get the posistion that is estimated to be the gauge
# gaugeLoc <- raster::xyFromCell(dem, id)
#CONNECTION = NULL -ORDER = NULL -BASIN=NULL -SEGMENTS= -BASINS=/home/creu/lehre/msc/active/msc-2016/data/gis/run/crowns.shp -NODES=NULL -THRESHOLD=1
system(paste0(sagaCmd, " ta_channels 5 ",
                        " -DEM ",path_run,"rt_iChm.sgrd",
                        " -BASIN ",path_run,"rt_crown.shp",
                        " -BASINS ",path_run,"crowns.shp",
                        " -SEGMENTS ",path_run,"crowns.shp",
                        " -CONNECTION ",path_run,"trees.sgrd",
                        " -THRESHOLD 1"))

rgrass7::execGRASS('r.import',  
                   flags=c('o',"overwrite","quiet"),
                   input=paste0(path_run,"trees.sdat"),
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
                   output=paste0(path_run,"trees.shp"),
                   format="ESRI_Shapefile")
treesR <- rgdal::readOGR(path_run,'trees')
treesR[treesR < treeOrder]<-NA
treesR2<-treesR[complete.cases(treesR@data$noNode),]

# system(paste0(sagaCmd," garden_learn_to_program 7 ",
#               " -ELEVATION ",paste0(path_run,"rt_iChm.sgrd"),
#               " -AREA ",paste0(path_run,"rt_crownarea.sgrd"),
#               " -METHOD 0"))
# 
# (SAGA) calculate upslope area
crowns <- lapply(seq(1:length(min)),function(x){
  system(paste0(sagaCmd," ta_hydrology 4 ",
                " -TARGET_PT_X ",min$X[x],
                " -TARGET_PT_Y ",min$Y[x],
                " -ELEVATION ",path_run,"rt_iChm.sgrd",
                " -AREA ",path_run,"rt_tree_",x,".sgrd",
                " -METHOD 0", 
                " -CONVERGE=1.100000"))
  
})

# (gdalUtils) export it to  R as an raster object
gdalUtils::gdalwarp(paste0(path_run,"rt_tree_",x,".sdat"),
                    paste0(path_run,"rt_tree_",x,".tif") , 
                    overwrite=TRUE) 



# (R) 
upslope<-raster::raster(paste0(path_run,"rt_catch.tif"))
ws<-raster::raster(paste0(path_run,"rt_ws.tif"))

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
