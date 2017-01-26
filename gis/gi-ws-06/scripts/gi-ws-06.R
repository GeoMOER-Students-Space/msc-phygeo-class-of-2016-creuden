# gi-ws-06 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# calculate the basic watershed and catchment parameters and returns the catchment of a given gauge level
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden
######### setup the environment -----------------------------------------------
# define project folder

filepath_base<-"~/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"

# define the actual course session
activeSession<-6

# define the used input file(s)
inputFile<- "geonode-las_dtm_01m.tif"

# make a list of all functions in the corresponding function folder
sourceFileNames <- list.files(pattern="[.]R$", path=paste0(filepath_base,"fun"), full.names=TRUE)

# source all functions
res<- sapply(sourceFileNames, FUN=source)

# if at a new location create filestructure
createMocFolders(filepath_base)

# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "gi")

# set working directory
setwd(pd_gi_run)

######### initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal<- initgdalUtils()

initSAGA()

######### START of the thematic stuff ----------------------------------------

# 1) Preprocessing DEM 
#    -> Fill Sinks wang & Liu
#    <- filled DEM
#    <- local drainage network
#    <- watershed
#  2) calculate  catchments parallel approach
#   -> filled DEM
#   <- catchment area
#  3) identify opslope area (= catchment area) for a given position 
#     with respect to the bigger valley structure
#     - buffer posion for a rough estimate of gauge position
#  4) calculate upslope area

######## set vars ------------------------------------------------------------

# gauge position
lat <- 50.840860
lon <- 8.684456

######### start core script     -----------------------------------------------

# (R) assign the input file to a R raster
dem<-raster::raster(paste0(pd_gi_input,inputFile))

# (R) create an sp object of estimated gauge position
gauge <- data.frame(y = lat, x = lon)

# (R) turn into a spatial object
sp::coordinates(gauge) <- ~ x + y

# (R) assign the coordinate system (WGS84)
raster::projection(gauge) <- sp::CRS("+init=epsg:4326")

# (R) reproject it
estGauge <- sp::spTransform(gauge, sp::CRS("+init=epsg:25832"))



# (GDAL) convert the TIF to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_input,inputFile),paste0(pd_gi_run,"rt_dem.sdat"), overwrite=TRUE,  of='SAGA') 


# (SAGA) create filled DEM and watershed 
system(paste0(sagaCmd," ta_preprocessor 4 ",
                      " -ELEV ",pd_gi_run,"rt_dem.sgrd", 
                      " -FILLED ",pd_gi_run,"rt_dempitless.sgrd",
                      " -FDIR ",pd_gi_run,"rt_ldd.sgrd",
                      " -WSHED ",pd_gi_run,"rt_ws.sgrd", 
                      " -MINSLOPE=0.100000"))

# (SAGA) create catchment area
system(paste0(sagaCmd," garden_learn_to_program 7 ",
              " -ELEVATION ",paste0(pd_gi_run,"rt_dempitless.sgrd"),
              " -AREA ",paste0(pd_gi_run,"rt_catchmentarea.sgrd"),
              " -METHOD 0"))

# (gdalUtils) export it to  R as an raster object
gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_catchmentarea.sdat"),
                    paste0(pd_gi_run,"rt_catchmentarea.tif") , 
                    overwrite=TRUE) 
# (R) 
catchmentarea<-raster::raster(paste0(pd_gi_run,"rt_catchmentarea.tif"))

# the gauge position is not very accurate- a straightforward buffering approach may help to find the correct outlet/gauge position
# (R) buffer the gauge point for finding the  maximum  catchment value within 25 m radius
gaugeBuffer <- as.data.frame(raster::extract(catchmentarea, estGauge, buffer = 25, cellnumbers = T)[[1]])

# (R) get the id of maxpos
id <- gaugeBuffer$cell[which.max(gaugeBuffer$value)]

# (R) get the posistion that is estimated to be the gauge
gaugeLoc <- raster::xyFromCell(dem, id)


# (SAGA) calculate upslope area
system(paste0(sagaCmd," ta_hydrology 4 ",
                      " -TARGET_PT_X ",gaugeLoc[1,1],
                      " -TARGET_PT_Y ",gaugeLoc[1,2],
                      " -ELEVATION ",pd_gi_run,"rt_dempitless.sgrd",
                      " -AREA ",pd_gi_run,"rt_catch.sgrd",
                      " -METHOD 0", 
                      " -CONVERGE=1.100000"))

# (gdalUtils) export it to  R as an raster object
gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_catch.sdat"),
                    paste0(pd_gi_run,"rt_catch.tif") , 
                    overwrite=TRUE) 
gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_ws.sdat"),
                    paste0(pd_gi_run,"rt_ws.tif") , 
                    overwrite=TRUE) 

# (R) 
upslope<-raster::raster(paste0(pd_gi_run,"rt_catch.tif"))
ws<-raster::raster(paste0(pd_gi_run,"rt_ws.tif"))

# view it 
mapview::mapview(ws)+ upslope

