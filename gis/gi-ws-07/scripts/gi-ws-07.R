# gi-ws-07 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# calculate kinematic wave overland flow fora given level and different time steps

######### setup the environment -----------------------------------------------
# define project folder

filepath_base<-"~/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"

# define the actual course session
activeSession<-7

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
# 2) Calculate kinematic D8
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

# Crop DEM (2000m x 2000m square around the watershed at 477783 E, 5632176 N)
extent <- raster::extent(477783-1500, 477783+1500, 5632176-1500, 5632176+1500)
dem <- raster::crop(dem, extent)

raster::writeRaster(dem, file = paste0(pd_gi_run, "demcut.tif"), "GTiff", overwrite = TRUE)

# (GDAL) convert the TIF to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_run,"demcut.tif"),paste0(pd_gi_run,"rt_dem.sdat"), overwrite=TRUE,  of='SAGA') 


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

# gauge position
lat <- 50.840860
lon <- 8.684456

# (R) create an sp object of estimated gauge position
gauge <- data.frame(y = lat, x = lon, name="Pegel")

# (R) turn into a spatial object
sp::coordinates(gauge) <- ~ x + y

# (R) assign the coordinate system (WGS84)
raster::projection(gauge) <- sp::CRS("+init=epsg:4326")

# (R) reproject it
estGauge <- sp::spTransform(gauge, sp::CRS("+init=epsg:25832"))

# Save gauge
rgdal::writeOGR(estGauge, paste0(pd_gi_run, "out.shp"), "gauge", driver="ESRI Shapefile", overwrite = FALSE)


# Module Overland Flow - Kinematic Wave D8
system(paste0(sagaCmd,
              " sim_hydrology 1 ",
              " -DEM ", pd_gi_run,"rt_dempitless.sgrd",
              " -FLOW ", pd_gi_run, "flow_01.sgrd",
              " -GAUGES ", pd_gi_input, "out.shp",
              " -GAUGES_FLOW ", pd_gi_input, "runoff_48_01",
              " -TIME_SPAN 48.000000 ",
              " -TIME_STEP 0.100000 "))


# Module Overland Flow - Kinematic Wave D8
system(paste0(sagaCmd,
              " sim_hydrology 1 ",
              " -DEM ", pd_gi_run,"rt_dempitless.sgrd",
              " -FLOW ", pd_gi_run, "flow_05.sgrd",
              " -GAUGES ", pd_gi_input, "out.shp",
              " -GAUGES_FLOW ", pd_gi_input, "runoff_48_05",
              " -TIME_SPAN 48.000000 ",
              " -TIME_STEP 0.500000 "))


system(paste0(sagaCmd,
              " sim_hydrology 1 ",
              " -DEM ", pd_gi_run,"rt_dempitless.sgrd",
              " -FLOW ", pd_gi_run, "flow_10.sgrd",
              " -GAUGES ", pd_gi_input, "out.shp",
              " -GAUGES_FLOW ", pd_gi_input, "runoff_48_10",
              " -TIME_SPAN 48.000000 ",
              " -TIME_STEP 1.00000 "))

system(paste0(sagaCmd,
              " sim_hydrology 1 ",
              " -DEM ", pd_gi_run,"rt_dempitless.sgrd",
              " -FLOW ", pd_gi_run, "flow_10.sgrd",
              " -GAUGES ", pd_gi_input, "out.shp",
              " -GAUGES_FLOW ", pd_gi_input, "runoff_600_10",
              " -TIME_SPAN 600.000000 ",
              " -TIME_STEP 1.00000 "))


runoff_48_01 <- read.csv(paste0(pd_gi_input, "runoff_48_01"), sep = "\t")
runoff_48_05 <- read.csv(paste0(pd_gi_input, "runoff_48_05"), sep = "\t")
runoff_48_10 <- read.csv(paste0(pd_gi_input, "runoff_48_10"),sep = "\t")
runoff_600_10 <- read.csv(paste0(pd_gi_input, "runoff_600_10"),sep = "\t")

plot(runoff_600_10, type = "l",col="green",lty=1)
lines(runoff_24_01, type = "l",col="red",lty=3)
lines(runoff_24_05,type = "l",col="blue",lty=2)
lines(runoff_24_10, type = "l",col="orange",lty=1)
