
 # define project folder
filepath_base<-"~/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"
activeSession<-4
inputFile<- "geonode-las_dtm_01m.tif"

# make a list of all functions in the corresponding function folder
sourceFileNames <- list.files(pattern="[.]R$", path=paste0(filepath_base,"fun"), full.names=TRUE)

# source all functions
sapply(sourceFileNames, FUN=source)

# set the global path variables for the current script
setPathGlobal(filepath_base,csess = 15)


# kernelsize for smoothing
ksize<-3
msize<-5

# TRUE = filter + standard morphometric + fuzzy
# FALSE= wood + fuzzy
basic<-TRUE

#[Wood]
wsize<-    3
tol_slope<-14.000000
tol_curve<-0.00001
exponent<- 0.000000
zscale<-   1.000000

#[FuzzyLf]
slopetodeg<-   0
t_slope_min<- 3.0
t_slope_max<- 10.0
t_curve_min<-  0.00000001
t_curve_max<-  0.0001


# (GDAL) gdalwarp is used to (1) convert the data format (2) assign the
# projection information to the data.
gdalUtils::gdalwarp(paste0(pd_gi_input,inputFile),paste0(pd_gi_output,"rt_dem.sdat"), overwrite=TRUE,  of='SAGA') 

# (SAGA) import DEM to saga 
system(paste0("saga_cmd io_gdal 0 ",
              "-GRIDS ", pd_gi_output,"rt_dem.sgrd ",
              "-TRANSFORM 0 ",
              "-FILES ",pd_gi_input,inputFile," ",
              "-INTERPOL 0 ")
)


# (R) read raster
dem<-raster::raster(paste0(pd_gi_input,inputFile))

if (basic) {
# (R) mean filter
#demf<- focal(dem, w=matrix(1/(ksize*ksize)*1.0, nc=ksize, nr=ksize))

# (SAGA) takes times longer than focal
#print('Filtering the DEM - may take a while...')
#system(paste0("saga_cmd grid_filter 0 ",
#              "-INPUT ",pd_gi_output,"rt_dem.sgrd ",
#              "-RESULT ",pd_gi_output,"rt_fildem.sgrd ",
#              "-RADIUS ",as.character((ksize/2)+1)))
#gdalUtils::gdalwarp(paste0(pd_gi_output,"rt_fildem.sdat"),paste0(pd_gi_output,"rt_fildem.tif") , overwrite=TRUE)  
#fildem<-raster::raster(paste0(pd_gi_output,"rt_fildem.tif"))
#plot(fildem)

# standard morhpometry
system(paste0("saga_cmd ta_morphometry 0 ",
              "-ELEVATION ", pd_gi_output,"rt_fildem.sgrd ",
              "-UNIT_SLOPE 1 ",
              "-UNIT_ASPECT 1 ",
              "-SLOPE ",pd_gi_run,"rt_slope.sgrd ", 
              "-ASPECT ",pd_gi_run,"rt_aspect.sgrd ",
              "-C_TANG ",pd_gi_run,"rt_tangcurve.sgrd ",
              "-C_PROF ",pd_gi_run,"rt_profcurve.sgrd ",
              "-C_MINI ",pd_gi_run,"rt_mincurve.sgrd ",
              "-C_MAXI ",pd_gi_run,"rt_maxcurve.sgrd"))

} else{


  
  # morphometric features
  # calculate wood's terrain indices   wood= 1=planar,2=pit,3=channel,4=pass,5=ridge,6=peak
  system(paste0("saga_cmd ta_morphometry 23 ",
                "-DEM ",pd_gi_output,"rt_dem.sgrd " ,
                "-FEATURES ",pd_gi_output,"rt_woodLANDFORM.sgrd " ,
                "-ELEVATION ",pd_gi_output,"rt_genSURFACE.sgrd " , 
                "-SLOPE ",pd_gi_run,"rt_slope.sgrd ", 
                "-ASPECT ",pd_gi_run,"rt_aspect.sgrd ",
                "-LONGC ",pd_gi_run,"rt_profcurve.sgrd ",
                "-CROSC ",pd_gi_run,"rt_tangcurve.sgrd ",
                "-MINIC ",pd_gi_run,"rt_mincurv.sgrd ",
                "-MAXIC ",pd_gi_run,"rt_maxcurv.sgrd ", 
                "-SIZE ",ksize," ",
                "-TOL_SLOPE ",tol_slope," ",
                "-TOL_CURVE ",tol_curve," ",
                "-EXPONENT ",exponent," ",
                "-ZSCALE ", zscale))
  
  gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_woodLANDFORM.sdat"),paste0(pd_gi_run,"rt_woodLANDFORM.tif") , overwrite=TRUE)  
  wooddem<-raster::raster(paste0(pd_gi_input,"rt_genSURFACE.tif"))
  wood<-raster::raster('rt_woodLANDFORM.tif')
  raster::plot(wood)
  

}

# calculate Jochen Schmidt's fuzzy landforms (https://faculty.unlv.edu/buckb/GEOL%20786%20Photos/NRCS%20data/Fuzz/felementf.aml) fuzzylandoforms are:  
# using SAGA 'ta_morphometry',"Fuzzy Landform Element Classification" The result is classified as follows:
# PLAIN     , 100  # PIT       , 111  # PEAK      , 122  # RIDGE     , 120  # CHANNEL   , 101	
# SADDLE    , 121	# BSLOPE    ,   0	# FSLOPE    ,  10	# SSLOPE    ,  20	# HOLLOW    ,   1	
# FHOLLOW   ,  11	# SHOLLOW   ,  21	# SPUR      ,   2	# FSPUR     ,  12	# SSPUR     ,  22	
# NOTEwood SIZE=9,TOL_SLOPE=10.000000,TOL_CURVE=0.00001,EXPONENT=0.000000,ZSCALE=1.000000 
# fuzzy SLOPETODEG='0',T_SLOPE_MIN=0.0000001,T_SLOPE_MAX=20.000000,T_CURVE_MIN=0.00000001,T_CURVE_MAX=0.0001))
system(paste0("saga_cmd ta_morphometry 25 ",
              "-SLOPE "  ,pd_gi_run,"rt_slope.sgrd ", 
              "-MINCURV ",pd_gi_run,"rt_mincurve.sgrd ",
              "-MAXCURV ",pd_gi_run,"rt_maxcurve.sgrd ", 
              "-TCURV "  ,pd_gi_run,"rt_tangcurve.sgrd ",
              "-PCURV "  ,pd_gi_run,"rt_profcurve.sgrd ",
              "-FORM "   ,pd_gi_run,"rt_LANDFORM.sgrd ", 
              "-SLOPETODEG   ",slopetodeg ," ",
              "-T_SLOPE_MIN  ",t_slope_min," ",
              "-T_SLOPE_MAX  ",t_slope_max," ",
              "-T_CURVE_MIN  ",t_curve_min," ",
              "-T_CURVE_MAX  ",t_curve_max))
#microbenchmark(
#system(paste0("saga_cmd grid_filter 6 ",
#              "-INPUT ",pd_gi_run,"rt_LANDFORM.sgrd ",
#              "-MODE 0 ",
#              "-RESULT ",pd_gi_output,"rt_modal.sgrd ",
#              "-RADIUS  ",msize," ",
#              "-THRESHOLD 0.000000"))
#,times = 1)


# read sdat file into raster object
gdalUtils::gdalwarp(paste0(pd_gi_output,"rt_modal.sdat"),paste0(pd_gi_run,"rt_LANDFORM.tif") , overwrite=TRUE) 
landform<-raster::raster('rt_LANDFORM.tif')
raster::plot(landform)
#microbenchmark(
# get rid of the noise
flatm<- focal(landform, w=matrix(1, nc=msize, nr=msize),fun=modal,na.rm = TRUE, pad = TRUE)
#, times = 1)


# we have to reassign correct projection due to some troubles in twgs84 transformations
# crs(peak.area)<-
# reclassify fuzzylandforms to get a binary peak mask
flat<-raster::reclassify(landform, c(0,99,0, 99,100,1,100,200,0 ))

# plot it
raster::plot(flat)
# reclass it statically
flat[flat==1 & dem>240]<-1  
flat[flat==1 & dem<=240 ]<-2
raster::plot(flat)

# plot it again
raster::plot(flat)

rawHlp<-sagaModuleHelp("grid_filter","0")
sagaModuleCmd("grid_filter","0")
system2("saga_cmd","io_gdal 0 -h",stdout = TRUE)




