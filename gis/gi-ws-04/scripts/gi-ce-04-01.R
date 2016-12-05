# gi-ws-04 main control script 
######### setup the environment -----------------------------------------------
# define project folder

filepath_base<-"/home/creu/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"

# define the actual course session
activeSession<-4

# define the used input file(s)
inputFile<- "geonode-las_dtm_01m.tif"

# make a list of all functions in the corresponding function folder
sourceFileNames <- list.files(pattern="[.]R$", path=paste0(filepath_base,"fun"), full.names=TRUE)

# source all functions
res<- sapply(sourceFileNames, FUN=source)

# if at a new location create filestructure
createMocFolders(filepath_base)

# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession)

# set working directory
setwd(pd_gi_run)

######### initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal<- initgdalUtils()

initSAGA(c("C:\\apps\\saga_3.0.0_x64","C:\apps\\saga_3.0.0_x64\\modules"))

######## set vars ------------------------------------------------------------


# kernelsize
modalKernelSize<-5
kernelSize<-3

#[FuzzyLf]

slopetodeg<-   0
t_slope_min<- 3.0
t_slope_max<- 10.0
t_curve_min<-  0.00000001
t_curve_max<-  0.0001

######### start core script     -----------------------------------------------

# (R) assign the input file to a R raster
dem<-raster::raster(paste0(pd_gi_input,inputFile))

# (GDAL) convert the TIF to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_input,inputFile),paste0(pd_gi_run,"rt_dem.sdat"), overwrite=TRUE,  of='SAGA') 

# (R) mean filter 
demf<- raster::focal(dem, w=matrix(1/(kernelSize*kernelSize)*1.0, nc=kernelSize, nr=kernelSize))

# R write data to TIF format
raster::writeRaster(demf,paste0(pd_gi_run,"rt_fildemR.tif"),overwrite=TRUE)

# (GDAL) convert the TIF to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_fildemR.tif"),paste0(pd_gi_run,"rt_fildemR.sdat"), overwrite=TRUE,  of='SAGA') 

# ultra straightforward loop
for (type in  list("mean","modal")){
  if (type == "mean") {
    currentName<- "rt_fildemR.tif"
  } else {
    currentName<- "rt_dem.tif"  
  }
  
  # (SAGA) standard morhpometry 
  system(paste0(sagaCmd," ta_morphometry 0 ",
                " -ELEVATION ", pd_gi_run,currentName,
                " -UNIT_SLOPE 1 ",
                " -UNIT_ASPECT 1 ",
                " -SLOPE ",pd_gi_run,"rt_slope.sgrd ", 
                " -ASPECT ",pd_gi_run,"rt_aspect.sgrd ",
                " -C_TANG ",pd_gi_run,"rt_tangcurve.sgrd ",
                " -C_PROF ",pd_gi_run,"rt_profcurve.sgrd ",
                " -C_MINI ",pd_gi_run,"rt_mincurve.sgrd ",
                " -C_MAXI ",pd_gi_run,"rt_maxcurve.sgrd"))
  
  
  #  (SAGA) 'ta_morphometry',"Fuzzy Landform Element Classification"
  # *** NOTE *** we are using the parameter setting from above
  # Reference: Jochen Schmidt's fuzzy landforms (https://faculty.unlv.edu/buckb/GEOL%20786%20Photos/NRCS%20data/Fuzz/felementf.aml) fuzzylandoforms are:  
  # The result is classified as follows:
  # PLAIN     , 100  # PIT       , 111  # PEAK      , 122  # RIDGE     , 120  # CHANNEL   , 101	
  # SADDLE    , 121	# BSLOPE    ,   0	# FSLOPE    ,  10	# SSLOPE    ,  20	# HOLLOW    ,   1	
  # FHOLLOW   ,  11	# SHOLLOW   ,  21	# SPUR      ,   2	# FSPUR     ,  12	# SSPUR     ,  22	
  # type       map name		meaning
  # type       PLAIN		plain (membership)
  # type       PIT		pit (membership)
  # type       PEAK		peak (membership)
  # type       RIDGE		ridge (membership)
  # type       CHANNEL		channel (membership)
  # type       SADDLE		saddele (membership)
  # type       BSLOPE		backslope (membership)
  # type       FSLOPE		foot slope (membership)
  # type       SSLOPE		shoulder slope (membership)
  # type       HOLLOW		hollow (membership)
  # type       FHOLLOW		foot hollow (membership)
  # type       SHOLLOW		shoulder hollow (membership)
  # type       SPUR		spur (membership)
  # type       FSPUR		foot spur (membership)
  # type       SSPUR		shoulder spur (membership)
  # type       OUT		form (from maximum membership)
  # type       MEM		maximum membership
  # type       ENT		entropy
  # https://data.nodc.noaa.gov/coris/library/NOAA/CRCP/other/other_crcp_publications/PIBHMC/linkages_project_methods_final.pdf
  system(paste0(sagaCmd," ta_morphometry 25 ",
                " -SLOPE "  ,pd_gi_run,"rt_slope.sgrd ", 
                " -MINCURV ",pd_gi_run,"rt_mincurve.sgrd ",
                " -MAXCURV ",pd_gi_run,"rt_maxcurve.sgrd ", 
                " -TCURV "  ,pd_gi_run,"rt_tangcurve.sgrd ",
                " -PCURV "  ,pd_gi_run,"rt_profcurve.sgrd ",
                " -FORM "   ,pd_gi_run,"rt_LANDFORM.sgrd ", 
                " -SLOPETODEG   ",slopetodeg ," ",
                " -T_SLOPE_MIN  ",t_slope_min," ",
                " -T_SLOPE_MAX  ",t_slope_max," ",
                " -T_CURVE_MIN  ",t_curve_min," ",
                " -T_CURVE_MAX  ",t_curve_max))
  
  
  ###  (SAGA) modal filter
  if (type =="modal") {
    # (SAGA) first using SAGA get rid of the noise
    system(paste0(sagaCmd," grid_filter 6",
                  " -INPUT ",pd_gi_run,"rt_LANDFORM.sgrd",
                  " -MODE 0 ",
                  " -RESULT ",pd_gi_run,"rt_LANDFORM.sgrd",
                  " -RADIUS  ",modalKernelSize," ",
                  " -THRESHOLD 0.000000"))
  }
  
  # (GDAL) convert the TIF to SAGA format
  gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_LANDFORM.sdat"),paste0(pd_gi_run,"rt_LANDFORM.tif") , overwrite=TRUE)  
  
  # (R) assign to raster
  landform<-raster::raster(paste0(pd_gi_run,"rt_LANDFORM.tif"))

  # reclassify from all landforms to flat only
  flat<-raster::reclassify(landform, c(0,99,0, 99,100,1,100,200,0 ))
  
  # reclass with arbitray altitude tresholds
  flat[flat==1 & dem>300]<-3   # plateau
  flat[flat==1 & dem>240 & dem <= 300]<-0  # even if flat no assignment
  flat[flat==1 & dem<=240 ]<-2 # plain
  
  # SHOW RESULT
  raster::plot(flat,main=type)
  
}
