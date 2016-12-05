# gi-ws-04 main control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
# gi-ce-04  (1) show the use of the external helper functions (for a given fixed structure) 
#           (2) shows the basic workflow of a main script using main sections as marked with 
#               ### some explanation -----------------------------------------
#           (3) shows good practice syntax and commandline examples  
#           (4) shows some alternatives using SAGA or R for the same task
#
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden

#########                       -----------------------------------------------
######### setup the environment -----------------------------------------------
#########                       -----------------------------------------------
# define project folder

filepath_base<-"D:/BEN/msc/msc-phygeo-class-of-2016-creuden/"


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

#########                       -----------------------------------------------
######### initialize the external GIS packages --------------------------------
#########                       -----------------------------------------------
# check GDAL binaries and start gdalUtils
gdal<- initgdalUtils()

# check SAGA binaries and export pathes to .envGlobal
# ***NOTE*** c("C:\\OSGeo4W64\\apps\\saga","C:\\OSGeo4W64\\apps\\saga\\modules")
#            is the default osgeo4w64 bit installation path
#            it is strongly recommended to install SAGA standalone
initSAGA(c("C:\\apps\\saga_3.0.0_x64","C:\apps\\saga_3.0.0_x64\\tools"))

#########                       -----------------------------------------------
######### START of the thematic stuff ----------------------------------------
#########                       -----------------------------------------------
# Basic Workflow Task 1 post filtering
# 1) import DEM data
# 2) calculate basic morphometry as needed for fuzzy landforms 
# 3) classify landforms using the fuzzy landform algorithm
# 4) filter result to get rid of noise (modal filter due to categorical variables)
# 5) reclassify the landforms to plain (flat< 250 m ), plateau (flat>300 m) other (the rest)


# Basic Workflow Task 2 pre filtering
# 1) import DEM data
# 2) filter dem to smooth areas (mean filter is the most straightforward approach)
# 3) calculate basic morphometry as needed for fuzzy landforms 
# 4) classify landforms using the fuzzy landform algorithm
# 5) reclassify the landforms to plain (flat< 250 m ), plateau (flat>300 m) other (the rest)

# TODO
# define the kernel size parameters for filtering 
# understand which output from the basic morphometry is used as input for fuzzy landform 
# define the parameters used by fuzzy landforms 

#########                       -----------------------------------------------
######### setup the the variables for the script ------------------------------
#########                       -----------------------------------------------
# kernelsize for smoothing (meanfilter)
ksize<-3

# kernelsize for smothing modal filter
msize<-5

#[FuzzyLf]
slopetodeg<-   0
t_slope_min<- 3.0
t_slope_max<- 10.0
t_curve_min<-  0.00000001
t_curve_max<-  0.0001

#########                       -----------------------------------------------
######### start core script     -----------------------------------------------
#########                       -----------------------------------------------

# (GDAL) gdalwarp is used to convert the data format from tif to SAGA format
gdalUtils::gdalwarp(paste0(pd_gi_input,inputFile),paste0(pd_gi_run,"rt_dem.sdat"), overwrite=TRUE,  of='SAGA') 

# (SAGA) import DEM to saga
# ***NOTE1*** same as before using saga_cmd
# ***NOTE2*** if you use a SAGA Version < 2.14 you have to change the call to:
#            system(paste0(sagaCmd, ' io_gdal "GDAL: Import Raster"',
#            ' -GRIDS=', pd_gi_run,'rt_dem.sgrd',
#            ' -FILES=',pd_gi_input,inputFile,
#            ' -INTERPOL=4'))
## (SAGA 3.0.0) 
# ***NOTE3*** All params that are set to zero must be ommitted!
system(paste0(sagaCmd," io_gdal 0",
              " -GRIDS=", pd_gi_run,"rt_dem.sgrd",
              " -FILES=",pd_gi_input,inputFile)
)

# (R) assign the input file to a R raster
dem<-raster::raster(paste0(pd_gi_input,inputFile))


###  mean filter of the input file -----------------------------------------
# ***NOTE1*** we are using the parameter setting from above
# ***NOTE2*** this time R is performing the filtering significantly FASTER than SAGA
# (R) mean filter very fast due to the mean calculation inside the filter matrix
demf<- raster::focal(dem, w=matrix(1/(ksize*ksize)*1.0, nc=ksize, nr=ksize))

# export it to tif format
raster::writeRaster(demf,paste0(pd_gi_run,"rt_fildemR.tif"),overwrite=TRUE)

# ***NOTE*** This is possible because we already did convert the input file to SAGA
# (SAGA) takes times longer than focal
system(paste0(sagaCmd," grid_filter 0",
              " -INPUT ",pd_gi_run,"rt_dem.sgrd",
              " -RESULT ",pd_gi_run,"rt_fildemSAGA.sgrd",
              " -RADIUS ",as.character(ceiling((ksize/2)+1))))

# (R) plot the results
raster::plot(demf)

# (SAGA) plot the results
# ***NOTE*** we need to re-convert SAGA to raster
gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_fildemSAGA.sdat"),paste0(pd_gi_run,"rt_fildemSAGA.tif") , overwrite=TRUE)  
demfSAGA<-raster::raster(paste0(pd_gi_run,"rt_fildemSAGA.tif"))
raster::plot(demfSAGA$rt_fildemSAGA)

###  now caluating the standard morhpometry -----------------------------------
# *** NOTE *** take care if you take the results from:
# (1) SAGA "rt_fildemSAGA.sgrd" 
# (2) R ("rt_fildemR.tif")
system(paste0(sagaCmd," ta_morphometry 0 ",
              "-ELEVATION ", pd_gi_run,"rt_fildemSAGA.sgrd ",
              "-UNIT_SLOPE 1 ",
              "-UNIT_ASPECT 1 ",
              "-SLOPE ",pd_gi_run,"rt_slope.sgrd ", 
              "-ASPECT ",pd_gi_run,"rt_aspect.sgrd ",
              "-C_TANG ",pd_gi_run,"rt_tangcurve.sgrd ",
              "-C_PROF ",pd_gi_run,"rt_profcurve.sgrd ",
              "-C_MINI ",pd_gi_run,"rt_mincurve.sgrd ",
              "-C_MAXI ",pd_gi_run,"rt_maxcurve.sgrd"))


###  (SAGA) 'ta_morphometry',"Fuzzy Landform Element Classification"-----------
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

# export it to  R as an raster object
gdalUtils::gdalwarp(paste0(pd_gi_run,"rt_LANDFORM.sdat"),
                    paste0(pd_gi_run,"rt_LANDFORM.tif") , 
                    overwrite=TRUE) 
landformSAGA<-raster::raster(paste0(pd_gi_run,"rt_LANDFORM.tif"))


###  modal filter for smoothing the classified areas --------------------------

# (SAGA) first using SAGA get rid of the noise
system(paste0(sagaCmd," grid_filter 6 ",
              "-INPUT ",pd_gi_run,"rt_LANDFORM.sgrd ",
              "-MODE 0 ",
              "-RESULT ",pd_gi_run,"rt_modalSAGA.sgrd ",
              "-RADIUS  ",msize," ",
              "-THRESHOLD 0.000000"))


# (R) same with R get rid of the noise

# ***NOTE2*** this time R is performing the filtering significantly SLOWER than SAGA
landformModalR<- raster::focal(landformSAGA, w=matrix(1, nc=msize, nr=msize),fun=raster::modal,na.rm = TRUE, pad = TRUE)



###  reclass to plain / plateau  ----------------------------------------------
# *** NOTE *** take care if you take the results from:
# (1) SAGA  not exported yet
# (2) R ("landformModalR")
flat<-raster::reclassify(landformModalR, c(0,99,0, 99,100,1,100,200,0 ))

# finally reclass it according to fixed altitude tresholds
flat[flat==1 & dem>300]<-3  
flat[flat==1 & dem>240 & dem <= 300]<-0  
flat[flat==1 & dem<=240 ]<-2
raster::plot(flat)


