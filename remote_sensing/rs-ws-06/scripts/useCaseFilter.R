# gi-ws-06 main control script 
######### setup the environment -----------------------------------------------
# define project folder
filepath_base<-"C:/Users/creu/Documents/lehre/active/msc-phy-geo-2016/msc-phygeo-class-of-2016-creuden/"

# define the actual course session
activeSession<-5

# define the used input file(s)
inputFile<- "test.tif"

# make a list of all functions in the corresponding function folder
sourceFileNames <- list.files(pattern="[.]R$", path=paste0(filepath_base,"fun"), full.names=TRUE)

# source all functions
res<- sapply(sourceFileNames, FUN=source)

# if at a new location create filestructure
createMocFolders(filepath_base)

# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "rs")

# set working directory
setwd(pd_rs_run)

######### initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal<- initgdalUtils()

# initialize SAGA GIS bindings for SAGA 3.x
initSAGA(c("C:\\apps\\saga_3.0.0_x64","C:\apps\\saga_3.0.0_x64\\modules"))

# initialize OTB bindings for OSGeo4W64 default 
initOTB(otbType="osgeo4w64OTB")

######## start tests ------------------------------------------------------------

x<- raster::stack(paste0(pd_rs_aerial,"test.tif"))
# glcm package
  glcm<-textureVariables(x,
                         nrasters=1:nlayers(x),
                         filter=c(3),
                         stats=c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
                                 "second_moment", "correlation"),
                         parallel=TRUE,
                         n_grey = 8 )
# haralick  
hara<- otbHaraTex(input=paste0(pd_rs_aerial,"test.tif"), texture="simple",retRaster = TRUE)
# standard stat
stat<- otblocalStat(input=paste0(pd_rs_aerial,"test.tif"),radius=5,retRaster = TRUE)
# two times edge
otbedge(input=paste0(pd_rs_aerial,"test.tif"),filter = "touzi", filter.touzi.yradius = 5, filter.touzi.xradius = 5)
otbedge(input=paste0(pd_rs_aerial,"test.tif"),filter = "sobel")
# two times morpho
otbgraymorpho(input=paste0(pd_rs_aerial,"test.tif"),structype = "cross")
otbgraymorpho(input=paste0(pd_rs_aerial,"test.tif"),structype.ball.xradius = 5,structype.ball.yradius = 10)
