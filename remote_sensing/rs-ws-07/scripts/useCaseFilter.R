# rs-ws-07
#
# MOC - Remote Sensing (T. Nauss, C. Reudenbach)
# useCase control script for textureVariables
# demonstrates:
# - the use of otbcli system calls
# - differences in deriving textures using R (GLCM) vs OTB (HaralickTexture) 
# - the use of the init functions
# - how to call the otb and glcm functions
# - introduce curl for convenient platform independent downloading webcontent
# - gives another sapply example (plotting) 
#
#  it is recommended to read the documentation of the called functions

######### setup the environment -----------------------------------------------
# define project folder
if(Sys.info()["sysname"] == "Windows"){
  filepath_base<- "C:/Users/creu/Documents/lehre/active/msc-phy-geo-2016/msc-phygeo-class-of-2016-creuden/"
} else {
  filepath_base<-  "/home/creu/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"
}
# define the actual course session
activeSession<-7

# make a list of all functions in the corresponding function folder
sourceFileNames <- list.files(pattern="[.]R$", path=paste0(filepath_base,"fun"), full.names=TRUE)

# source all functions
res<- sapply(sourceFileNames, FUN=source)

# if necessary, create filestructure
createMocFolders(filepath_base)

# get the global path variables for the activeSession 
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "rs")

# set working directory
setwd(pd_rs_run)

# define the used input file(s)
# download data from the Bavarian Landesamt für Digitalisierung,
# Breitband und Vermessung 
url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
res <- curl::curl_download(url, "testdata.zip")
unzip(res,junkpaths = TRUE,overwrite = TRUE)
inputFile<- "4490600_5321400.tif"

######### initialize the external GIS packages --------------------------------
### NOTE: providing the correct pathes will EXTREMLY 
###       speed up the initialisation process
###       Depending on your installations 
###       you may be asked to choose a version

# check GDAL binaries, start gdalUtils (full search because no path is provided)
gdal<- initgdalUtils()

# initialize SAGA GIS (full search because no path is provided)
initSAGA()

# initialize OTB (full search because no path is provided)  
initOTB()

######## start texture extracting ------------------------------------------------------------

# read tif file into a raster stack
x<- raster::stack(paste0(pd_rs_run,inputFile))

# because we are geographers we like to see where we are
raster::plotRGB(x)

# glcm textures
glcm<-textureVariables(x,
                       nrasters=1:nlayers(x),
                       kernelSize=c(3),
                       stats=c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
                               "second_moment", "correlation"),
                       parallel=TRUE,
                       n_grey = 8 )
# plot them
raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.1)))
raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.2)))
raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.3)))

# haralick advanced filter  
hara<- otbHaraTex(input=paste0(pd_rs_run,inputFile), texture="simple",retRaster = TRUE)
# plot them
res<- sapply(hara, FUN=raster::plot)

# standard stat (mean, variance, skewness, kurtosis )
stat<- otblocalStat(input=paste0(pd_rs_run,inputFile),radius=5,retRaster = TRUE)
# plot them
res<- sapply(stat, FUN=raster::plot)

# two arbitrary edge filter
touzi<- otbEdge(input=paste0(pd_rs_run,inputFile),filter = "touzi", filter.touzi.yradius = 5, filter.touzi.xradius = 5,retRaster = TRUE)
sobel<- otbEdge(input=paste0(pd_rs_run,inputFile),filter = "sobel",retRaster = TRUE)
# plot them
res<- sapply(touzi, FUN=raster::plot)
res<- sapply(sobel, FUN=raster::plot)

# two arbitrary morphological gray level filter
gmc<- otbGrayMorpho(input=paste0(pd_rs_run,inputFile),structype = "cross",retRaster = TRUE)
gmb<- otbGrayMorpho(input=paste0(pd_rs_run,inputFile),structype.ball.xradius = 5,structype.ball.yradius = 10,retRaster = TRUE)
# plot them
res<- sapply(gmc, FUN=raster::plot)
res<- sapply(gmb, FUN=raster::plot)
