# rs-ws-07
#
# MOC - Remote Sensing (T. Nauss, C. Reudenbach)
# useCase control script for textureVariables
#' 
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

# if at a new location create filestructure
createMocFolders(filepath_base)

# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "rs")

# set working directory
setwd(pd_rs_run)

#define the used input file(s)
url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
res <- curl::curl_download(url, "testdata.zip")
unzip(res,junkpaths = TRUE,overwrite = TRUE)
inputFile<- "4490600_5321400.tif"

######### initialize the external GIS packages --------------------------------
### NOTE providing the correct pathes will extremly 
###       speed up the initialisation process

# check GDAL binaries and start gdalUtils
gdal<- initgdalUtils()

# initialize SAGA GIS 
initSAGA()

# initialize OTB  
initOTB()

######## start tests ------------------------------------------------------------

# read tif file
x<- raster::stack(paste0(pd_rs_run,inputFile))
raster::plotRGB(x)

# glcm textures
glcm<-textureVariables(x,
                       nrasters=1:nlayers(x),
                       filter=c(3),
                       stats=c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
                               "second_moment", "correlation"),
                       parallel=TRUE,
                       n_grey = 8 )
raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.1)))
raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.2)))
raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.3)))

# haralick advanced filter  
hara<- otbHaraTex(input=paste0(pd_rs_run,inputFile), texture="advanced",retRaster = TRUE)
res<- sapply(hara, FUN=raster::plot)

# standard stat (mean, variance, skewness, kurtosis )
stat<- otblocalStat(input=paste0(pd_rs_run,inputFile),radius=5,retRaster = TRUE)
res<- sapply(stat, FUN=raster::plot)

# two arbitrary edge filter
touzi<- otbEdge(input=paste0(pd_rs_run,inputFile),filter = "touzi", filter.touzi.yradius = 5, filter.touzi.xradius = 5,retRaster = TRUE)
sobel<- otbEdge(input=paste0(pd_rs_run,inputFile),filter = "sobel",retRaster = TRUE)
res<- sapply(touzi, FUN=raster::plot)
res<- sapply(sobel, FUN=raster::plot)


# two arbitrary morphological gray level filter
gmc<- otbGrayMorpho(input=paste0(pd_rs_run,inputFile),structype = "cross",retRaster = TRUE)
gmb<- otbGrayMorpho(input=paste0(pd_rs_run,inputFile),structype.ball.xradius = 5,structype.ball.yradius = 10,retRaster = TRUE)
res<- sapply(gmc, FUN=raster::plot)
res<- sapply(gmb, FUN=raster::plot)
