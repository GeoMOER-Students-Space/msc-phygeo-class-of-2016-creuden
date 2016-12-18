# gi-ws-06 main control script 
######### setup the environment -----------------------------------------------
# define project folder
if(Sys.info()["sysname"] == "Windows"){
filepath_base<- "C:/Users/creu/Documents/lehre/active/msc-phy-geo-2016/msc-phygeo-class-of-2016-creuden/"
} else {
filepath_base<-  "/home/creu/lehre/msc/active/msc-2016/msc-phygeo-class-of-2016-creuden/"
}
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

# x<- raster::stack(paste0(pd_rs_aerial,"test.tif"))
# # glcm package
#   glcm<-textureVariables(x,
#                          nrasters=1:nlayers(x),
#                          filter=c(3),
#                          stats=c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
#                                  "second_moment", "correlation"),
#                          parallel=TRUE,
#                          n_grey = 8 )

# calculate rgbi
rgbi <- rgbI(img,writeTif = TRUE,outDir = pd_rs_aerial_rgbi)
rgbi_file<-    list.files(pattern="rgbi", path=paste0(pd_rs_aerial_rgbi), full.names=TRUE)

# haralick  
hara<- otbHaraTex(input=rgbi_file, texture="advanced",retRaster = TRUE,outDir = pd_rs_aerial_texture)
# standard stat
stat<- otblocalStat(input=rgbi_file, radius=3,retRaster = TRUE,outDir = pd_rs_aerial_texture)
# two times edge
#touzi<- otbEdge(input=rgbi_file, filter = "touzi", filter.touzi.yradius = 5, filter.touzi.xradius = 5,retRaster = TRUE,outDir = pd_rs_aerial_texture)
sobel<- otbEdge(input=rgbi_file, filter = "sobel",retRaster = TRUE,outDir = pd_rs_aerial_texture)
# two times morpho
# gmc<- otbGrayMorpho(input=rgbi_file, structype = "cross",retRaster = TRUE,outDir = pd_rs_aerial_texture)
# gmb<- otbGrayMorpho(input=rgbi_file, structype.ball.xradius = 5,structype.ball.yradius = 10,retRaster = TRUE,outDir = pd_rs_aerial_texture)


# create a list for each band
b1_textures <- list.files(pattern="band_1", path=paste0(pd_rs_aerial_texture), full.names=TRUE)
b2_textures <- list.files(pattern="band_2", path=paste0(pd_rs_aerial_texture), full.names=TRUE)
b3_textures <- list.files(pattern="band_3", path=paste0(pd_rs_aerial_texture), full.names=TRUE)

# merge all tifs to one tif
system(paste("gdal_merge.py -of GTiff   -o ", paste0(pd_rs_aerial,"merge.tif"), b1_tifFileNames,b2_tifFileNames,b3_tifFileNames,rgbi_file))

