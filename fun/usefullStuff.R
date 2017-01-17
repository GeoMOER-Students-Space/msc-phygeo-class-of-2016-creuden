# Split rgb
gdalsplit<-function(fn){
  directory<-dirname(fn)  
  noBands<-seq(length(grep(gdalUtils::gdalinfo(fn,nomd = TRUE),pattern = "Band ")))
  for (i in noBands){
    gdalUtils::gdal_translate(fn,paste0(directory,"/b",i,".tif"),b=i)
  }
  return(noBands)
}

tifList2Brick<- function(x,path){
  cat("importing ",x)
  
  if (class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    imgStack<-NULL
    files<-paste0(x,".tif")
    # put all raster in a brick
    imgStack<- brick(lapply(files, raster))
    writeraster(imgStack,filename = "brick.tif",overwrite=TRUE)
    # if GEOTIFF or other gdal type of data
  } else{
    imgStack<- raster::brick(x)
  }
}