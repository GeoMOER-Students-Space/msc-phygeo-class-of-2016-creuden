# Split rgb
gdalsplit<-function(fn){
  directory<-dirname(fn)  
  noBands<-seq(length(grep(gdalUtils::gdalinfo(fn,nomd = TRUE),pattern = "Band ")))
  for (i in noBands){
    gdalUtils::gdal_translate(fn,paste0(directory,"/b",i,".tif"),b=i)
  }
  return(noBands)
}