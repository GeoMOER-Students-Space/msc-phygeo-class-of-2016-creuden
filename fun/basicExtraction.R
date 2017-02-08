
basicExtraction <- function(x,fN,nsamples=10000,responseCat="id"){
  
  if (!class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    imgStack<-NULL
    files<-paste0(x,".tif")
    # put all raster in a brick
    imgStack<- brick(lapply(files, raster))
    writeraster(imgStack,filename = "brick.tif",overwrite=TRUE)
    # if GEOTIFF or other gdal type of data
  } else{
    imgStack<- raster::stack(x)
  }
  if (class(fN)=="character") {
  vecObj   <- importVec(fN)
  } else {
  vecObj <- fN  
  }
  exValDF       <- getPixVal(imgStack = imgStack,
                             vecObj = vecObj,
                             responseCol = responseCat)
  #exValDF$class <- factor(exValDF$class)
  return(exValDF)
}


importVec<- function(fN){
  cat("importing ",fN)
  # read shapefile
  if (path.expand(raster::extension(fN)) == ".json") 
    vecObj<-rgdal::readOGR(dsn = path.expand(fN), layer = "OGRGeoJSON",verbose = FALSE)
  else if (path.expand(raster::extension(fN)) != ".kml" ) 
    vecObj<- rgdal::readOGR(dsn = path.expand(dirname(fN)), layer = tools::file_path_sans_ext(basename(fN)),verbose = FALSE)
  else if (path.expand(raster::extension(fN)) == ".kml" ) {
    vecObj<- rgdal::readOGR(dsn = path.expand(fN), layer = tools::file_path_sans_ext(basename(fN)),verbose = FALSE)    
  }
  return(vecObj)
}

getPixVal<- function(imgStack=NULL,vecObj=NULL,responseCol=NULL){
  #extract  Area pixel values
  exValDF = data.frame(matrix(vector(), nrow = 0, ncol = length(names(imgStack)) + 1))   
  for (i in 1:length(vecObj[[responseCol]])){
    category <- vecObj[[responseCol]][i]
    if (i %% 500 == 0)  cat("\n extracting feature: ",i," of: ",length(unique(vecObj[[responseCol]])))
    categorymap <- vecObj[vecObj[[responseCol]] == category,]
    dataSet <- raster::extract(imgStack, categorymap)
    dataSet <- lapply(dataSet, function(x){cbind(x,  as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    exValDF <- rbind(exValDF, df)
  }
  names(exValDF)<-gsub(names(exValDF),pattern = "\\.",replacement = "_")
  names(exValDF)<-gsub(names(exValDF),pattern = paste0("V",as.character(ncol(exValDF))),replacement = responseCol)
  return(exValDF)
}

