
basicExtraction <- function(x,fN,nsamples=10000,responseCat="id"){
  imgStack    <- tifList2Brick(x)
  if (class(fN)=="character") {
  vecObj   <- importVec(fN)
  } else {
  vecObj <- fN  
  }
  exValDF       <- getPixVal(imgStack = imgStack,
                             vecObj = vecObj,
                             responseCol = responseCat)
  exValDF$class <- factor(exValDF$class)
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
  #extract training Area pixel values
  exValDF = data.frame(matrix(vector(), nrow = 0, ncol = length(names(imgStack)) + 1))   
  for (i in 1:length(unique(vecObj[[responseCol]]))){
    category <- unique(vecObj[[responseCol]])[i]
    cat("\n extracting cat: ",levels(category)[i]," no: ",i," of: ",length(unique(vecObj[[responseCol]])))
    categorymap <- vecObj[vecObj[[responseCol]] == category,]
    dataSet <- raster::extract(imgStack, categorymap)
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    exValDF <- rbind(exValDF, df)
  }
  names(exValDF)<-gsub(names(exValDF),pattern = "\\.",replacement = "_")
  return(exValDF)
}

