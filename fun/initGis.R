# gi-ws-05-1
#' @description  MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' initGIS
#'@return 
#' defines external GIS software and GI binaries
#'
initGis <- function(){
  
## (gdalUtils) check for a valid GDAL binary installation on your system
gdalUtils::gdal_setInstallation()
valid.install<-!is.null(getOption("gdalUtils_gdalPath"))
if (!valid.install){
  stop('no valid GDAL/OGR found')
}else {
  cat("GDAL version: ",getOption("gdalUtils_gdalPath")[[1]]$version)
  gdal<-getOption("gdalUtils_gdalPath")[[1]]$version
}


# (R) set pathes  of SAGA modules and binaries depending on OS  
exist<-FALSE
if(Sys.info()["sysname"] == "Windows"){
  exV("saga_bin", "C:/OSGeo4W/apps/saga")
  exV("saga_mod",  "C:/OSGeo4W/apps/saga/modules/")
  # check if already in system path
  p<- Sys.getenv("PATH")
  if(substr(p, 1,nchar(saga_bin)) == saga_bin){
    exist<-TRUE
  }
  # append SAGA path to systempath
  if (!exist){
    Sys.setenv(PATH=paste0(saga_bin,";",saga_mod,";",Sys.getenv("PATH")))}
}else {
  exV("saga_bin",  "/usr/local/bin")
  exV("saga_mod","/usr/local/lib/saga")
  # check if already in path
  p<- Sys.getenv("PATH")
  if(substr(p, 1,nchar(saga_bin)) == saga_bin){
    exist<-TRUE
  }
  # append SAGA path to systempath
  if (!exist){
    Sys.setenv(PATH=paste0(saga_mod,":",saga_mod,":",Sys.getenv("PATH")))}
}

}
