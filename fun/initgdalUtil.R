# gi-ws-05-1
#' @description  MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' initGIS check and initializes gdal binaries
#'@return 
#' a list of the complete capabilities of the current installed gdal version
#' 
#'@example 
#'
#' # get all available driver 
#' gdal<- initgdalUtils()
#' 
#' gdal[[1]]$drivers$format_code
#' 
#' # GET BINARY PATH 
#' gdal[[1]]$path
#' 
#' # get additional and available python tools
#' gdal[[1]]$python_utilities
#' 
#' 
#'
initgdalUtils <- function(searchPath=NULL){
  if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
    gdalUtils::gdal_setInstallation(search_path = shQuote("C:/Program Files/QGIS 2.14/bin/"))
  } else {
    if (is.null(searchPath)) {
    cat("\nsearching for GDAL binaries - this will take a while\n")
    cat("Alternatively you can provide a searchPath like: 'C:\\OSGeo4W64\\bin\\'\n")
  ## (gdalUtils) check for a valid GDAL binary installation on your system
  gdalUtils::gdal_setInstallation()
    } else {
      gdalUtils::gdal_setInstallation(search_path = shQuote(searchPath))
    }
  }
  valid.install<-!is.null(getOption("gdalUtils_gdalPath"))
  if (!valid.install){
    stop('no valid GDAL/OGR found')
  }else {
    cat("GDAL version: ",getOption("gdalUtils_gdalPath")[[1]]$version)
    gdal<-getOption("gdalUtils_gdalPath")
  }
  
  # make the path available for System calls
  makGlobalVar("gdalPath", gdal[[1]]$path)
  
  # add to the beginning of the sessions PATH
  add2Path(gdal[[1]]$path)
  
  
  # return all gdalUtilSettings
  return(gdal)
  
}
