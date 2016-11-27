#' sagaModuleHelp
#'@description gi-ws-04-1 MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' get the  command line help for the requested module/algorithm combination using the defined saga_cmd  
#'  Usage: rawCmd<-sagaModuleHelp("name_of_SAGA_module","number_of_algorithm")
#'  example import gdal raster:  sagaModuleHelp("io_gdal","0")
#'
#'@return the help output from the currently linked SAGA binary for the requested module
#'
#'@param module name of the saga module
#'@param algorithm number or name of algorithm 
#'

sagaModuleHelp<- function(module,algorithm=NULL) {
  options(warn=-1)
  if (!is.null(algorithm)){  
    info<- system2('saga_cmd',paste(module,algorithm),stderr = TRUE)
  } else{
    info<-system2("saga_cmd",paste(module),stderr = TRUE)
  }
  options(warn=0)
  return(info)
}