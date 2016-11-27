#' sagaModuleCmd
#'@description gi-ws-04-1 MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' get the raw command line string from the saga_cmd call
#'
#' usage: rawCmd<-sagaModuleCmd("name_of_SAGA_module","number_of_algorithm")
#' example import gdal raster:  sagaModuleCmd("io_gdal","0")
#'
#'#'@return the raw command from the currently linked SAGA binary for the requested module
#'
#'@param module name of the saga module
#'@param algorithm number or name of algorithm 
#'



sagaModuleCmd<- function(module,algorithm) {
  options(warn=-1)
  t<- sagaModuleHelp(module,algorithm)
  options(warn=0)
  cmd<- unique (grep(paste("Usage:",collapse="|"), t, value=TRUE))
  cmd<- substr(cmd, which(strsplit(cmd, '')[[1]]==':')+2, nchar(cmd))
  return(cmd)
}