# gi-ws-06-1
#' @description  MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' getOTB defines external orfeo toolbox bindings 
#'
#'@param otbPath string contains path to otb binaries
#'@param sagaCmd string contains the full string to call otb launcher
#'
#'@return 
#' add otb pathes to the enviroment and creates global variables otbCmd
#' 
#'@export initOTB
#'
#'@example 
#'
#'## call it for a default OSGeo4W oinstallation of SAGA
#'initOTB()
#'
#'

initOTB <- function(defaultOtb = "C:\\OSGeo4W64\\bin",installationRoot="C:\\OSGeo4W64"){
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
    defaultOtb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    
  }
  
  # (R) set pathes  of otb modules and binaries depending on OS  
  exist<-FALSE
  if(Sys.info()["sysname"] == "Windows"){
    makGlobalVar("otbPath", paste0(defaultOtb,"\\"))
    add2Path(defaultOtb)

    Sys.setenv(OSGEO4W_ROOT=installationRoot)
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    
  } else {
    makGlobalVar("otbPath", "(usr/bin/")
   }
}
