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

initOTB <- function(defaultOtb = "C:\\OSGeo4W64\\bin"){
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
    defaultOtb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
  }
  
  # (R) set pathes  of otb modules and binaries depending on OS  
  exist<-FALSE
  if(Sys.info()["sysname"] == "Windows"){
    makGlobalVar("otbPath", paste0(defaultOtb,"\\"))
    add2Path(defaultOtb)
  } else {
    makGlobalVar("otbPath", "(usr/bin/")
   }
}
