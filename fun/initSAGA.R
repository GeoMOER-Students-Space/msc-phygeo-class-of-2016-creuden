# gi-ws-05-1
#' @description  MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' initSAGA defines external SAGA binaries 
#'
#'@param sagaPath string contains path to SAGA binaries
#'@param sagaModPath string contains path to SAGA modules
#'@param sagaCmd string contains the full string to call saga_cmd
#'
#'@return 
#' add saga pathes to the enviroment and creates global variables sagaPath, sagaModPath and sagaCmd
#' 
#'@export initSAGA
#'
#'@example 
#'
#'## call it for a default OSGeo4W oinstallation of SAGA
#'initSAGA()
#'
#'

initSAGA <- function(defaultSAGA = c("C:\\OSGeo4W\\apps\\saga","C:\\OSGeo4W\\apps\\saga")){

# (R) set pathes  of SAGA modules and binaries depending on OS  
exist<-FALSE
if(Sys.info()["sysname"] == "Windows"){
  makGlobalVar("sagaCmd", paste0(defaultSAGA[1],"/saga_cmd.exe"))
  makGlobalVar("sagaPath", defaultSAGA[1])
  makGlobalVar("sagaModPath",  defaultSAGA[2])
  # check if already in system path
  p<- Sys.getenv("PATH")
  if(substr(p, 1,nchar(sagaPath)) == sagaPath){
    exist<-TRUE
  }
  # if NOT already done append SAGA path to systempath
  if (!exist){
    Sys.setenv(PATH=paste0(sagaPath,";",sagaModPath,";",Sys.getenv("PATH")))}
}else {
  if (substr(defaultSAGA[1],2,2) == ":") {
    makGlobalVar("sagaCmd", "/usr/local/bin/saga_cmd")
    makGlobalVar("sagaPath", "/usr/local/bin")
    makGlobalVar("sagaModPath","/usr/local/lib/saga")
  } else {
  makGlobalVar("sagaCmd", paste0(defaultSAGA[1],"/saga_cmd"))
  makGlobalVar("sagaPath", defaultSAGA[1])
  makGlobalVar("sagaModPath",  defaultSAGA[2])
}
  # check if already in path
  p<- Sys.getenv("PATH")
  if(substr(p, 1,nchar(sagaPath)) == sagaPath){
    exist<-TRUE
  }
  # append SAGA path to systempath
  if (!exist){
    Sys.setenv(PATH=paste0(sagaPath,":",sagaModPath,":",Sys.getenv("PATH")))}
}

}
