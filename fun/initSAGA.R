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
#'## call it for a default OSGeo4W64 oinstallation of SAGA
#'initSAGA(defaultSAGA=c("C:\\OSGeo4W64\\apps\\saga","C:\\OSGeo4W64\\apps\\saga\\modules"))
#'
#'

initSAGA <- function(defaultSAGA = NULL, DL = "C:", MP="/usr"){
  # (R) set pathes  of SAGA modules and binaries depending on OS  
  exist<-FALSE
  if(Sys.info()["sysname"] == "Windows"){
    if (!is.null(defaultSAGA)) defaultSAGA<-data.frame(bin=defaultSAGA[1],lib=defaultSAGA[2])
    if (is.null(defaultSAGA)) defaultSAGA<- searchSAGA(DL = DL) 
    if (nrow(defaultSAGA) > 1) {
      cat("\nmore than 1 SAGA installation found: \n")
      print(defaultSAGA,digits = 0)
      cat("\n I will use the first one: ")
      print(defaultSAGA[1,1],digits = 0)
      
      makGlobalVar("sagaCmd", paste0(defaultSAGA[1][1,],"saga_cmd.exe"))
      makGlobalVar("sagaPath", defaultSAGA[1][1,])
      makGlobalVar("sagaModPath",  defaultSAGA[2][1,])
      
      add2Path(defaultSAGA[1][1,])
      add2Path(defaultSAGA[2][1,])
      } else {
        makGlobalVar("sagaCmd", paste0(defaultSAGA[1],"saga_cmd.exe"))
        makGlobalVar("sagaPath", defaultSAGA[1])
        makGlobalVar("sagaModPath",  defaultSAGA[2])
        
        add2Path(defaultSAGA[1])
        add2Path(defaultSAGA[2])
          
      }
  } 
  # if Linux
  else {
    
    if (is.null(defaultSAGA)) {
      
      defaultSAGA[1]<- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'saga_cmd' -print"),stdout = TRUE)
      defaultSAGA[2]<-substr(defaultSAGA,1,nchar(defaultSAGA)-9)
      rawSAGALib<- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'libio_gdal.so' -print"),stdout = TRUE)
      defaultSAGA[3]<-substr(rawSAGALib,1,nchar(rawSAGALib)-14)
    }
    makGlobalVar("sagaCmd", defaultSAGA[1])
    makGlobalVar("sagaPath", defaultSAGA[2] )
    makGlobalVar("sagaModPath",  defaultSAGA[3])
    add2Path(defaultSAGA[2])
    add2Path(defaultSAGA[3])
                   
  }
}

#'@name searchSAGA
#'
#'@title search for valid SAGA installations on a given windows drive 
#'@description  provides a pretty good estimation of valid SAGA installations on your Windows system
#'@param DL drive letter default is "C:"
#'@return a dataframe with the SAGA root folder, the version name and the installation type
#'@author Chris Reudenbach
#'@export searchSAGA
#'
#'@examples
#'#### Examples how to use searchSAGA 
#'
#' # get all valid SAGA installation folders and params
#' sagaParams<- searchSAGA()

searchSAGA <- function(DL = "C:"){
  
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
    defaultSAGA <- shQuote(c("C:\\Program Files\\QGIS 2.14\\apps\\saga","C:\\Program Files\\QGIS 2.14\\apps\\saga\\modules"))
  } else {
    
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for otb*.bat returns all version of otb bat files
    cat("\nsearching for SAGA installations - this may take a while\n")
    cat('Alternatively you can provide a path like: \n')
    cat('c("C:\\OSGeo4W64\\apps\\saga","C:\\OSGeo4W64\\apps\\saga\\modules")\n')
    rawSAGA <- system(paste0("cmd.exe /c dir /B /S ",DL,"\\","saga_cmd.exe"),intern = TRUE)
    
    # trys to identify valid otb installations and their version numbers
    sagaPath <- lapply(seq(length(rawSAGA)), function(i){
      # convert codetable according to cmd.exe using type
      cmdfileLines <- rawSAGA[i]
      installerType<-""
      # if the the tag "OSGEO4W" exists set installationType
      if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), rawSAGA[i], value = TRUE))) > 0){
        rootDir<-unique(grep(paste("OSGeo4W64", collapse = "|"), rawSAGA[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "saga_cmd.exe", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "osgeo4w64SAGA"
      }    
      
      # if the the tag "OSGEO4W" exists set installationType
      else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), rawSAGA[i], value = TRUE))) > 0){
        rootDir<-unique(grep(paste("OSGeo4W", collapse = "|"), rawSAGA[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "saga_cmd.exe", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "osgeo4wSAGA"
      }
      # if the the tag "QGIS" exists set installationType
      else if (length(unique(grep(paste("QGIS", collapse = "|"), rawSAGA[i], value = TRUE))) > 0){
        rootDir<-unique(grep(paste("QGIS", collapse = "|"), rawSAGA[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "saga_cmd.exe", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "qgisSAGA"
      } else {
        rootDir<-unique(grep(paste("saga_", collapse = "|"), rawSAGA[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "saga_cmd.exe", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "UserSAGA"
      }
      
      # put the existing GISBASE directory, version number  and installation type in a data frame
      data.frame(binDir = rootDir, baseDir=installDir, installationType = installerType,stringsAsFactors = FALSE)
      
    }) # end lapply
    
    # bind the df lines
    sagaPath <- do.call("rbind", sagaPath)
  }
  return(sagaPath)
}

