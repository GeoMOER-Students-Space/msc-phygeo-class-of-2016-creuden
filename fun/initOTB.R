# rs-ws-07
#
# MOC - Advanced GIS/Remote Sensing (T. Nauss, C. Reudenbach)
#' 
#'@name getOTB
#'@title getOTB setup the orfeo toolbox bindings for an rsession
#'@description  getOTB trys to find all valid OTB installation and returns the pathes and environment settings 
#'@param defaultOTBPath string contains path to otb binaries
#'@param DL hard drive letter
#'@author CR
#'@return 
#' add otb pathes to the enviroment and creates global variables otbPath
#' 
#'@export initOTB
#'
#'@example 
#'
#'## call it for a default OSGeo4W oinstallation of SAGA
#'initOTB("C:\\OSGeo4W64\\bin\\")
#'
#'


initOTB <- function(defaultOTBPath=NULL,installationRoot= NULL, otbType=NULL,DL="C:"){
  
  # (R) set pathes  of OTB  binaries depending on OS WINDOWS
  if (is.null(defaultOTBPath)){
    
    # if no path is provided  we have to search
    otbParams<-searchOSgeo4WOTB(DL=DL)
    
    # if just one valid installation was found take it
    if (nrow(otbParams) == 1) {  
      otbPath<-setOtbEnv(defaultOtb=otbParams$binDir[1],installationRoot=otbParams$baseDir[2])
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(otbParams) > 1) {
      cat("You have more than one valid OTB version\n")
      #print("installation folder: ",otbParams$baseDir,"\ninstallation type: ",otbParams$installationType,"\n")
      print(otbParams[1],right = FALSE,row.names = TRUE) 
      if (is.null(otbType)) {
        ver<- as.numeric(readline(prompt = "Please choose one:  "))
        otbPath<-setOTBEnv(defaultOtb=otbParams$binDir[[ver]],installationRoot = otbParams$baseDir[[ver]])
      } else {
        otbPath<-setOTBEnv(defaultOtb=otbParams[otbParams["installationType"]==otbType][1],installationRoot = otbParams[otbParams["installationType"]==otbType][2])
      }
    }
    
    # if a setDefaultOTB was provided take this 
  } else {
    otbPath<-setOTBEnv(defaultOTBPath,installationRoot)  
  }
  return(otbPath)
}


#'@name setOTBEnv
#'
#'@title  setOTBEnv set environ Params of OTB
#'@description  during a rsession you will have full access to OTB via the command line 
#'
#'@param otbPath string contains path to otb binaries
#'@param sagaCmd string contains the full string to call otb launcher
#'
#'@return 
#' add otb pathes to the enviroment and creates global variables otbCmd
#' 
#'@export setOTBEnv
#'
#'@example 
#'
#'## call it for a default OSGeo4W64 oinstallation of SAGA
#'setOTBEnv()
#'
#'

setOTBEnv <- function(defaultOtb = "C:\\OSGeo4W64\\bin",installationRoot="C:\\OSGeo4W64"){
  
  if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
    defaultOtb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    installationRoot <- shQuote("C:\\Program Files\\QGIS 2.14")
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    
  } else {
  
  # (R) set pathes  of otb modules and binaries depending on OS  
  
  if(Sys.info()["sysname"] == "Windows"){
    
    makGlobalVar("otbPath", defaultOtb)
    add2Path(defaultOtb)
    Sys.setenv(OSGEO4W_ROOT=installationRoot)
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    
  } else {
    makGlobalVar("otbPath", "(usr/bin/")
  }
  }
  return(defaultOtb)
}

#'@name searchOSgeo4WOTB
#'
#'@title search for valid OTB installations on a given windows drive 
#'@description  provides a pretty good estimation of valid OTB installations on your Windows system
#'@param DL drive letter default is "C:"
#'@return a dataframe with the OTB root dir the Version name and the installation type
#'@author Chris Reudenbach
#'@export searchOSgeo4WOTB
#'
  #'@examples
  #'#### Examples how to use RSAGA and OTB bindings from R
  #'
  #' # get all valid OTB installation folders and params
  #' otbParam<- searchOSgeo4WOTB()
  
  searchOSgeo4WOTB <- function(DL = "C:"){
    
    
    if (substr(Sys.getenv("COMPUTERNAME"),1,5)=="PCRZP") {
      defaultOtb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
      otbInstallations<- data.frame(instDir = shQuote("C:\\Program Files\\QGIS 2.14\\bin"), installationType = "osgeo4wOTB",stringsAsFactors = FALSE)
      Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
      
    } else {
    
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for otb*.bat returns all version of otb bat files
      cat("\nsearching for OTB installations - this may take a while\n")
      cat("Alternatively you can provide a path like: C:\\OSGeo4W64\\bin\\\n")
      cat("You can also provide a installation type like: 'osgeo4w64OTB'\n")
      rawOTB <- system(paste0("cmd.exe /c dir /B /S ",DL,"\\","otbcli.bat"),intern = TRUE)
    
    # trys to identify valid otb installations and their version numbers
    otbInstallations <- lapply(seq(length(rawOTB)), function(i){
      # convert codetable according to cmd.exe using type
      batchfileLines <- rawOTB[i]
      installerType<-""
      # if the the tag "OSGEO4W" exists set installationType
      if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), rawOTB[i], value = TRUE))) > 0){
        rootDir<-unique(grep(paste("OSGeo4W64", collapse = "|"), rawOTB[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "otbcli.bat", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "osgeo4w64OTB"
      }    
      
      # if the the tag "OSGEO4W" exists set installationType
      else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), rawOTB[i], value = TRUE))) > 0){
        rootDir<-unique(grep(paste("OSGeo4W", collapse = "|"), rawOTB[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "otbcli.bat", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "osgeo4wOTB"
      }
      # if the the tag "QGIS" exists set installationType
      else if (length(unique(grep(paste("QGIS", collapse = "|"), batchfileLines, value = TRUE))) > 0){
        rootDir<-unique(grep(paste("QGIS", collapse = "|"), rawOTB[i], value = TRUE))
        rootDir<- substr(rootDir,1, gregexpr(pattern = "otbcli.bat", rootDir)[[1]][1] - 1)
        installDir<-substr(rootDir,1, gregexpr(pattern = "bin", rootDir)[[1]][1] - 2)
        installerType<- "qgisOTB"
      }
      
      # put the existing GISBASE directory, version number  and installation type in a data frame
        data.frame(binDir = rootDir, baseDir=installDir, installationType = installerType,stringsAsFactors = FALSE)

    }) # end lapply
    
    # bind the df lines
    otbInstallations <- do.call("rbind", otbInstallations)
    }
    return(otbInstallations)
  }
  
