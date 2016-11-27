#'@name initGrass4R '@title initializes enviroment for using \link{rgrass7} with GRASS 
#'@description Function that initializes environment and pathes for GRASS7x. '
#'Despite the GRASS GIS seup is performed by the initGRASS() funtion of the
#'\link{rgrass7} package, there are some workarounds necessary. 
#'While initGRASS works fine for known pathes and environmental varibles, one 
#'will find that the integration of Windows based GRASS especially as provided 
#'by OSGeo4W or the usage of parallel installations could be cumbersome. 
#'initGrass4R trys to find valid GRASS binaries by analyzing the initial GRASS script files.
#'If necessary it set the system variables and finally it initialize GRASS for R with user
#'provided  valid raster or sp object.\cr\cr 
#'*NOTE* If you have more than one valid installation you will be ask to select.
#'@details The concept is very straightforward but for an all days usage pretty helpful. 
#'You need to provide a \link{raster}/\link{sp} spatial object which is correct georeferenced
#'The resulting params will be used to initialize a temporary but static 
#'\href{https://cran.r-project.org/web/packages/rgrass7}{rgrass7}
#'environment. During the rsession you will have full access to
#'GRASS7 via the \link{rgrass7} wrapper package.
#'@usage initGrass4R() 
#'@param x raster or sp object 
#'@param setDefaultGrass default = NULL will force a search for GRASS You
#'may provide a valid combination as c("C:\\OSGeo4W64","grass-7.0.5","osgeo4w") 
#'@author Chris Reudenbach 
#'@return initGrass4R initializes the usage of GRASS7.
#'@export initGrass4R 
#'
#'@examples '####  GRASS bindings from R ' ' # get meuse
#' data(meuse) 
#' coordinates(meuse) <- ~x+y 
#' proj4string(meuse) <-CRS("+init=epsg:28992") 
#' 
#' # automatic search and find of GRASS binaries if 
#' # more than one you have to choose. 
#' initGrass4R(meuse) 
#' 
#' # assuming a typical standalone installation 
#' initGrass4R(meuse,c("C:\\Program Files\\GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS")) 
#' 
#' # assuming a typical OSGeo4W installation
#' initGrass4R(meuse,c("C:\\OSGeo4W64","grass-7.0.5","osgeo4W"))
#' 
#' # string for Linux c("/usr/bin","grass72") '

initGrass4R <- function(x=NULL,setDefaultGrass=NULL,SP = NULL){
  if (is.null(x)) {
    stop("no information from raster data neither rasterParam ")
  } else {
    type<-getSimpleClass(x)
    if (type=="rst") {
      resolution <- raster::res(x)[1]
      proj4 <- as.character(x@crs)
      ymax<-x@extent@ymax
      ymin<-x@extent@ymin
      xmax<-x@extent@xmax
      xmin<-x@extent@xmin
    } else {
      # i do not understand all this class stuff :-(
      s<-x@proj4string
      s<-s@projargs
      s2<-(strsplit(s,split = " "))
      proj4<- paste(s2[[1]][2:length(unlist(s2))], collapse = ' ')
      xmax<-x@bbox[3]
      xmin<-x@bbox[1]
      ymax<-x@bbox[4]
      ymin<-x@bbox[2]
      #resolution<-0.0008333333
     }
  }
  if(Sys.info()["sysname"] == "Windows"){
    if (is.null(SP)) SP<-"C:"
    grass.gis.base<- getGrassParams4W(setDefaultGrass,SP)
  } else {
    if (is.null(SP)) SP<-"/usr"
    grass.gis.base<- getGrassParams4X(setDefaultGrass,SP)
  }


#Sys.setenv(.GRASS_CACHE = paste(Sys.getenv("HOME"), "\\.grass_cache",sep = "")) 
#################### start with GRASS setup ------------------------------------
# create the TEMPORARY GRASS location
rgrass7::initGRASS(gisBase=grass.gis.base,
                   home=tempdir(),
                   mapset='PERMANENT',
                   override=TRUE
)

# assign GRASS projection according to data set
rgrass7::execGRASS('g.proj',
                   flags=c('c','quiet'),
                   proj4=proj4
)

# assign GRASS extent
if (type=="rst") {
  rgrass7::execGRASS('g.region',
                     flags=c('quiet'),
                     n=as.character(ymax),
                     s=as.character(ymin),
                     e=as.character(xmax),
                     w=as.character(xmin),
                     res=as.character(resolution)
                     )
  } else {
    rgrass7::execGRASS('g.region',
                       flags=c('quiet'),
                       n=as.character(ymax),
                       s=as.character(ymin),
                       e=as.character(xmax),
                       w=as.character(xmin)
                       #res=as.character(resolution)
    )
    }
return(rgrass7::gmeta())
}

#'@name getGrassParams4W
#'@title initializes enviroment for using \link{rgrass7} with GRASS 
#'@description Function that initializes environment and pathes for GRASS7 .*NOTE* you probably have to customize some settings according to your personal installation folders.
#'@details The concept is very straightforward but for an all days usage pretty helpful. You need to provide a GDAL conform raster file, a \link{raster} object or you may download SRTM data with \link{getGeoData}. This settings will be used to initialize a temporary but static \href{https://cran.r-project.org/web/packages/rgrass7}{rgrass7} environment. Additionally \href{https://cran.r-project.org/web/packages/RSAGA}{RSAGA} and \href{https://cran.r-project.org/web/packages/gdalUtils}{gdalUtils} are initialized and checked. During the rsession you will have full access to GRASS7 via the wrapper packages. .
#'@usage getGrassParams4W()
#'@param x raster or sp object 
#'@param setDefaultGrass default = NULL wil force a search for GRASS You may provide a valid combination as c("C:\\OSGeo4W64","grass-7.0.5","osgeo4w")
#'@return getGrassParams4W initializes the usage of GRASS7.
#'@export getGrassParams4W
#'
#'@examples
#'####  GRASS bindings from R
#' # automatic search and find of GRASS binaries if more than one you have to choose.
#' getGrassParams4W()
#' 
#' # assuming a typical standalone installation
#' getGrassParams4W(c("C:\\Program Files\\GRASS GIS 7.0.5","GRASS GIS 7.0.5","NSIS"))
#' 
#' # assuming a typical OSGeo4W installation
#' getGrassParams4W(c("C:\\OSGeo4W64","grass-7.0.5","osgeo4W"))
 
getGrassParams4W <- function(setDefaultGrass=NULL,DL="C:"){
  
  # (R) set pathes  of GRASS  binaries depending on OS WINDOWS
  if (is.null(setDefaultGrass)){
    
    # if no path is provided  we have to search
    grassParams<-searchOSgeo4WGrass(DL=DL)
    
    # if just one valid installation was found take it
    if (nrow(grassParams) == 1) {  
      grass.gis.base<-setGrassEnv4W(grassRoot=setDefaultGrass[1],grassVersion=setDefaultGrass[2],installationType = setDefaultGrass[3])
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(grassParams) > 1) {
      cat("You have more than one valid GRASS version\n")
      print(grassParams)
      cat("\n")
      ver<- as.numeric(readline(prompt = "Please choose one:  "))
      grass.gis.base<-setGrassEnv4W(grassRoot=grassParams$instDir[[ver]],
                                    grassVersion=grassParams$version[[ver]], 
                                    installationType = grassParams$installationType[[ver]] )
    }
    
    # if a setDefaultGrass was provided take this 
    } else {
      grass.gis.base<-setGrassEnv4W(grassRoot=setDefaultGrass[1],grassVersion=setDefaultGrass[2],installationType = setDefaultGrass[3])  
    }
  return(grass.gis.base)
}

#'@name searchOSgeo4WGrass
#'
#'@title search for valid GRASS installations on a given windows drive 
#'@description  provides a pretty good estimation of valid GRASS installations on your Windows system
#'@param DL drive letter default is "C:"
#'@return a dataframe with the GRASS root dir the Version name and the installation type
#'@author Chris Reudenbach
#'@export searchOSgeo4WGrass
#'
#'@examples
#'#### Examples how to use RSAGA and GRASS bindings from R
#'
#' # get all valid GRASS installation folders and params
#' grassParam<- searchOSgeo4WGrass()

searchOSgeo4WGrass <- function(DL = "C:"){
  # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
  # recursive dir for grass*.bat returns all version of grass bat files
  rawGRASS <- system(paste0("cmd.exe /c dir /B /S ", DL, "\\grass*.bat"), intern = T)
  
  # trys to identify valid grass installations and their version numbers
  grassInstallations <- lapply(seq(length(rawGRASS)), function(i){
    # convert codetable according to cmd.exe using type
    batchfileLines <- system(paste0("cmd.exe /c TYPE \"", rawGRASS[i], "\""), 
                             ignore.stdout = TRUE, intern = T)
    osgeo4w<-FALSE
    standAlone<-FALSE
    rootDir<-''
    
    # if the the tag "OSGEO4W" exists set installationType
    if (length(unique(grep(paste("OSGEO4W", collapse = "|"), batchfileLines, value = TRUE))) > 0){
      osgeo4w <- TRUE
      standAlone <- FALSE
    }
    # if the the tag "NSIS installer" exists set installationType
    if (length(unique(grep(paste("NSIS installer", collapse = "|"), batchfileLines, value = TRUE))) > 0){
      osgeo4w <- FALSE
      standAlone <- TRUE
    }
    
    ### if installationType is osgeo4w
    if (osgeo4w){
      # grep line with root directory and extract the substring defining GISBASE
      rootDir <- unique(grep(paste("SET OSGEO4W_ROOT=", collapse = "|"), batchfileLines, value = TRUE))
      if (length(rootDir) > 0) rootDir <- substr(rootDir, gregexpr(pattern = "=", rootDir)[[1]][1] + 1, nchar(rootDir))
      
      # grep line with the version name and extract it
      verChar <- unique(grep(paste("\\benv.bat\\b", collapse = "|"), batchfileLines,value = TRUE))
      if (length(rootDir) > 0){
        verChar <- substr(verChar, gregexpr(pattern = "\\grass-", verChar)[[1]][1], nchar(verChar))
        verChar <- substr(verChar, 1, gregexpr(pattern = "\\\\", verChar)[[1]][1]-1)
      }
      installerType <- "osgeo4W"
    }
    
    ### if installatationtype is standalone
    if (standAlone){
      # grep line containing GISBASE and extract the substring 
      rootDir <- unique(grep(paste("set GISBASE=", collapse = "|"), batchfileLines, value = TRUE))
      if (length(rootDir) > 0) rootDir <- substr(rootDir, gregexpr(pattern = "=", rootDir)[[1]][1] + 1, nchar(rootDir))
      verChar <- rootDir
      if (length(rootDir) > 0){
        verChar <- substr(verChar, gregexpr(pattern = "GRASS", verChar)[[1]][1], nchar(verChar))
      }
      installerType <- "NSIS"
    }
    
    # check if the the folder really exists
    if (length(rootDir) > 0){
      if (!file.exists(file.path(rootDir))){
        exist <- FALSE
      } else {
        exist <- TRUE
      } 
    } else {
      exist <- FALSE
    }
    
    # put the existing GISBASE directory, version number  and installation type in a data frame
    if (length(rootDir) > 0 & exist){
      data.frame(instDir = rootDir, version = verChar, installationType = installerType,stringsAsFactors = FALSE)
    }
  }) # end lapply
  
  # bind the df lines
  grassInstallations <- do.call("rbind", grassInstallations)
  
  return(grassInstallations)
}


#'@name getGrassParams4X
#'@title initializes enviroment for using \link{rgrass7} with GRASS 
#'@description Function that initializes environment and pathes for GRASS7 .*NOTE* you probably have to customize some settings according to your personal installation folders.
#'@details The concept is very straightforward but for an all days usage pretty helpful. You need to provide a GDAL conform raster file, a \link{raster} object or you may download SRTM data with \link{getGeoData}. This settings will be used to initialize a temporary but static \href{https://cran.r-project.org/web/packages/rgrass7}{rgrass7} environment. Additionally \href{https://cran.r-project.org/web/packages/RSAGA}{RSAGA} and \href{https://cran.r-project.org/web/packages/gdalUtils}{gdalUtils} are initialized and checked. During the rsession you will have full access to GRASS7 via the wrapper packages. .
#'@usage getGrassParams4X()
#'@param x raster or sp object 
#'@param setDefaultGrass default = NULL wil force a search for GRASS You may provide a valid combination as c("C:\\OSGeo4W64","grass-7.0.5","osgeo4w")
#'@return getGrassParams4X initializes the usage of GRASS7.
#'@export getGrassParams4X
#'
#'@examples
#'####  GRASS bindings from R
#' # automatic search and find of GRASS binaries if more than one you have to choose.
#' getGrassParams4X()
#' 
#' # assuming a typical standalone installation
#' getGrassParams4X("/usr/bin/grass72")
#' 
#' # assuming a typical user defined installation
#' getGrassParams4X("/usr/local/bin/grass72")

getGrassParams4X <- function(setDefaultGrass=NULL, MP = "/usr"){
  
  # (R) set pathes  of GRASS  binaries depending on OS WINDOWS
  if (is.null(setDefaultGrass)){
    
    # if no path is provided  we have to search
    grassParams<-searchXGrass(MP=MP)
    
    # if just one valid installation was found take it
    if (nrow(grassParams) == 1) {  
      grass.gis.base<-grassParams$instDir
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(grassParams) > 1) {
      cat("You have more than one valid GRASS version\n")
      print(grassParams)
      cat("\n")
      ver<- as.numeric(readline(prompt = "Please choose one:  "))
      grass.gis.base<-grassParams$instDir[[ver]]
    }
    
    # if a setDefaultGrass was provided take this 
  } else {
    grass.gis.base<-setDefaultGrass
  }
  return(grass.gis.base)
}

#'@name searchXGrass
#'
#'@title search for valid GRASS installations on a given windows drive 
#'@description  provides a pretty good estimation of valid GRASS installations on your Windows system
#'@param MP default is /usr
#'@return a dataframe the GRASS root dir the Version name and the installation type
#'@author Chris Reudenbach
#'@export searchXGrass
#'
#'@examples
#'#### Examples how to use RSAGA and GRASS bindings from R
#'
#' # get all valid GRASS installation folders in the home directory
#' grassParam<- searchXGrass("~/")

searchXGrass <- function(MP = "/usr"){
  rawGRASS<- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'grass??' -print"),stdout = TRUE)
  grassInstallations <- lapply(seq(length(rawGRASS)), function(i){
    # grep line containing GISBASE and extract the substring 
    rootDir<- grep(readLines(rawGRASS),pattern = 'isbase = "',value = TRUE)
    rootDir<- substr(rootDir, gregexpr(pattern = '"', rootDir)[[1]][1]+1, nchar(rootDir)-1)
    verChar<- grep(readLines(rawGRASS),pattern = 'grass_version = "',value = TRUE)
    verChar<- substr(verChar, gregexpr(pattern = '"', verChar)[[1]][1]+1, nchar(verChar)-1)
    cmd<- grep(readLines(rawGRASS),pattern = 'cmd_name = "',value = TRUE)
    cmd<- substr(cmd, gregexpr(pattern = '"', cmd)[[1]][1]+1, nchar(cmd)-1)
    
    # put it in data frame
    data.frame(instDir = rootDir, version = verChar, cmd = cmd , stringsAsFactors = FALSE)
  }) # end lapply
  
  # bind the df lines
  grassInstallations <- do.call("rbind", grassInstallations)
  return(grassInstallations)
  }




#'@name setGrassEnv4W
#'
#'@title  setGrassEnv4W set environ Params of GRASS
#'@description  during ar running rsession you will have full access to GRASS7 via the wrapper or command line packages
#'@param grassRoot  grass root directory i.e. "C:\\OSGEO4~1",
#'@param grassVersion grass version name i.e. "grass-7.0.5"
#'@param installationType two options "osgeo4w" and "NSIS"
#'@param jpgmem jpeg2000 memory allocation default is 1000000
#'@return set the whole enviroment and returns the gisbase directory for windows
#'@author Chris Reudenbach
#'
#'@export setGrassEnv4W
#'
#'@examples
#'#### Examples how to use RSAGA and GRASS bindings from R
#'
#' # get all valid GRASS installation folders and params
#' grassParam<- setGrassEnv4W()

setGrassEnv4W<- function(grassRoot="C:\\OSGEO4~1",grassVersion="grass-7.0.5",installationType="osgeo4w",jpgmem=1000000){
  
  #.GRASS_CACHE <- new.env(FALSE parent=globalenv())
  if (installationType == "osgeo4w"){
    Sys.setenv(OSGEO4W_ROOT=grassRoot)
    # define GISBASE
    grass.gis.base<-paste0(grassRoot,"\\apps\\grass\\",grassVersion)
    Sys.setenv(GISBASE=grass.gis.base,envir = .GlobalEnv)
    assign("SYS", "WinNat", envir=.GlobalEnv)
    assign("addEXE", ".exe", envir=.GlobalEnv)
    assign("WN_bat", "", envir=.GlobalEnv)
    assign("legacyExec", "windows", envir=.GlobalEnv)
    
    
    Sys.setenv(GRASS_PYTHON=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\bin\\python.exe"),envir = .GlobalEnv)
    Sys.setenv(PYTHONHOME=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\apps\\Python27"),envir = .GlobalEnv)
    Sys.setenv(PYTHONPATH=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\apps\\grass\\",grassVersion,"\\etc\\python"),envir = .GlobalEnv)
    Sys.setenv(GRASS_PROJSHARE=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\proj"),envir = .GlobalEnv)
    Sys.setenv(PROJ_LIB=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\proj"),envir = .GlobalEnv)
    Sys.setenv(GDAL_DATA=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\gdal"),envir = .GlobalEnv)
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    Sys.setenv(FONTCONFIG_FILE=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\etc\\fonts.conf"),envir = .GlobalEnv)
    Sys.setenv(JPEGMEM=jpgmem,envir = .GlobalEnv)
    Sys.setenv(FONTCONFIG_FILE=paste0(Sys.getenv("OSGEO4W_ROOT"),"\\bin\\gdalplugins"),envir = .GlobalEnv)
    Sys.setenv(GISRC = paste(Sys.getenv("HOME"), "\\.grassrc7",  sep = ""),envir = .GlobalEnv)
    
    # set path variable
    Sys.setenv(PATH=paste0(grass.gis.base,";",
                           grassRoot,"\\apps\\Python27\\lib\\site-packages\\numpy\\core",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\bin",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\lib",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\etc",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\etc\\python",";",
                           grassRoot,"\\apps\\Python27\\Scripts",";",
                           grassRoot,"\\bin",";",
                           grassRoot,"\\apps",";",
                           paste0(Sys.getenv("WINDIR"),"/WBem"),";",
                           Sys.getenv("PATH")),envir = .GlobalEnv)
    
    # get list of all tools
    system(paste0(grassRoot,"/bin/o-help.bat"))
    
  } 
  # for the NSIS windows installer versions
  else {
    
    Sys.setenv(GRASS_ROOT=grassRoot)
    # define GISBASE
    grass.gis.base<-grassRoot
    Sys.setenv(GISBASE=grass.gis.base,envir = .GlobalEnv)
    assign("SYS", "WinNat", envir=.GlobalEnv)
    assign("addEXE", ".exe", envir=.GlobalEnv)
    assign("WN_bat", "", envir=.GlobalEnv)
    assign("legacyExec", "windows", envir=.GlobalEnv)
    
    
    Sys.setenv(GRASS_PYTHON=paste0(Sys.getenv("GRASS_ROOT"),"\\bin\\python.exe"),envir = .GlobalEnv)
    Sys.setenv(PYTHONHOME=paste0(Sys.getenv("GRASS_ROOT"),"\\apps\\Python27"),envir = .GlobalEnv)
    Sys.setenv(PYTHONPATH=paste0(Sys.getenv("GRASS_ROOT"),"\\apps\\grass\\",grassVersion,"\\etc\\python"),envir = .GlobalEnv)
    Sys.setenv(GRASS_PROJSHARE=paste0(Sys.getenv("GRASS_ROOT"),"\\share\\proj"),envir = .GlobalEnv)
    Sys.setenv(PROJ_LIB=paste0(Sys.getenv("GRASS_ROOT"),"\\share\\proj"),envir = .GlobalEnv)
    Sys.setenv(GDAL_DATA=paste0(Sys.getenv("GRASS_ROOT"),"\\share\\gdal"),envir = .GlobalEnv)
    Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("GRASS_ROOT"),"\\share\\epsg_csv"),envir = .GlobalEnv)
    Sys.setenv(FONTCONFIG_FILE=paste0(Sys.getenv("GRASS_ROOT"),"\\etc\\fonts.conf"),envir = .GlobalEnv)
    Sys.setenv(JPEGMEM=jpgmem,envir = .GlobalEnv)
    Sys.setenv(FONTCONFIG_FILE=paste0(Sys.getenv("GRASS_ROOT"),"\\bin\\gdalplugins"),envir = .GlobalEnv)
    Sys.setenv(GISRC = paste(Sys.getenv("HOME"), "\\.grassrc7",  sep = ""),envir = .GlobalEnv)
    
    # set path variable
    Sys.setenv(PATH=paste0(grass.gis.base,";",
                           grassRoot,"\\apps\\Python27\\lib\\site-packages\\numpy\\core",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\bin",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\lib",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\etc",";",
                           grassRoot,"\\apps\\grass\\",grassVersion,"\\etc\\python",";",
                           grassRoot,"\\apps\\Python27\\Scripts",";",
                           grassRoot,"\\bin",";",
                           grassRoot,"\\apps",";",
                           paste0(Sys.getenv("WINDIR"),"/WBem"),";",
                           Sys.getenv("PATH")),envir = .GlobalEnv)
    
  }
  
  return(grass.gis.base)
}

###  raster or vector class
getSimpleClass <- function(obj) {
  if (class(obj) %in% c("RasterLayer", "RasterStack",
                        "RasterBrick", "Satellite",
                        "SpatialGridDataFrame",
                        "SpatialPixelsDataFrame")) "rst" else "vec"
}
