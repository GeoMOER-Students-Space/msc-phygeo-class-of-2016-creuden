# gis function
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
# postclassification of tree crown areas 
# returns crown polygons and tree position as derived by the centroids
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden

classifyTreeCrown <- function(crownFn,segType="2", 
                              funNames = c("eccentricityboundingbox","solidity"),
                              thChmAltitude = 5, 
                              crownMinArea = 3, 
                              crownMaxArea =150, 
                              solidity = 1, 
                              thWidthLengthRatio = 0.5) {
  # read crown vector data set
  crownarea <- rgdal::readOGR(dirname(crownFn),tools::file_path_sans_ext(basename(crownFn)), verbose = FALSE)
  crownarea@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # calculate area
  crownarea@data$area <- rgeos::gArea(crownarea,byid = TRUE)
  # filter for min, tree height and min max crown area
  crownarea <-  crownarea[crownarea@data$crownsHeigh >= thChmAltitude ,]
  crownarea <- crownarea[crownarea@data$area > crownMinArea & 
                           crownarea@data$area < crownMaxArea,]
  # calculate more metrics
  crownarea <- caMetrics(crownarea,funNames = funNames)
  #  filter for solidity and WL ratio
  crowns <- crownarea[as.numeric(crownarea@data$solidity) != solidity &
                        as.numeric(crownarea@data$eccentricityboundingbox) > thWidthLengthRatio ,]
  # calculate centroids as synthetic trees and ass all knoledge from the crowns
  sT <- rgeos::gCentroid(crowns,byid = TRUE)
  crowns@data$xcoord <- sT@coords[,1]
  crowns@data$ycoord <- sT@coords[,2]
  centerTrees <- crowns@data
  sp::coordinates(centerTrees) <- ~xcoord+ycoord
  sp::proj4string(centerTrees) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # save centerTrees and crowns as shapefile
  rgdal::writeOGR(obj = centerTrees,
                  layer = paste0("cTr_",segType), 
                  driver = "ESRI Shapefile", 
                  dsn = pd_gi_run, 
                  overwrite_layer = TRUE)
  rgdal::writeOGR(obj = crowns,
                  layer = paste0("cro_",segType), 
                  driver = "ESRI Shapefile", 
                  dsn = pd_gi_run, 
                  overwrite_layer = TRUE)
  return(list(centerTrees,crowns))
}