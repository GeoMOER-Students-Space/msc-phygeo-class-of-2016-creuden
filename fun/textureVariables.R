#' Calculate selected Texture parameters from clouds based on spectral properties
#' 
#' @param x A rasterLayer or a rasterStack containing different channels
#' where clouds are already masked
#' @param nrasters A vector of channels to use from x. Default =nlayers(x)
#' @param filter A vector of numbers indicating the environment sizes for which 
#' the textures are calculated
#' @param stats A string vector of parameters to be calculated.
#'  see \code{\link{glcm}}
#' @param n_grey Number of grey values. see \code{\link{glcm}}
#' @param parallel A logical value indicating whether parameters are calculated 
#' parallely or not
#' @param min_x for each channel the minimum value which can occur. If NULL then
#' the minimum value from the rasterLayer is used.
#' @param max_x for each channel the maximum value which can occur. If NULL then
#' the maximum value from the rasterLayer is used.
#' @return A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  
#' @details This functions fills the glcm function with standard settings used 
#' for the rainfall retrieval
#' @author Hanna Meyer
#' @note include correlation the possibility to use only one shift param set
#' # http://www.fp.ucalgary.ca/mhallbey/more_informaton.htm  
#' # Homogeneity is correlated with Contrast,  r = -0.80
#' # Homogeneity is correlated with Dissimilarity, r = -0.95
#' # GLCM Variance is correlated with Contrast,  r= 0.89
#' # GLCM Variance is correlated with Dissimilarity,  r= 0.91
#' # GLCM Variance is correlated with Homogeneity,  r= -0.83
#' # Entropy is correlated with ASM,  r= -0.87
#' # GLCM Mean and Correlation are more independent. For the same image,
#' # GLCM Mean shows  r< 0.1 with any of the other texture measures demonstrated in this tutorial.
#' # GLCM Correlation shows  r<0.5 with any other measure.
#' @export textureVariables
#' @examples 
#' 
#' ## example on how to calculate texture from a list of msg channels
#' 
#' #'# stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#'  
#' #calculate texture
#' result <- textureVariables(msg_example,nrasters=1:3,
#' stats=c("mean", "variance", "homogeneity"))
#' 
#' #plot the results from VIS0.6 channel:
#' plot(result$size_3$VIS0.6)
#' 
#' @seealso \code{\link{glcm}}

textureVariables <- function(x,
                             nrasters=1:nlayers(x),
                             filter=c(3),
                             stats=c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
                                     "second_moment", "correlation"),
                             shift=list(c(0,1), c(1,1), c(1,0),c(1,-1)),
                             parallel=TRUE,
                             n_grey = 8,
                             min_x=NULL,
                             max_x=NULL){
  require(glcm) 
  require(raster)
  if (parallel){
    require(doParallel)
    registerDoParallel(detectCores()-1)
  }
  
  
  #set values larger than the max/min value to the max/minvalue. 
  #Otherwise NA would be used
  if(!is.null(min_x)){
    if (length(nrasters)>1){
      for (i in nrasters){
        x[[i]]=reclassify(x[[i]], c(max_x[i],Inf,max_x[i]))
        x[[i]]=reclassify(x[[i]], c(-Inf,min_x[i],min_x[i]))
      }
    } else { # only one raster
      x=reclassify(x, c(max_x,Inf,max_x))
      x=reclassify(x, c(-Inf,min_x,min_x)) 
    }
  }
  
  
  glcm_filter<-list()
  for (j in 1:length(filter)){
    if (class (x)=="RasterStack"||class (x)=="RasterBrick"){  
      if (parallel){
        glcm_filter[[j]]<-foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%dopar%{
                                    glcm(x[[i]], 
                                         window = c(filter[j], filter[j]), 
                                         shift=shift,
                                         statistics=stats,n_grey=n_grey,
                                         min_x=min_x[i],max_x=max_x[i],
                                         na_opt="center")
                                  } 
      } else {
        glcm_filter[[j]]<-foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%do%{
                                    mask(glcm(x[[i]], 
                                              window = c(filter[j], filter[j]), 
                                              shift=shift,
                                              statistics=stats,n_grey=n_grey,
                                              min_x=min_x[i],max_x=max_x[i],
                                              na_opt="center"), x[[i]])
                                  }
      }
      names(glcm_filter[[j]])<-names(x)[nrasters]
    } else {
      glcm_filter[[j]]<-mask(glcm(x, window = c(filter[j], filter[j]), 
                                  shift=shift,
                                  statistics=stats,n_grey=n_grey,
                                  min_x=min_x,max_x=max_x,
                                  na_opt="center"), x)
    }   
  }
  names(glcm_filter)<-paste0("size_",filter)
  return(glcm_filter)
}



#' Calculate selected Texture parameters from clouds based on gray level properties
#' 
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out string pattern vor individual naming of the output file(s)
#' @param parameters.xyrad list with the x and y radius in pixel indicating the kernel sizes for which 
#' the textures are calculated
#' @param 
#' @param parameters.xyoff  vector containg the directional offsets. Valid combinations are: list(c(1,1),c(1,0),c(0,1),c(1,-1))
#' @param n_grey Number of grey values. 
#' @param parallel A logical value indicating whether parameters are calculated 
#' parallely or not
#' @param parameters.minmax   minimum/maximum gray value which can occur. 
#' @param parameters.nbbin number of gray level bins (classes)
#' @param texture type of filter "simple" "advanced" "higher"
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @return A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  
#' @details 
#' @author Chris Reudenbach
#' @note include correlation the possibility to use only one shift param set
#' # http://www.fp.ucalgary.ca/mhallbey/more_informaton.htm  
#' # Homogeneity is correlated with Contrast,  r = -0.80
#' # Homogeneity is correlated with Dissimilarity, r = -0.95
#' # GLCM Variance is correlated with Contrast,  r= 0.89
#' # GLCM Variance is correlated with Dissimilarity,  r= 0.91
#' # GLCM Variance is correlated with Homogeneity,  r= -0.83
#' # Entropy is correlated with ASM,  r= -0.87
#' # GLCM Mean and Correlation are more independent. For the same image,
#' # GLCM Mean shows  r< 0.1 with any of the other texture measures demonstrated in this tutorial.
#' # GLCM Correlation shows  r<0.5 with any other measure.
#' @export otbHaraTex
#' @examples 
#' 
#' otbHaraTex(input=paste0(pd_rs_aerial,"test.tif"),
#' texture="simple")



otbHaraTex<- function(input=NULL,
                      out="hara",
                      ram="8192",
                      parameters.xyrad=list(c(1,1)),
                      parameters.xyoff=list(c(1,1)),
                      parameters.minmax=c(0,255),
                      parameters.nbbin=128,
                      texture="advanced",
                      channel=NULL){
  
  directory<-dirname(input)
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    for (xyrad in parameters.xyrad) {
      for (xyoff in parameters.xyoff) {
        outName<-paste0(directory, "/",
                        "band_",
                        band,
                        "_",
                        out,
                        "_",
                        texture,
                        "_",
                        xyrad[1],
                        xyrad[2],
                        "_",
                        xyoff[1],
                        xyoff[2],
                        ".tif")
        
        command<-paste0(otbPath,"otbcli_HaralickTextureExtraction")
        command<-paste(command, " -in ", input)
        command<-paste(command, " -channel ", channel)
        command<-paste(command, " -out ", outName)
        command<-paste(command, " -ram ",ram)
        command<-paste(command, " -parameters.xrad ",xyrad[1])
        command<-paste(command, " -parameters.yrad ",xyrad[2])
        command<-paste(command, " -parameters.xoff ",xyoff[1])
        command<-paste(command, " -parameters.yoff ",xyoff[2])
        command<-paste(command, " -parameters.min ",parameters.minmax[1])
        command<-paste(command, " -parameters.max ",parameters.minmax[2])
        command<-paste(command, " -parameters.nbbin ",parameters.nbbin)
        command<-paste(command, " -texture ",texture)
        
        cat("\nexecute ", command[band],"\n")
        system(command[band]) 
        
        
      }
    }
  }
}

#' Calculates local statistics for a given kernel size
#' 
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out string pattern vor individual naming of the output file(s)
#' @param radius computational window in pixel
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @return list of geotiffs containing thelocal statistics for each channel 
#' @details 
#' @author Chris Reudenbach
#' @export otblocalStat
#' @examples 
#' 
#' otblocalStat(input=paste0(pd_rs_aerial,"test.tif"),radius=5)

otblocalStat<- function(input=NULL,
                        out="localStat",
                        ram="8192",
                        radius=3,
                        channel=NULL){
  
  directory<-dirname(input)
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    outName<-paste0(directory, "/",
                    "band_",
                    band,
                    "_",
                    out,
                    "_",
                    radius,
                    ".tif")
    
    command<-paste0(otbPath,"otbcli_LocalStatisticExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    command<-paste(command, " -radius ",radius)
    cat("\nexecute ", command[band],"\n")
    system(command[band]) 
    
  }
}


#' Calculates edges for a given kernel size
#' 
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out the output mono band image containing the edge features
#' @param filter the choice of edge detection method (gradient/sobel/touzi)
#' @param filter.touzi.xradius x radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param filter.touzi.yradius y radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @return list of geotiffs containing thelocal statistics for each channel 
#' @details 
#' @author Chris Reudenbach
#' @export otbedge
#' @examples 
#' 
#' otblocalStat(input=paste0(pd_rs_aerial,"test.tif"),radius=5)

otbedge<- function(input=NULL,
                   out="edge",
                   ram="8192",
                   filter="gradient",
                   filter.touzi.xradius=1,
                   filter.touzi.yradius=1,
                   channel=NULL){
  
  directory<-dirname(input)
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    outName<-paste0(directory, "/",
                    "band_",
                    band,
                    "_",
                    filter,
                    "_",
                    out,
                    ".tif")
    
    command<-paste0(otbPath,"otbcli_EdgeExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    if (filter == "touzi") {
      command<-paste(command, " -filter.touzi.xradius ", filter.touzi.xradius)
      command<-paste(command, " -filter.touzi.yradius ", filter.touzi.yradius)
    }
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    cat("\nexecute ", command[band],"\n")
    system(command[band]) 
  }
}


#' Calculates Gray scale morphological operations for a given kernel size
#' 
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out the output mono band image containing the edge features
#' @param filter the choice of the morphological operation (dilate/erode/opening/closing) (default value is dilate)
#' @param structype the choice of the structuring element type (ball/cross)
#' @param structype.ball.xradius x the ball structuring element X Radius (only if structype==ball)
#' @param structype.ball.yradius y the ball structuring element Y Radius (only if structype==ball)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @return list of geotiffs containing thelocal statistics for each channel 
#' @details 
#' @author Chris Reudenbach
#' @export otbgraymorpho
#' @examples 
#' 
#' otbgraymorpho(input=paste0(pd_rs_aerial,"test.tif"))

otbgraymorpho<- function(input=NULL,
                         out="edge",
                         ram="8192",
                         filter="dilate",
                         structype="ball",
                         structype.ball.xradius=5,
                         structype.ball.yradius=5,
                         channel=NULL){
  
  directory<-dirname(input)
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    outName<-paste0(directory, "/",
                    "band_",
                    band,
                    "_",
                    filter,
                    "_",
                    structype,
                    "_",
                    out,
                    ".tif")
    
    command<-paste0(otbPath,"otbcli_GrayScaleMorphologicalOperation")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    if (structype == "ball") {
      command<-paste(command, " -structype.ball.xradius ", structype.ball.xradius)
      command<-paste(command, " -structype.ball.yradius ", structype.ball.yradius)
    }
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    cat("\nexecute ", command[band],"\n")
    system(command[band]) 
  }
}