# ------------- GRASS utility function for raster to point conversion
# (GRASS)   raster to vector points 
ras2vecpoiGRASS <- function(fNinput,retSP=FALSE){
  # (GRASS) import
  rgrass7::execGRASS('r.import',  
                     flags=c('o',"overwrite","quiet"),
                     input=fNinput,
                     output="rt_treeNodes",
                     band=1
  )
  # (GRASS) raster to vector
  rgrass7::execGRASS('r.to.vect',  
                     flags=c('s',"overwrite","quiet"),
                     input="rt_treeNodes",
                     output="rt_treeNodes",
                     type="point",
                     column="Z")
  # (GRASS) export
  rgrass7::execGRASS('v.out.ogr',  
                     flags = c("overwrite","quiet"),
                     input = "rt_treeNodes",
                     output = paste0(tools::file_path_sans_ext(fNinput),".shp"),
                     format = "ESRI_Shapefile")
  treesR <- rgdal::readOGR(pd_gi_run,basename(tools::file_path_sans_ext(fNinput)))
  if (retRaster) return( treesR) 
}

