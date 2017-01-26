# ------------- GRASS utility function for raster to point conversion
# (GRASS)   raster to vector points 
gheatMapGRASS <- function(fNinput,retSP=FALSE,radius=50){
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
  
  # kernel density
  rgrass7::execGRASS('v.kernel',  
                     flags=c("quiet"),
                     input="rt_treeNodes",
                     output="rt_heatmap_trees",
                     radius=radius)
  
  
  rgrass7::execGRASS('r.out.gdal',  
                     flags = c("overwrite","quiet","c"),
                     input = "rt_heatmap_trees",
                     output = paste0(pd_gi_run,"rt_heatmap_trees_",as.character(radius),".tif"),
                     type="Float64")
 
  heatTrees <- rgdal::readOGR(pd_gi_run,"rt_cluster_trees")
  if (retSP) return( clustTrees) 
}

