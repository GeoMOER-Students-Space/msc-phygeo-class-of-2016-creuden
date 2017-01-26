# ------------- GRASS utility function for point clustering

gPointClust <- function(fNinput,retSP=FALSE,radius=50){
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
  # cluster identify clusters
  
  rgrass7::execGRASS('v.cluster',  
                     flags=c("2","b","overwrite","quiet"),
                     input="rt_treeNodes",
                     output="rt_cluster_trees",
                     min=radius,
                     layer="3",
                     method="density")
  

  rgrass7::execGRASS('v.out.ogr',  
                     flags = c("overwrite","quiet"),
                     input = "rt_cluster_trees",
                     output = paste0(pd_gi_run,"rt_cluster_trees_",as.character(radius),".shp"),
                     format = "ESRI_Shapefile")
 
  clustTrees <- rgdal::readOGR(pd_gi_run,"rt_cluster_trees")
  if (retSP) return( clustTrees) 
}

