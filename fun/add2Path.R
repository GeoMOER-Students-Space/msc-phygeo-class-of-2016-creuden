add2Path<- function(newPath) {
  exist<-FALSE
  if(Sys.info()["sysname"] == "Windows"){
    del<-";"  
  } else {
    del<-":"  
  } 
  p<- Sys.getenv("PATH")
  if(substr(p, 1,nchar(newPath)) == newPath){
    exist<-TRUE
  }
  # if not exist append path to systempath
  if (!exist){
    Sys.setenv(PATH=paste0(newPath,del,Sys.getenv("PATH")))
  }
}