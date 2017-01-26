# if NOT existing 
# assigns a variable in .GlobalEnv 
# 
makGlobalVar <- function(name,value) {
  if(exists(name, envir = .GlobalEnv)) {
    #warning(paste0("The variable '", name,"' already exist in .GlobalEnv"))
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global .GlobalEnv\n")
  } else {
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global .GlobalEnv\n")
  } 
}