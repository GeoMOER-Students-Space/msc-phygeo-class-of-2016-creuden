# if NOT existing 
# assigns a variable in .GlobalEnv 
# 
makGlobalVar <- function(name,value) {
  if(exists(name, envir = .GlobalEnv)) {
    warning(paste0("The variable '", name,"' already exist in .GlobalEnv"))
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } else {
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } 
}