# assigns a variable in .GlobalEnv 
makGlobalVar <- function(name,value) {
  if(!exists(name, envir = .GlobalEnv)) {
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } 
}