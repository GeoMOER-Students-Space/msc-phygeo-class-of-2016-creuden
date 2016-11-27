# if NOT existing 
# assigns a variable in .GlobalEnv 
# 
makGlobalVar <- function(name,value) {
  if(!exists(name, envir = .GlobalEnv)) {
    assign(name, value, envir = .GlobalEnv, inherits = TRUE)
  } else {
    warning("One or more variables did alredy exist in .GlobalEnv  ")
  } 
}