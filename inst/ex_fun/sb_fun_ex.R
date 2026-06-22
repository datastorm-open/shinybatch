somme <- function(x, y, error = FALSE){
  if(error){
    stop("error")
  }
  x + y
}

sb_fun_ex <- function(x, y, z, error = FALSE) {
  res <- somme(x, y, error = error)
  message('Running !')
  warning("Complex (or not) variable z is not used...!")
  res
}
