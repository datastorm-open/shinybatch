call_rnorm <- function(n, mean, sd, error = FALSE){
  if(error){
    stop("error")
  }
  rnorm(n, mean, sd)
}

my_fun <- function(n, mean, sd, sleep, error = FALSE) {
  message("Waiting for ", sleep, " secondes")
  Sys.sleep(sleep)
  call_rnorm(n, mean, sd, error = error)
}