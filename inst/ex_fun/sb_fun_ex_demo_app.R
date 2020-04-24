my_fun <- function(n, mean, sd, sleep) {
  Sys.sleep(sleep)
  rnorm(n, mean, sd)
}