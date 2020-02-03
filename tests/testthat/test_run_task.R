context("test_run_task")

# create temporary directory for conf
dir_conf <- tempdir()
# create temporary directory for fun
dir_fun <- tempdir()
con <- file(paste0(dir_fun, "/fun_script.R"))
writeLines("my_fun <- function(x, y, z) {
                        res <- x + y ;
                        warning('Just a warning') ;
                        res} ",
           con)
close(con)

# create and save conf
conf <- init_task_conf(dir_path = dir_conf,
                  conf_descr = list(title = "my_title",
                                    description = "my_descr"),
                  fun_path = paste0(dir_fun, "/fun_script.R"),
                  fun_name = "my_fun",
                  fun_args = list(x = 1,
                                  y = 0:4,
                                  z = iris),
                  priority = 1)


test_that("test outputs", {
  
  # fun output
  time <- Sys.time()
  expect_equal(suppressWarnings(run_task(paste0(attr(conf, "path"), "conf.yml"))), 1:5)
  
  # conf updates
  #####
  # 1 #
  #####
  expect_equal(yaml::read_yaml(paste0(attr(conf, "path"), "conf.yml"))$run_info$status, "finished")
  expect_equal(yaml::read_yaml(paste0(attr(conf, "path"), "conf.yml"))$run_info$date_end_run, as.character(time))
  
  #####
  # 2 #
  #####
  # create temporary directory for conf
  dir_conf <- tempdir()
  # create temporary directory for fun
  dir_fun <- tempdir()
  con <- file(paste0(dir_fun, "/fun_script.R"))
  writeLines("my_fun <- function(x, y, z) {
             res <- x + y ;
             stop('crap an error') ;
             res} ",
             con)
  close(con)
  
  # create and save conf
  conf <- init_task_conf(dir_path = dir_conf,
                    conf_descr = list(title = "my_title",
                                      description = "my_descr"),
                    fun_path = paste0(dir_fun, "/fun_script.R"),
                    fun_name = "my_fun",
                    fun_args = list(x = 1,
                                    y = 0:4,
                                    z = iris),
                    priority = 1)
  
  time <- Sys.time()
  try(run_task(paste0(attr(conf, "path"), "conf.yml")), silent = T)
  
  expect_equal(yaml::read_yaml(paste0(attr(conf, "path"), "conf.yml"))$run_info$status, "error")

})