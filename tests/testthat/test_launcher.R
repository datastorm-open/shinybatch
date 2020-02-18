context("test_launcher")

# create temporary directory for conf
dir_conf <- paste0(tempdir(), "/conf_launcher")
if(dir.exists(dir_conf)) unlink(dir_conf, recursive = TRUE)
dir.create(dir_conf, recursive = T)

# create temporary directory for fun
dir_fun <- paste0(tempdir(), "/fun_launcher")
if(dir.exists(dir_fun))  unlink(dir_fun, recursive = TRUE)
dir.create(dir_fun)

con <- file(paste0(dir_fun, "/fun_script.R"))
writeLines("my_fun <- function(x, y, z) {x + y}",
           con)
close(con)

# create 2 confs
conf_1 <- configure_task(dir_path = dir_conf,
                         conf_descr = list(title_1 = "my_title_1",
                                           description_1 = "my_descr_1"),
                         fun_path = paste0(dir_fun, "/fun_script.R"),
                         fun_name = "my_fun",
                         fun_args = list(x = 0,
                                         y = 0:4,
                                         z = iris),
                         priority = 1)

Sys.sleep(0.1)

conf_2 <- configure_task(dir_path = dir_conf,
                         conf_descr = list(title_2 = "my_title_2",
                                           description_2 = "my_descr_2"),
                         fun_path = paste0(dir_fun, "/fun_script.R"),
                         fun_name = "my_fun",
                         fun_args = list(x = 1,
                                         y = 0:4,
                                         z = iris),
                         priority = 2)


test_that("test outputs", {
  
  expect_equal(launcher(NULL),
               0)
  
  # launch highest priority
  expect_equal(launcher(dir_conf), 
               1)
  expect_equal(readRDS(paste0(conf_2$path, "output/res.RDS")),
               1:5)
  
  # launch last conf file
  expect_equal(launcher(dir_conf), 
               1)
  expect_equal(readRDS(paste0(conf_1$path, "output/res.RDS")), 
               0:4)
  
  # launch nothing
  expect_equal(launcher(dir_conf), 
               0)
  
})