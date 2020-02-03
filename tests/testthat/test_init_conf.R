context("test_init_task_conf")

# create temporary directory
dir <- tempdir()

# create and save con
time <- Sys.time()
conf <- init_task_conf(dir_path = dir,
                  conf_descr = list(title = "my_title",
                                    description = "my_descr"),
                  fun_path = "my_fun_path",
                  fun_name = "my_fun_name",
                  fun_args = list(x = 1,
                                  y = 0:4,
                                  z = iris),
                  priority = 1)


test_that("test outputs", {
  # output files
  conf_run_info <- conf$run_info
  conf_run_info$date_init <- NULL
  expect_equal(conf_run_info, list(
    "date_start_run" = "N/A",
    "date_end_run" = "N/A",
    "priority" = 1,
    "status" = "waiting")
  )
  
  expect_equal(conf$descriptive, list(title = "my_title", description = "my_descr"))
  expect_equal(conf$`function`, list(
    "path" = "my_fun_path",
    "name" = "my_fun_name")
  )
  
  expect_equal(c("x", "y", "z"), names(conf$args))
  
  expect_equal(list.files(attr(conf, "path")), c("conf.yml", "inputs"))
  
  expect_equal(readRDS(paste0(attr(conf, "path"), "inputs/y.RDS")), 0:4)
  
  expect_equal(readRDS(paste0(attr(conf, "path"), "inputs/z.RDS")), iris)
  
})