context("test_init_conf")

# create temporary directory
dir <- tempdir()

# create and save conf
time <- Sys.time()
conf <- init_conf(conf_path = dir,
                  conf_descr = list(title = "my_title",
                                    description = "my_descr"),
                  fun_path = "my_fun_path",
                  fun_name = "my_fun_name",
                  fun_args = list(x = 1,
                                  y = 0:4,
                                  z = iris),
                  priority = 1)


test_that("test outputs", {
  
  # conf file fields
  expected_conf <- list(
    "run_info" = list(
      "date_init" = as.character(time),
      "date_start_run" = "N/A",
      "date_end_run" = "N/A",
      "priority" = 1,
      "status" = "waiting"),
    "descriptive" = list(title = "my_title",
                         description = "my_descr"),
    "function" = list(
      "path" = "my_fun_path",
      "name" = "my_fun_name"),
    "args" = list(
      x = 1,
      y =  list("_path" = paste0(dir, "/", gsub(" ", "_", gsub("-|:", "", time)), "/inputs/y.RDS")),
      z =  list("_path" = paste0(dir, "/", gsub(" ", "_", gsub("-|:", "", time)), "/inputs/z.RDS"))))
    attr(expected_conf, "path") <- paste0(dir, "/", gsub(" ", "_", gsub("-|:", "", time)), "/")
  
  # output files
  expect_equal(conf, expected_conf)
  
  expect_equal(list.files(attr(conf, "path")), c("conf.yml", "inputs"))
  
  expect_equal(readRDS(paste0(attr(conf, "path"), "inputs/y.RDS")), 0:4)
  
  expect_equal(readRDS(paste0(attr(conf, "path"), "inputs/z.RDS")), iris)
  
})