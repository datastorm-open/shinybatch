context("test_launcher")

# create temporary directory for conf
dir_conf <- paste0(tempdir(), "/conf_launcher")
if (dir.exists(dir_conf)) unlink(dir_conf, recursive = TRUE)
dir.create(dir_conf, recursive = T)

# ex fun
fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch")
fun_name = "sb_fun_ex"

# create 2 confs
conf_1 <- configure_task(dir_path = dir_conf,
                         conf_descr = list(title_1 = "my_title_1",
                                           description_1 = "my_descr_1"),
                         fun_path = fun_path,
                         fun_name = fun_name,
                         fun_args = list(x = 0,
                                         y = 0:4,
                                         z = iris),
                         priority = 1)

Sys.sleep(2)

conf_2 <- configure_task(dir_path = dir_conf,
                         conf_descr = list(title_2 = "my_title_2",
                                           description_2 = "my_descr_2"),
                         fun_path = fun_path,
                         fun_name = fun_name,
                         fun_args = list(x = 1,
                                         y = 0:4,
                                         z = iris),
                         priority = 2)


test_that("test outputs", {

  expect_error(launcher(NULL))

  # launch highest priority
  expect_equal(launcher(dir_conf), 1)

  Sys.sleep(2) # time to launch the batch script

  if(file.exists(paste0(conf_2$dir, "output/res.RDS"))){
    expect_equal(readRDS(paste0(conf_2$dir, "output/res.RDS")),
                 1:5)
    expect_equal(yaml::read_yaml(paste0(conf_1$dir, "/conf.yml"))$run_info$status,
                 "waiting")
    expect_equal(yaml::read_yaml(paste0(conf_2$dir, "/conf.yml"))$run_info$status,
                 "finished")
  }

  # launch last conf file
  expect_equal(launcher(dir_conf),
               1)

  Sys.sleep(2) # time to launch the batch script

  if(file.exists(paste0(conf_1$dir, "output/res.RDS"))){
    expect_equal(readRDS(paste0(conf_1$dir, "output/res.RDS")),
                 0:4)
    expect_equal(yaml::read_yaml(paste0(conf_1$dir, "/conf.yml"))$run_info$status,
                 "finished")

  }
  # launch nothing
  expect_equal(launcher(dir_conf),
               0)

  # launch 2 confs
  # create temporary directory for conf
  unlink(tempdir(), recursive = TRUE)
  dir.create(dir_conf, recursive = T)

  # ex fun
  fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch")
  fun_name = "sb_fun_ex"

  # create 2 confs
  conf_1 <- configure_task(dir_path = dir_conf,
                           conf_descr = list(title_1 = "my_title_1",
                                             description_1 = "my_descr_1"),
                           fun_path = fun_path,
                           fun_name = fun_name,
                           fun_args = list(x = 0,
                                           y = 0:4,
                                           z = iris),
                           priority = 1)

  Sys.sleep(2)

  conf_2 <- configure_task(dir_path = dir_conf,
                           conf_descr = list(title_2 = "my_title_2",
                                             description_2 = "my_descr_2"),
                           fun_path = fun_path,
                           fun_name = fun_name,
                           fun_args = list(x = 1,
                                           y = 0:4,
                                           z = iris),
                           priority = 2)

  expect_equal(launcher(dir_conf,
                        max_runs = 2),
               2)

  Sys.sleep(2) # time to launch the batch script

  if(file.exists(paste0(conf_1$dir, "output/res.RDS"))){
    expect_equal(readRDS(paste0(conf_1$dir, "output/res.RDS")),
                 0:4)
    expect_equal(yaml::read_yaml(paste0(conf_1$dir, "/conf.yml"))$run_info$status,
                 "finished")
  }

  if(file.exists(paste0(conf_2$dir, "output/res.RDS"))){
    expect_equal(readRDS(paste0(conf_2$dir, "output/res.RDS")),
                 1:5)
    expect_equal(yaml::read_yaml(paste0(conf_2$dir, "/conf.yml"))$run_info$status,
                 "finished")
  }

})
