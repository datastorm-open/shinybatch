context("test_run_order")


test_that("test outputs", {
  
  # check date
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  Sys.sleep(1)
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  expect_equal(run_order(list(conf_1, conf_2)), 
               c(1, 2))
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  Sys.sleep(1)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  expect_equal(run_order(list(conf_1, conf_2)), 
               c(2, 1))
  
  # check priority
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  Sys.sleep(1)
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 2)
  expect_equal(run_order(list(conf_1, conf_2)), 
               c(2, 1))
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 2)
  Sys.sleep(1)
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  expect_equal(run_order(list(conf_1, conf_2)), 
               c(1, 2))
  
  # check ignore_status (all except waiting)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 2)
  conf_1$run_info$status <- "error"
  Sys.sleep(1)
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  expect_equal(run_order(list(conf_1, conf_2)), 
               c(2, 1))
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 2)
  conf_2$run_info$status <- "error"
  Sys.sleep(1)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  expect_equal(run_order(list(conf_1, conf_2)), 
               c(1, 2))
  
  # check ignore_status (all except waiting and error)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 2)
  conf_1$run_info$status <- "error"
  Sys.sleep(1)
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  expect_equal(run_order(list(conf_1, conf_2),
                         ignore_status = c("running", "finished")), 
               c(1, 2))
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 2)
  conf_2$run_info$status <- "error"
  Sys.sleep(1)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  expect_equal(run_order(list(conf_1, conf_2),
                         ignore_status = c("running", "finished")), 
               c(2, 1))
  
  # check delay_reruns (T)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  conf_1$run_info$status <- "error"
  Sys.sleep(1)
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  Sys.sleep(1)
  conf_1$run_info$date_start <- as.character(Sys.time())
  expect_equal(run_order(list(conf_1, conf_2),
                         ignore_status = c("running", "finished"),
                         delay_reruns = T), 
               c(2, 1))
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  conf_2$run_info$status <- "error"
  Sys.sleep(1)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  Sys.sleep(1)
  conf_2$run_info$date_start <- as.character(Sys.time())
  expect_equal(run_order(list(conf_1, conf_2),
                         ignore_status = c("running", "finished"),
                         delay_reruns = T), 
               c(1, 2))
  
  # check delay_reruns (F)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  conf_1$run_info$status <- "error"
  Sys.sleep(1)
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  Sys.sleep(1)
  conf_1$run_info$date_start <- as.character(Sys.time())
  expect_equal(run_order(list(conf_1, conf_2),
                         ignore_status = c("running", "finished"),
                         delay_reruns = F), 
               c(1, 2))
  conf_2 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  conf_2$run_info$status <- "error"
  Sys.sleep(1)
  conf_1 <- configure_task(dir_path = tempdir(),
                           fun_path = tempdir(), fun_name = "fun_name", fun_args = list(arg_1 = NA),
                           priority = 1)
  Sys.sleep(1)
  conf_2$run_info$date_start <- as.character(Sys.time())
  expect_equal(run_order(list(conf_1, conf_2),
                         ignore_status = c("running", "finished"),
                         delay_reruns = F), 
               c(2, 1))
  
})