context("test_run_task_handler")

make_fun_script <- function(dir, body_lines) {
  path <- file.path(dir, "fun_script.R")
  writeLines(c("test_fun <- function(x) {", body_lines, "}"), con = path)
  path
}

test_that("error is logged as FATAL with traceback, status set to error, no propagation", {
  dir_conf <- file.path(tempdir(), "handler_test_error")
  unlink(dir_conf, recursive = TRUE)
  dir.create(dir_conf, recursive = TRUE)

  fun_path <- make_fun_script(dir_conf, c(
    "  inner <- function() stop('deep error')",
    "  inner()"
  ))

  conf <- configure_task(
    dir_path = dir_conf,
    conf_descr = list(title = "test"),
    fun_path = fun_path,
    fun_name = "test_fun",
    fun_args = list(x = 1),
    priority = 1
  )

  result <- run_task(paste0(conf$dir, "conf.yml"))

  expect_null(result)
  expect_equal(yaml::read_yaml(paste0(conf$dir, "conf.yml"))$run_info$status, "error")

  log_file <- list.files(paste0(conf$dir, "output"), pattern = "log_run", full.names = TRUE)
  expect_length(log_file, 1)
  log_lines <- readLines(log_file)

  expect_true(any(grepl("FATAL", log_lines)))
  expect_true(any(grepl("deep error", log_lines)))
  expect_true(any(grepl("Traceback", log_lines)))
  expect_true(any(grepl("inner", log_lines)))
})

test_that("warning is logged as WARN and task completes as finished", {
  dir_conf <- file.path(tempdir(), "handler_test_warn")
  unlink(dir_conf, recursive = TRUE)
  dir.create(dir_conf, recursive = TRUE)

  fun_path <- make_fun_script(dir_conf, c(
    "  warning('test warning')",
    "  42L"
  ))

  conf <- configure_task(
    dir_path = dir_conf,
    conf_descr = list(title = "test"),
    fun_path = fun_path,
    fun_name = "test_fun",
    fun_args = list(x = 1),
    priority = 1
  )

  result <- suppressWarnings(run_task(paste0(conf$dir, "conf.yml")))

  expect_equal(result, 42L)
  expect_equal(yaml::read_yaml(paste0(conf$dir, "conf.yml"))$run_info$status, "finished")

  log_file <- list.files(paste0(conf$dir, "output"), pattern = "log_run", full.names = TRUE)
  log_lines <- readLines(log_file)

  expect_true(any(grepl("WARN", log_lines)))
  expect_true(any(grepl("test warning", log_lines)))
  expect_false(any(grepl("FATAL", log_lines)))
})

test_that("message is logged as INFO and task completes as finished", {
  dir_conf <- file.path(tempdir(), "handler_test_msg")
  unlink(dir_conf, recursive = TRUE)
  dir.create(dir_conf, recursive = TRUE)

  fun_path <- make_fun_script(dir_conf, c(
    "  message('test message')",
    "  99L"
  ))

  conf <- configure_task(
    dir_path = dir_conf,
    conf_descr = list(title = "test"),
    fun_path = fun_path,
    fun_name = "test_fun",
    fun_args = list(x = 1),
    priority = 1
  )

  result <- suppressMessages(run_task(paste0(conf$dir, "conf.yml")))

  expect_equal(result, 99L)
  expect_equal(yaml::read_yaml(paste0(conf$dir, "conf.yml"))$run_info$status, "finished")

  log_file <- list.files(paste0(conf$dir, "output"), pattern = "log_run", full.names = TRUE)
  log_lines <- readLines(log_file)

  expect_true(any(grepl("INFO", log_lines)))
  expect_true(any(grepl("test message", log_lines)))
  expect_false(any(grepl("FATAL", log_lines)))
})

test_that("nested sourced functions show traceback with file and line info", {
  dir_conf <- file.path(tempdir(), "handler_test_nested")
  unlink(dir_conf, recursive = TRUE)
  dir.create(dir_conf, recursive = TRUE)

  # helper script — defines validate() and compute()
  helper_path <- normalizePath(file.path(dir_conf, "helpers.R"), winslash = "/", mustWork = FALSE)
  writeLines(c(
    "validate <- function(v) {",                        # line 1
    "  if (v < 0) stop('negative value not allowed')",  # line 2
    "}",                                                # line 3
    "compute <- function(val) {",                       # line 4
    "  validate(val)",                                  # line 5
    "}"                                                 # line 6
  ), con = helper_path)

  # main script — sources helper, defines test_fun using compute via process
  fun_path <- normalizePath(file.path(dir_conf, "fun_script.R"), winslash = "/", mustWork = FALSE)
  writeLines(c(
    paste0("source('", helper_path, "')"),  # line 1
    "test_fun <- function(x) {",             # line 2
    "  process <- function(data) {",         # line 3
    "    compute(data - 10)",                # line 4  -> triggers validate(-9) -> stop()
    "  }",                                   # line 5
    "  process(x)",                          # line 6
    "}"                                      # line 7
  ), con = fun_path)

  conf <- configure_task(
    dir_path = dir_conf,
    conf_descr = list(title = "test"),
    fun_path = fun_path,
    fun_name = "test_fun",
    fun_args = list(x = 1),
    priority = 1
  )

  result <- run_task(paste0(conf$dir, "conf.yml"))

  expect_null(result)
  expect_equal(yaml::read_yaml(paste0(conf$dir, "conf.yml"))$run_info$status, "error")

  log_file <- list.files(paste0(conf$dir, "output"), pattern = "log_run", full.names = TRUE)
  expect_length(log_file, 1)
  log_lines <- readLines(log_file)

  expect_true(any(grepl("FATAL", log_lines)))
  expect_true(any(grepl("negative value not allowed", log_lines)))
  expect_true(any(grepl("Traceback", log_lines)))

  # toutes les fonctions imbriquées doivent apparaître
  expect_true(any(grepl("validate", log_lines)))
  expect_true(any(grepl("compute",  log_lines)))
  expect_true(any(grepl("process",  log_lines)))

  # les deux scripts doivent apparaître dans les srcrefs
  expect_true(any(grepl("helpers\\.R", log_lines)))
  expect_true(any(grepl("fun_script\\.R", log_lines)))

  # des numéros de ligne doivent être présents (format [fichier:N])
  expect_true(any(grepl("\\[.+:[0-9]+\\]", log_lines)))
})
