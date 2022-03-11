test_that("failed to get armadillo connection", {
  expect_error(
    .get_armadillo_connection(),
    "Please login using: 'armadillo.login('http://armadillo', 'http://minio')'",
    fixed=TRUE
  )
})

test_that("get armadillo connection", {
  options(MolgenisArmadillo.auth.token = "abcde")
  options(MolgenisArmadillo.armadillo.endpoint = "http://armadillo")
  
  local_reproducible_output()
  expect_equal(
    .get_armadillo_connection(),
    list(handle = httr::handle("http://armadillo"), headers = httr::add_headers("Authorization" = "Bearer abcde"))
  )
  
  options(MolgenisArmadillo.auth.token = NULL)
  options(MolgenisArmadillo.armadillo.endpoint = NULL)
})

test_that("failed to install a package", {
  response <- list(status_code = 500)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  httr_content <- mock("Could not install package")
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_path <- "C:/test/test.tar.gz"
  
  expect_error(
    with_mock(
      .install_package(path = expected_path),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post,
      "httr:::content" = httr_content
    ),
    "Internal server error: "
  )
})

test_that("failed to install a package", {
  response <- list(status_code = 404)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  httr_content <- mock("Could not install package")
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_path <- "C:/test/test.tar.gz"
  
  expect_error(
    with_mock(
      .install_package(path = expected_path),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post,
      "httr:::content" = httr_content
    ),
    ""
  )
})

test_that("install a package", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_path <- "C:/test/test.tar.gz"
  
  expect_message(
    with_mock(
      .install_package(path = expected_path),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post
    ),
    regexp = "^Attempting to install package",
  )
})

test_that("install packages", {
  response <- list(status_code = 404)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection = mock(list(handle = httr_handle, headers = mock()))
  expected_path = "C:/test/test.tar.gz"
  
  expect_error(
    with_mock(
      armadillo.install_packages(path = expected_path, profile = "exposome"),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "MolgenisArmadillo:::.install_package" = mock(),
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post
    ),
    "Profile not found: [ 'exposome' ]",
    fixed = TRUE
  )
})

test_that("whitelist a package", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection = mock(list(handle = httr_handle, headers = mock()))
  expected_pkg <- "DSI"
  
  expect_message(
    with_mock(
      .whitelist_package(pkg = expected_pkg),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "httr:::POST" = httr_post
    ),
    regexp = "^Attempting to whitelist package",
  )
})

test_that("whitelist packages", {
  response <- list(status_code = 200)
  httr_request <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection = mock(list(handle = httr_handle, headers = mock()))
  expected_pkg <- "DSI"
  
  expect_message(
    with_mock(
      armadillo.whitelist_packages(pkg = expected_pkg),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "MolgenisArmadillo:::.whitelist_package" = mock(),
      "httr:::POST" = httr_request,
      "httr:::content" = mock(c("DSI")),
      "httr:::GET" = httr_request
    ),
    regexp = "^Packages whitelisted:"
  )
})