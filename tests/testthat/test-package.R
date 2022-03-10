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
    paste0("Internal server error: ")
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
    paste0("")
  )
})

test_that("install a package", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection = mock(list(handle = httr_handle, headers = mock()))
  expected_path = "C:/test/test.tar.gz"
  
  expect_message(
    with_mock(
      .install_package(path = expected_path),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post
    ),
    cat(paste0("Attempting to install package [ ' ", expected_path, " ' ]", "\n","\n", "Package [ ", expected_path," ] installed"))
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
    cat(paste0("Profile not found: 'exposome'"))
  )
})
