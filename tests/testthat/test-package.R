test_that("failed to get armadillo connection", {
  expect_error(
    .get_armadillo_connection(),
    "Please login using: 'armadillo.login('http://armadillo', 'http://minio')'",
    fixed = TRUE
  )
})

test_that("get armadillo connection", {
  options(MolgenisArmadillo.auth.token = "abcde")
  options(MolgenisArmadillo.armadillo.endpoint = "http://armadillo")

  local_reproducible_output()
  expect_equal(
    .get_armadillo_connection(),
    list(handle = httr::handle(
      "http://armadillo"),
      headers = httr::add_headers("Authorization" = "Bearer abcde"))
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

test_that("profile not found when installing packages", {
  response <- list(status_code = 404)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  httr_content <- mock("Profile not found")
  broken_profile <- "broken"
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
    paste0(
      "Endpoint doesn't exist. ",
      "Make sure you're running Armadillo in development mode.")
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

test_that("install packages fails due to missing profile", {
  response <- list(status_code = 404)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_path <- "C:/test/test.tar.gz"

  expect_error(
    with_mock(
      armadillo.install_packages(paths = expected_path, profile = "exposome"),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "MolgenisArmadillo:::.install_package" = mock(),
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post
    ),
    "Profile not found: [ 'exposome' ]",
    fixed = TRUE
  )
})

test_that("install packages", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_path1 <- "C:/test/test1.tar.gz"
  expected_path2 <- "C:/test/test2.tar.gz"

  expect_silent(
    with_mock(
      armadillo.install_packages(paths = c(expected_path1, expected_path2)),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "MolgenisArmadillo:::.install_package" = mock(),
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post
    )
  )
})

test_that("install packages fails because of empty path", {
  expect_error(
    with_mock(
      armadillo.install_packages(paths = ""),
      "MolgenisArmadillo:::.install_package" = mock(),
      "httr:::upload_file" = mock()
    ),
    paste0(
      "You need to specify the full path(s) of the package(s);",
      "e.g. 'C:/User/test.tar.gz'"),
    fixed = TRUE
  )
})

test_that("whitelist a package", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection <- mock(list(handle = httr_handle, headers = mock()))
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

test_that("whitelist a package", {
  response <- list(status_code = 404)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_pkg <- "DSI"

  expect_error(
    with_mock(
      .whitelist_package(pkg = expected_pkg),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "httr:::POST" = httr_post
    ),
    regexp = paste0(
      "Endpoint doesn't exist. ",
      "Make sure you're running Armadillo in development mode.")
  )
})

test_that("whitelist packages", {
  response <- list(status_code = 200)
  httr_request <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_pkg1 <- "DSI"
  expected_pkg2 <- "unicorns"

  expect_message(
    with_mock(
      armadillo.whitelist_packages(pkgs = c(expected_pkg1, expected_pkg2)),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "MolgenisArmadillo:::.whitelist_package" = mock(),
      "httr:::POST" = httr_request,
      "httr:::content" = mock(c("DSI", "unicorns")),
      "httr:::GET" = httr_request
    ),
    regexp = "^Packages whitelisted:"
  )
})

test_that("whitelist packages fails on empty package", {
  expect_error(
    with_mock(
      armadillo.whitelist_packages(pkgs = ""),
      "MolgenisArmadillo:::.whitelist_package" = mock()
    ),
    regexp = paste0(
      "You need to specify the the package(s) ",
      "you want to whitelist; e.g. 'DSI'"),
    fixed = TRUE
  )
})

test_that("whitelist packages fails because profile not set", {
  response <- list(status_code = 404)
  httr_request <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection <- mock(list(handle = httr_handle, headers = mock()))
  expected_pkg <- "DSI"
  profile <- "unavailable"

  expect_error(
    with_mock(
      armadillo.whitelist_packages(pkgs = expected_pkg, profile = profile),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "MolgenisArmadillo:::.whitelist_package" = mock(),
      "httr:::POST" = httr_request,
      "httr:::content" = mock(c("DSI")),
      "httr:::GET" = httr_request
    ),
    paste0("Profile not found: [ '", profile, "' ]"),
    fixed = TRUE
  )
})

test_that("is_empty: not empty string", {
  expect_silent(.is_empty("message", "blaat"))
})

test_that("is_empty: not empty vector > 1", {
  expect_silent(.is_empty("message", c("blaat", "blaat2")))
})

test_that("is_empty: vector > 1 with empty value", {
  expect_error(.is_empty("message", c("blaat", "")), "message")
})

test_that("is_empty: empty string", {
  expect_error(.is_empty("message", ""), "message")
})

test_that("is_empty: non character type", {
  expect_error(
    .is_empty("message", NULL),
    "Datatype of package should be character: [] is type of [NULL]")
})
