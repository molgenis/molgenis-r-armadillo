handle <- httr::handle("https://test.nl")
withr::local_options("MolgenisArmadillo.armadillo.handle" = handle)

test_that("failed to install a package", {
  response <- list(status_code = 500)

  expect_error(
    with_mocked_bindings(
      .install_package(path = "C:/test/test.tar.gz"),
      .get_url = function(){mock()},
      upload_file =  mock(),
      POST = mock(response, cycle = TRUE), 
      content = function(response, as, encoding){list(message = "")}
    ),
    "Internal server error: "
  )
})

test_that("profile not found when installing packages", {
  response <- list(status_code = 404)

  expect_error(
    with_mocked_bindings(
      .install_package(path = "C:/test/test.tar.gz"),
      upload_file = mock(),
      POST = mock(response, cycle = TRUE),
      content = mock("Profile not found")
    ),
    paste0(
      "Endpoint doesn't exist. ",
      "Make sure you're running Armadillo in development mode."
    )
  )
})

test_that("install a package", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  expect_message(
    with_mocked_bindings(
      .install_package(path = "C:/test/test.tar.gz"),
      upload_file = mock(),
      POST = httr_post
    ),
    regexp = "^Attempting to install package",
  )
})


test_that("failed to install a package", {
  response <- list(status_code = 404)
    expect_error(
      with_mocked_bindings(
        armadillo.install_packages(paths = "C:/test/test.tar.gz", profile = "exposome"),
        .install_package = mock(),
        upload_file = mock(),
        POST =  mock(response, cycle = TRUE)  
      ),
      "Profile not found: [ 'exposome' ]",
      fixed = TRUE
    )
})


test_that("install packages", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  expected_path1 <- "C:/test/test1.tar.gz"
  expected_path2 <- "C:/test/test2.tar.gz"

  expect_silent(
      with_mocked_bindings(
        armadillo.install_packages(paths = "C:/test/test.tar.gz", profile = "exposome"),
        .install_package = mock(),
        upload_file = mock(),
        POST =  mock(response, cycle = TRUE)  
      )
  )
})

test_that("install packages fails because of empty path", {
  
  expect_error(
    with_mocked_bindings(
      armadillo.install_packages(paths = ""),
      .install_package = mock(),
      upload_file = mock()
    ),
    paste0(
      "You need to specify the full path(s) of the package(s);",
      "e.g. 'C:/User/test.tar.gz'"
    ),
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
    "Datatype of package should be character: [] is type of [NULL]"
  )
})
