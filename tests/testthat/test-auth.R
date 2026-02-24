test_that(".check_admin succeeds for admin user", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = "[]",
      headers = list("Content-Type" = "application/json")
    )

  expect_silent(.check_admin("https://test.nl/"))

  stub_registry_clear()
})

test_that(".check_admin errors on 403 for non-admin user", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(status = 403)

  expect_error(
    .check_admin("https://test.nl/"),
    "User does not have admin permissions on this server."
  )

  stub_registry_clear()
})

test_that(".check_admin errors on 401 for unauthenticated user", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(status = 401)

  expect_error(
    .check_admin("https://test.nl/"),
    "User does not have admin permissions on this server."
  )

  stub_registry_clear()
})

test_that("armadillo.login errors when user is not admin", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(status = 403)

  expect_error(
    with_mocked_bindings(
      armadillo.login("https://test.nl/"),
      armadillo.get_token = function(server) "fake-token"
    ),
    "User does not have admin permissions on this server."
  )

  stub_registry_clear()
})

test_that("armadillo.login succeeds for admin user", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = "[]",
      headers = list("Content-Type" = "application/json")
    )

  with_mocked_bindings(
    armadillo.login("https://test.nl/"),
    armadillo.get_token = function(server) "fake-token"
  )

  expect_equal(.pkgglobalenv$auth_token, "fake-token")
  expect_equal(.pkgglobalenv$armadillo_url, "https://test.nl/")

  stub_registry_clear()
})

test_that("armadillo.login_basic errors when user is not admin", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(status = 403)

  expect_error(
    armadillo.login_basic("https://test.nl/", "user", "pass"),
    "User does not have admin permissions on this server."
  )

  stub_registry_clear()
})

test_that("armadillo.login_basic succeeds for admin user", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = "[]",
      headers = list("Content-Type" = "application/json")
    )

  armadillo.login_basic("https://test.nl/", "admin", "admin")

  expect_equal(.pkgglobalenv$auth_username, "admin")
  expect_equal(.pkgglobalenv$auth_password, "admin")

  stub_registry_clear()
})
