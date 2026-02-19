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
