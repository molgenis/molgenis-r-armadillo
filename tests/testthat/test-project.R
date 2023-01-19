handle <- httr::handle("https://test.nl")
withr::local_options("MolgenisArmadillo.armadillo.handle" = handle)

test_that("armadillo.create_project checks folder name", {
  expect_error(
    armadillo.create_project("example_folder"),
    "Project name must consist of lowercase letters and numbers\\."
  )
})

test_that("armadillo.create_project creates a folder", {
  stub_request("put", uri = "https://test.nl/access/projects") %>%
    wi_th(
      headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "Content-Type" = "application/json"),
      body = "{\"name\":\"project\"}"
    ) %>%
    to_return(
      status = 204
    )

  expect_message(
    armadillo.create_project("project"),
    "Created project 'project'"
  )

  stub_registry_clear()
})

test_that("armadillo.list_projects lists all shared buckets", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = "[
        {
          \"name\": \"lifecycle\",
          \"users\": []
        },
        {
          \"name\": \"test\",
          \"users\": []
        }
      ]",
      headers = list("Content-Type" = "application/json")
    )

  res <- armadillo.list_projects()
  expect_equal(res, c("lifecycle", "test"))

  stub_registry_clear()
})

test_that("armadillo.delete_project handles errors", {
  stub_request("delete", uri = "https://test.nl/access/projects/project") %>%
    to_return(
      status = 404,
      body = "{
        \"message\": \"project not found\"
      }",
      headers = list("Content-Type" = "application/json")
    )

  expect_error(
    armadillo.delete_project("project"),
    "project not found"
  )

  stub_registry_clear()
})

test_that("armadillo.delete_project deletes project", {
  stub_request("delete", uri = "https://test.nl/access/projects/project") %>%
    to_return(status = 204)

  expect_message(
    armadillo.delete_project("project"),
    "Deleted project 'project'"
  )

  stub_registry_clear()
})
