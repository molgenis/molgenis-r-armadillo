library(testthat)
handle <- httr::handle("https://test.nl")
withr::local_options("MolgenisArmadillo.armadillo.handle" = handle)

mock_projects_with_users <- '[
  {
    "name": "lifecycle",
    "users": []
  },
  {
    "name": "other-project",
    "users": ["john", "tommy"]
  },
  {
    "name": "other-projectnumbertwo",
    "users": []
    }
]'

get_projects_header <- list("Content-Type" = "application/json")

test_that("armadillo.create_project checks folder name", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )
  expect_error(
    armadillo.create_project("example_folder", overwrite = "no"),
    "Project name must consist of lowercase letters and numbers\\."
  )
  stub_registry_clear()
})

test_that("armadillo.create_project creates a folder", {
  stub_request("put", uri = "https://test.nl/access/projects") %>%
    wi_th(
      headers = list(
        "Accept" =
          "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/json"
      ),
      body = '{"name":"project"}'
    ) %>%
    to_return(
      status = 204
    )
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )

  expect_message(
    armadillo.create_project("project"),
    "Created project 'project'"
  )

  stub_registry_clear()
})

test_that("armadillo.create_project with users", {
  stub_request("put", uri = "https://test.nl/access/projects") %>%
    wi_th(
      headers = list(
        "Accept" =
          "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/json"
      ),
      body = '{"name":"project","users":["user1@users.com","user2@users.com"]}'
    ) %>%
    to_return(
      status = 204
    )
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )

  expect_message(
    armadillo.create_project(
      "project",
      list("user1@users.com", "user2@users.com")
    ),
    "Created project 'project' with users: user1@users.com, user2@users.com"
  )

  stub_registry_clear()
})

test_that("armadillo.create_project with empty user list", {
  stub_request("put", uri = "https://test.nl/access/projects") %>%
    wi_th(
      headers = list(
        "Accept" =
          "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/json"
      ),
      body = '{"name":"project"}'
    ) %>%
    to_return(
      status = 204
    )

  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )

  expect_message(
    armadillo.create_project("project", list()),
    "Created project 'project' without users"
  )

  stub_registry_clear()
})

test_that("armadillo.list_projects lists all shared buckets", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = '[
        {
          "name": "lifecycle",
          "users": []
        },
        {
          "name": "test",
          "users": []
        }
      ]',
      headers = get_projects_header
    )

  res <- armadillo.list_projects()
  expect_equal(res, c("lifecycle", "test"))

  stub_registry_clear()
})

test_that("armadillo.delete_project handles errors", {
  stub_request("delete", uri = "https://test.nl/access/projects/project") %>%
    to_return(
      status = 404,
      body = '{
        "message": "project not found"
      }',
      headers = get_projects_header
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

test_that("armadillo.get_projects_info gets all projects and their users", {
  test_data <- list(
    list("name" = "lifecycle", "users" = list()),
    list("name" = "other-project", "users" = list("john", "tommy")),
    list("name" = "other-projectnumbertwo", "users" = list())
  )
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )
  res <- armadillo.get_projects_info()
  expect_equal(res, test_data)

  stub_registry_clear()
})

test_that("armadillo.get_project_users with a project that has users", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )
  res <- armadillo.get_project_users("other-project")
  expect_equal(res, list("john", "tommy"))

  stub_registry_clear()
})

test_that("armadillo.get_project_users with a project that has no users", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = list("Content-Type" = "application/json")
    )
  res <- armadillo.get_project_users("lifecycle")
  expect_equal(res, list())

  stub_registry_clear()
})

test_that("armadillo.get_project_users with a non existing project", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )
  expect_error(
    armadillo.get_project_users("nonexisting-project"),
    "Project nonexisting-project not found."
  )

  stub_registry_clear()
})

test_that("armadillo.create_project without overwriting existing project", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )

  stub_request("put", uri = "https://test.nl/access/projects") %>%
    wi_th(
      headers = list(
        "Accept" =
          "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/json"
      ),
      body = '{"name":"other-project"}'
    ) %>%
    to_return(
      status = 204
    )

  # Case sensitive
  expect_message(
    armadillo.create_project("other-project", overwrite_existing = "no"),
    "Did not create project: 'other-project' already exists and overwrite is set to 'no'"
  )

  # Non case sensitive
  expect_error(
    armadillo.create_project("oTHeR-ProJEct"),
    "Project name must consist of lowercase letters and numbers."
  )

  stub_registry_clear()
})

test_that("armadillo.create_project with existing overwrite", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )

  stub_request("put", uri = "https://test.nl/access/projects") %>%
    wi_th(
      headers = list(
        "Accept" =
          "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/json"
      ),
      body = '{"name":"other-project"}'
    ) %>%
    to_return(
      status = 204
    )

  expect_message(
    armadillo.create_project("other-project", overwrite_existing = "yes"),
    "Created project 'other-project' without users"
  )

  stub_registry_clear()
})

test_that("armadillo.create_project with nonexisting overwrite", {
  stub_request("get", uri = "https://test.nl/access/projects") %>%
    to_return(
      status = 200,
      body = mock_projects_with_users,
      headers = get_projects_header
    )

  stub_request("put", uri = "https://test.nl/access/projects") %>%
    wi_th(
      headers = list(
        "Accept" =
          "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/json"
      ),
      body = '{"name":"other-projectnumbertwo"}'
    ) %>%
    to_return(
      status = 204
    )

  expect_message(
    armadillo.create_project(
      "other-projectnumbertwo",
      overwrite_existing = "yes"
    ),
    # Required for linting
    paste0(
      "Created project 'other-projectnumbertwo' without users with overwrite set to 'yes'"
    ),
  )

  stub_registry_clear()
})

test_that(".make_overwrite_menu when user selects 'yes'", {
  expect_true(
    testthat::with_mocked_bindings(
      .make_overwrite_menu(project_name = "lifecycle"),
      askYesNo = function(title, default) TRUE
    )
  )
})

test_that(".make_overwrite_menu when user selects 'no'", {
  expect_false(
    testthat::with_mocked_bindings(
      .make_overwrite_menu(project_name = "lifecycle"),
      askYesNo = function(title, default) FALSE
    )
  )
})

test_that(".make_overwrite_menu when user selects 'cancel'", {
  expect_equal(
    testthat::with_mocked_bindings(
      .make_overwrite_menu(project_name = "lifecycle"),
      askYesNo = function(title, default) NA
    ),
    "cancel"
  )
})

test_that(".get_overwrite_choice where menu is displayed", {
  expect_true(
    testthat::with_mocked_bindings(
      .get_overwrite_choice(project_name = "lifecycle", project_exists = TRUE, overwrite_existing = "choose"),
      .make_overwrite_menu = function(project_name) TRUE
    )
  )
})

test_that(".get_overwrite_choice where overwrite is set to TRUE", {
  expect_true(
    .get_overwrite_choice(project_name = "lifecycle", project_exists = TRUE, overwrite_existing = "yes"),
  )
})

test_that(".get_overwrite_choice where overwrite is set to FALSE", {
  expect_false(
    .get_overwrite_choice(project_name = "lifecycle", project_exists = TRUE, overwrite_existing = "no"),
  )
})
