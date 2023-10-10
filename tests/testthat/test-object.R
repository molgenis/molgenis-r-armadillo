handle <- httr::handle("https://test.nl")
withr::local_options("MolgenisArmadillo.armadillo.handle" = handle)

test_that(".upload_object handles errors", {
  file <- tempfile()
  saveRDS(tibble::tribble(), file)
  on.exit(unlink(file))

  upload_file <- httr::upload_file(file)
  compress <- mock(".rds")

  stub_request("post",
               uri = "https://test.nl/storage/projects/project/objects") %>%
    wi_th(
      header = list("Content-Type" = "multipart/form-data"),
      body = list(
        file = upload_file,
        object = "example/test.rds"
      )
    ) %>%
    to_return(
      status = 404,
      body = "{
        \"message\": \"project not found\"
      }",
      headers = list("Content-Type" = "application/json")
    )

  expect_error(
    with_mock({
               .upload_object(
                 project = "project",
                 folder = "example",
                 object = datasets::iris,
                 name = "test",
                 compression_function = compress
               )},
    "tempfile" = function() {
      file
    }),
    "project not found"
  )
  stub_registry_clear()
})

test_that(".upload_object uploads object", {
  file <- tempfile()
  saveRDS(tibble::tribble(), file)
  on.exit(unlink(file))

  upload_file <- httr::upload_file(file)
  compress <- mock(".rds")

  stub_request("post",
               uri = "https://test.nl/storage/projects/project/objects") %>%
    wi_th(
      header = list("Content-Type" = "multipart/form-data"),
      body = list(
        file = upload_file,
        object = "example/test.rds"
      )
    ) %>%
    to_return(status = 204)

  expect_message(
    with_mock({
               .upload_object(
                 project = "project",
                 folder = "example",
                 object = datasets::iris,
                 name = "test",
                 compression_function = compress
               )},
    "tempfile" = function() {
      file
    }),
    "Uploaded example/test"
  )
  stub_registry_clear()
})

test_that(".list_objects_by_extension handles errors", {
  stub_request("get",
               uri = "https://test.nl/storage/projects/project/objects") %>%
    to_return(
      status = 404,
      body = "{
        \"message\": \"project not found\"
      }",
      headers = list("Content-Type" = "application/json")
    )

  expect_error(
    .list_objects_by_extension("project", ".rds"),
    "project not found"
  )

  stub_registry_clear()
})

test_that(".list_objects_by_extension lists the objects in a project", {
  stub_request("get",
               uri = "https://test.nl/storage/projects/project/objects") %>%
    to_return(
      status = 200,
      body = "[
        \"core/nonrep.parquet\",
        \"core/yearlyrep.parquet\",
        \"core/resource.rds\"
      ]",
      headers = list("Content-Type" = "application/json")
    )

  res <- .list_objects_by_extension("project", ".parquet")
  expect_equal(res, c("core/nonrep", "core/yearlyrep"))

  stub_registry_clear()
})

test_that(".delete_object handles errors", {
  stub_request("delete",
               uri = paste0("https://test.nl/storage/projects/project/",
                            "objects/core%2Fnonrep.parquet")) %>%
    to_return(
      status = 404,
      body = "{
        \"message\": \"object not found\"
      }",
      headers = list("Content-Type" = "application/json")
    )

  expect_error(
    .delete_object("project", "core", "nonrep", ".parquet"),
    "object not found"
  )

  stub_registry_clear()
})

test_that(".delete_object deletes an object", {
  stub_request("delete",
               uri = paste0("https://test.nl/storage/projects/project/objects/",
                            "core%2Fnonrep.parquet")) %>%
    to_return(
      status = 204
    )

  expect_message(
    .delete_object("project", "core", "nonrep", ".parquet"),
    "Deleted 'core/nonrep'"
  )

  stub_registry_clear()
})

test_that(".copy_object handles errors", {
  stub_request("post",
               uri = paste0("https://test.nl/storage/projects/project/objects/",
                            "core%2Fnonrep.parquet/copy")) %>%
    wi_th(
      headers = list("Accept" = "application/json"),
      body = list(name = "core/copy.parquet")
    ) %>%
    to_return(
      status = 409,
      body = "{
        \"message\": \"duplicate object\"
      }",
      headers = list("Content-Type" = "application/json")
    )

  expect_error(
    .copy_object(
      project = "project",
      folder = "core",
      name = "nonrep",
      new_folder = "core",
      new_name = "copy",
      extension = ".parquet"
    ),
    "duplicate object"
  )

  stub_registry_clear()
})

test_that(".copy_object warns if you copy an object onto itself", {
  expect_error(
    .copy_object(
      project = "project",
      folder = "folder",
      name = "test",
      extension = ".parquet"
    ),
    "Cannot copy table or resource onto itself\\."
  )
})

test_that(".copy_object copies object", {
  stub_request("post",
               uri = paste0("https://test.nl/storage/projects/project/objects/",
                            "core%2Fnonrep.parquet/copy")) %>%
    wi_th(
      headers = list("Accept" = "application/json"),
      body = list(name = "core/copy.parquet")
    ) %>%
    to_return(status = 204)

  expect_message(
    .copy_object(
      project = "project",
      folder = "core",
      name = "nonrep",
      new_folder = "core",
      new_name = "copy",
      extension = ".parquet"
    ),
    "Copied 'project/core/nonrep' to 'project/core/copy'"
  )

  stub_registry_clear()
})

test_that(".load_object handles errors", {
  stub_request("get",
               uri = paste0("https://test.nl/storage/projects/project/objects/",
                            "core%2Fnonrep.rds")) %>%
    wi_th(headers = list("Accept" = "application/octet-stream")) %>%
    to_return(status = 401)

  expect_error(
    .load_object(
      project = "project",
      folder = "core",
      name = "nonrep",
      load_function = load,
      extension = ".rds"
    ),
    "Unauthorized"
  )

  stub_registry_clear()
})

test_that(".load_object loads the object from file", {
  file <- tempfile()
  data <- tibble::tribble(
    ~colA, ~colB,
    "a", 1,
    "b", 2,
    "c", 3
  )
  saveRDS(data, file)
  on.exit(unlink(file))

  save_object <- mock(invisible(file))

  load_table <- function(infile) {
    readRDS(infile)
  }

  stub_request("get",
               uri = paste0("https://test.nl/storage/projects/project/objects/",
                            "core%2Fnonrep.parquet")) %>%
    wi_th(headers = list("Accept" = "application/octet-stream")) %>%
    to_return(status = 200, body = stringi::stri_read_raw(file))

  expect_silent(
    with_mock(
      {
        result <- .load_object(
          project = "project",
          folder = "core",
          name = "nonrep",
          load_function = load_table,
          extension = ".parquet"
        )
      },
      "tempfile" = function() {
        file
      }
    )
  )

  expect_equal(data, result)

  stub_registry_clear()
})

test_that(".move_object handles errors", {
  stub_request("post",
               uri = paste0("https://test.nl/storage/projects/project/objects/",
                            "core%2Fnonrep.parquet/move")) %>%
    wi_th(
      headers = list("Accept" = "application/json"),
      body = list(name = "other/renamed.parquet")
    ) %>%
    to_return(
      status = 409,
      body = "{
        \"message\": \"duplicate object\"
      }",
      headers = list("Content-Type" = "application/json")
    )

  expect_error(
    .move_object(
      project = "project",
      folder = "core",
      name = "nonrep",
      new_folder = "other",
      new_name = "renamed",
      extension = ".parquet"
    ),
    "duplicate object"
  )

  stub_registry_clear()
})

test_that(".move_object warns if you move an object onto itself", {
  expect_error(
    .move_object(
      project = "project",
      folder = "folder",
      name = "test",
      extension = ".parquet"
    ),
    "Cannot move table or resource onto itself\\."
  )
})

test_that(".move_object moves object", {
  stub_request("post",
               uri = paste0("https://test.nl/storage/projects/project/objects/",
                            "core%2Fnonrep.parquet/move")) %>%
    wi_th(
      headers = list("Accept" = "application/json"),
      body = list(name = "other/renamed.parquet")
    ) %>%
    to_return(status = 204)

  expect_message(
    .move_object(
      project = "project",
      folder = "core",
      name = "nonrep",
      new_folder = "other",
      new_name = "renamed",
      extension = ".parquet"
    ),
    "Moved 'project/core/nonrep' to 'project/other/renamed'"
  )

  stub_registry_clear()
})
