test_that("armadillo.upload_table checks if table is provided", {
  expect_error(
    armadillo.upload_table(project = "project", folder = "folder"),
    "argument \"table\" is missing, with no default"
  )
})

test_that("armadillo.upload_table checks if table is a data frame", {
  expect_error(
    armadillo.upload_table(project = "project", folder = "folder", table = 5),
    "is\\.data\\.frame\\(table\\) is not TRUE"
  )
})

test_that("armadillo.upload_table checks if folder is provided", {
  expect_error(
    armadillo.upload_table(project = "project", table = datasets::iris),
    "argument \"folder\" is missing, with no default"
  )
})

test_that("armadillo.upload_table calls .upload_object", {
  upload_object <- mock()

  with_mock(armadillo.upload_table("project",
    "folder",
    table = datasets::iris
  ),
  "MolgenisArmadillo:::.upload_object" = upload_object
  )

  expect_args(upload_object, 1,
    project = "project",
    folder = "folder",
    object = datasets::iris,
    name = "datasets::iris",
    compression_function = .compress_table
  )
})

test_that("armadillo.list_tables calls .list_objects_by_extension", {
  list_objects <- mock()

  with_mock(armadillo.list_tables("project"),
    "MolgenisArmadillo:::.list_objects_by_extension" = list_objects
  )

  expect_args(list_objects, 1,
    project = "project",
    extension = ".parquet"
  )
})

test_that("armadillo.delete_table calls .delete_object", {
  delete_object <- mock()

  with_mock(armadillo.delete_table("project", "folder", "name"),
    "MolgenisArmadillo:::.delete_object" = delete_object
  )

  expect_args(delete_object, 1,
    project = "project",
    folder = "folder",
    name = "name",
    extension = ".parquet"
  )
})

test_that("armadillo.copy_table calls .copy_object", {
  copy_object <- mock()

  with_mock(armadillo.copy_table("project", "folder", "name"),
    "MolgenisArmadillo:::.copy_object" = copy_object
  )

  expect_args(copy_object, 1,
    project = "project",
    folder = "folder",
    name = "name",
    new_project = "project",
    new_folder = "folder",
    new_name = "name",
    extension = ".parquet"
  )
})

test_that("armadillo.move_table calls .move_object", {
  move_object <- mock()

  with_mock(armadillo.move_table("project", "folder", "name"),
    "MolgenisArmadillo:::.move_object" = move_object
  )

  expect_args(move_object, 1,
    project = "project",
    folder = "folder",
    name = "name",
    new_project = "project",
    new_folder = "folder",
    new_name = "name",
    extension = ".parquet"
  )
})

test_that("armadillo.load_table calls .load_object", {
  load_object <- mock()

  with_mock(armadillo.load_table(
    "project",
    "folder",
    "name"
  ),
  "MolgenisArmadillo:::.load_object" = load_object
  )

  expect_args(load_object, 1,
    project = "project",
    folder = "folder",
    name = "name",
    load_function = .load_table,
    extension = ".parquet"
  )
})
