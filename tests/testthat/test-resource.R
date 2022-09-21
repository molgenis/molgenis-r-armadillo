test_that("armadillo.upload_resource checks if resource is provided", {
  expect_error(
    armadillo.upload_resource(project = "project", folder = "folder"),
    "argument \"resource\" is missing, with no default"
  )
})

test_that("armadillo.upload_resource checks if folder is provided", {
  expect_error(
    armadillo.upload_resource(project = "project", resource = datasets::iris),
    "argument \"folder\" is missing, with no default"
  )
})

test_that("armadillo.upload_resource calls .upload_object", {
  upload_object <- mock()

  with_mock(armadillo.upload_resource("project",
    "folder",
    resource = datasets::iris
  ),
  "MolgenisArmadillo:::.upload_object" = upload_object
  )

  expect_args(upload_object, 1,
    project = "project",
    folder = "folder",
    object = datasets::iris,
    name = "datasets::iris",
    compression_function = .compress_resource
  )
})

test_that("armadillo.list_resources calls .list_objects_by_extension", {
  list_objects <- mock()

  with_mock(armadillo.list_resources("project"),
    "MolgenisArmadillo:::.list_objects_by_extension" = list_objects
  )

  expect_args(list_objects, 1,
    project = "project",
    extension = ".rds"
  )
})

test_that("armadillo.delete_resource calls .delete_object", {
  delete_object <- mock()

  with_mock(armadillo.delete_resource("project", "folder", "name"),
    "MolgenisArmadillo:::.delete_object" = delete_object
  )

  expect_args(delete_object, 1,
    project = "project",
    folder = "folder",
    name = "name",
    extension = ".rds"
  )
})

test_that("armadillo.copy_resource calls .copy_object", {
  copy_object <- mock()

  with_mock(armadillo.copy_resource("project", "folder", "name"),
    "MolgenisArmadillo:::.copy_object" = copy_object
  )

  expect_args(copy_object, 1,
    project = "project",
    folder = "folder",
    name = "name",
    new_folder = "folder",
    new_name = "name",
    extension = ".rds"
  )
})

test_that("armadillo.move_resource calls .move_object", {
  move_object <- mock()

  with_mock(armadillo.move_resource("project", "folder", "name"),
    "MolgenisArmadillo:::.move_object" = move_object
  )

  expect_args(move_object, 1,
    project = "project",
    folder = "folder",
    name = "name",
    new_folder = "folder",
    new_name = "name",
    extension = ".rds"
  )
})

test_that("armadillo.load_resource calls .load_object", {
  load_object <- mock()
  environment <- new.env()

  with_mock(armadillo.load_resource(
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
    load_function = .load_resource,
    extension = ".rds"
  )
})
