test_that("armadillo.create_folder checks folder name", {
  expect_error(
    session <- armadillo.create_folder("example_folder"),
    "Folder name must consist of lowercase letters and numbers"
  )
})

test_that("create_folder creates a folder", {
  put_bucket <- mock(TRUE)
  get_ption <- mock(FALSE)

  with_mock(
    result <- armadillo.create_folder("examplefolder"),
    "aws.s3::put_bucket" = put_bucket,
    "getOption" = get_ption
  )

  expect_true(result)
  expect_args(getOption, 1, "MolgenisArmadillo.s3.use_https")
  expect_args(put_bucket, 1, "shared-examplefolder", use_https = FALSE)
})
