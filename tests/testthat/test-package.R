test_that("install a package", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)
  httr_handle <- mock(handle)
  connection = mock(list(handle = httr_handle, headers = mock()))
  expected_path = "C:/test/test.tar.gz"
  
  expect_message(
    with_mock(
      .install_package(path = expected_path),
      "MolgenisArmadillo:::.get_armadillo_connection" = connection,
      "httr:::upload_file" = mock(),
      "httr:::POST" = httr_post
    ),
    cat(paste0("Attempting to install package [ ' ", expected_path, " ' ]", "\n","\n", "Package [ ", expected_path," ] installed"))
  )
  
})

